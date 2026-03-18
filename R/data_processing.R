#' Summarize biovolumes for classified bins
#'
#' Wraps \code{iRfcb::ifcb_summarize_biovolumes()} and joins with taxonomy.
#'
#' @param feature_folder Path to the feature CSV directory.
#' @param hdr_folder Path to the raw data directory (for .hdr files).
#' @param classifications A data.frame from \code{read_h5_classifications()}.
#' @param taxa_lookup A data.frame with columns \code{clean_names},
#'   \code{name}, \code{AphiaID}.
#' @param non_bio_classes Character vector of non-biological class names to
#'   exclude.
#' @return A data.frame with per-sample, per-class biovolume data joined with
#'   taxonomy.
#' @export
summarize_biovolumes <- function(feature_folder, hdr_folder, classifications,
                                 taxa_lookup, non_bio_classes = character(0)) {
  # Build image name (sample_NNNNN format)
  image_names <- paste0(classifications$sample_name, "_",
                        sprintf("%05d", classifications$roi_number))

  # Identify diatom classes from taxa lookup
  diatom_classes <- identify_diatom_classes(taxa_lookup)

  biovolume_data <- iRfcb::ifcb_summarize_biovolumes(
    feature_folder = feature_folder,
    hdr_folder = hdr_folder,
    custom_images = image_names,
    custom_classes = classifications$class_name,
    diatom_include = diatom_classes
  )

  # Join with taxonomy
  biovolume_data <- merge(
    biovolume_data,
    taxa_lookup[, c("clean_names", "name", "AphiaID")],
    by.x = "class",
    by.y = "clean_names",
    all.x = TRUE
  )

  # Remove non-biological classes
  if (length(non_bio_classes) > 0) {
    biovolume_data <- biovolume_data[!biovolume_data$class %in% non_bio_classes, ]
  }

  biovolume_data
}

#' Identify diatom classes from taxa lookup
#'
#' @param taxa_lookup A data.frame with \code{clean_names} column.
#' @return Character vector of class names likely to be diatoms.
#' @keywords internal
identify_diatom_classes <- function(taxa_lookup) {
  # Known diatom-related patterns
  diatom_patterns <- c(
    "Navicula", "Actinocyclus", "Achnanthes", "Proboscia", "rhizosolenia",
    "Chaetocero", "centrales", "Centrales", "Coscinodiscus", "Thalassiosira",
    "Skeletonema", "Pseudo-nitzschia", "Nitzschia", "Ditylum", "Guinardia",
    "Dactyliosolen", "Lauderia", "Leptocylindrus", "Eucampia", "Corethron",
    "Melosira", "Paralia", "Fragilaria", "Asterionella", "Cerataulina",
    "Pauliella", "Pennales", "Odontella", "Porosira", "Stellarima",
    "Sundstroemia", "Thalassionema", "Trieres", "Diatoma", "Licmophora",
    "Striatella", "Cocconeis", "Cylindrotheca"
  )

  pattern <- paste(diatom_patterns, collapse = "|")
  taxa_lookup$clean_names[grepl(pattern, taxa_lookup$clean_names)]
}

#' Aggregate biovolume data per station visit
#'
#' Aggregates sample volumes, computes per-liter concentrations, and assigns
#' presence categories.
#'
#' @param biovolume_data Output from \code{summarize_biovolumes()}.
#' @param metadata Station-matched metadata with \code{STATION_NAME},
#'   \code{COAST}, \code{STATION_NAME_SHORT}, \code{sample_time}, and
#'   \code{ml_analyzed} columns.
#' @return A data.frame with per-station, per-taxon summary including
#'   \code{counts_per_liter}, \code{biovolume_mm3_per_liter},
#'   \code{carbon_ug_per_liter}, and \code{Presence_cat}.
#' @export
aggregate_station_data <- function(biovolume_data, metadata) {
  # Join biovolume with metadata
  all_data <- merge(
    biovolume_data,
    metadata[, c("pid", "STATION_NAME", "STATION_NAME_SHORT", "COAST",
                 "sample_time", "ml_analyzed")],
    by.x = "sample",
    by.y = "pid",
    all.x = TRUE
  )

  all_data$sample_date <- as.Date(all_data$sample_time)

  # Assign station visits (handles midnight crossings)
  all_data <- assign_station_visits(all_data)

  # For each visit, determine the representative date (most common)
  visit_dates <- stats::aggregate(
    sample_date ~ visit_id + STATION_NAME,
    data = all_data,
    FUN = function(x) {
      tab <- table(x)
      as.Date(names(tab)[which.max(tab)])
    }
  )
  names(visit_dates)[3] <- "visit_date"

  all_data <- merge(all_data, visit_dates, by = c("visit_id", "STATION_NAME"),
                    all.x = TRUE)

  # Aggregate sample volume per visit
  sample_meta <- unique(all_data[, c("sample", "visit_id", "STATION_NAME",
                                     "ml_analyzed", "sample_time")])
  sample_volume <- stats::aggregate(
    cbind(total_ml_analyzed = ml_analyzed) ~ visit_id + STATION_NAME,
    data = sample_meta,
    FUN = sum,
    na.rm = TRUE
  )
  median_time <- stats::aggregate(
    sample_time ~ visit_id + STATION_NAME,
    data = sample_meta,
    FUN = stats::median
  )
  names(median_time)[3] <- "median_time"
  sample_volume <- merge(sample_volume, median_time,
                         by = c("visit_id", "STATION_NAME"))

  # Aggregate biovolume per station-visit-taxon
  agg <- stats::aggregate(
    cbind(total_counts = counts,
          total_biovolume_mm3 = biovolume_mm3,
          total_carbon_ug = carbon_ug) ~
      visit_id + STATION_NAME + STATION_NAME_SHORT + COAST + visit_date +
      name + AphiaID,
    data = all_data,
    FUN = sum,
    na.rm = TRUE
  )

  agg <- merge(agg, sample_volume, by = c("visit_id", "STATION_NAME"),
               all.x = TRUE)

  # Compute per-liter concentrations
  agg$counts_per_liter <- agg$total_counts / (agg$total_ml_analyzed / 1000)
  agg$biovolume_mm3_per_liter <- agg$total_biovolume_mm3 / (agg$total_ml_analyzed / 1000)
  agg$carbon_ug_per_liter <- agg$total_carbon_ug / (agg$total_ml_analyzed / 1000)

  # Join station coordinates
  station_list <- SHARK4R:::load_station_bundle(verbose = FALSE)
  station_coords <- station_list[, c("STATION_NAME",
                                     "LATITUDE_WGS84_SWEREF99_DD",
                                     "LONGITUDE_WGS84_SWEREF99_DD")]
  station_coords <- unique(station_coords)
  agg <- merge(agg, station_coords, by = "STATION_NAME", all.x = TRUE)

  # Compute presence categories
  station_totals <- stats::aggregate(
    counts_per_liter ~ visit_id + STATION_NAME,
    data = agg,
    FUN = sum,
    na.rm = TRUE
  )
  names(station_totals)[3] <- "sample_counts"
  agg <- merge(agg, station_totals, by = c("visit_id", "STATION_NAME"),
               all.x = TRUE)

  agg$pct <- ifelse(agg$sample_counts > 0,
                    agg$counts_per_liter / agg$sample_counts, 0)

  agg$Presence_cat <- ifelse(
    agg$pct >= 0.50, 5L,
    ifelse(agg$pct >= 0.10, 4L,
    ifelse(agg$pct >= 0.01, 3L,
    ifelse(agg$pct >= 0.001, 2L,
    ifelse(agg$pct > 0, 1L, 0L)))))

  agg
}

#' Collect ferrybox data for station visits
#'
#' @param sample_times POSIXct vector of sample timestamps.
#' @param ferrybox_path Path to ferrybox data folder.
#' @return A data.frame with chlorophyll and other measurements per timestamp.
#' @export
collect_ferrybox_data <- function(sample_times, ferrybox_path) {
  if (!nzchar(ferrybox_path) || !dir.exists(ferrybox_path)) {
    return(data.frame(
      timestamp = sample_times[0],
      chl = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  ferrybox_parameters <- c("70", "80070", "8063", "88063", "8165", "88165",
                           "8173", "88173", "8166", "88166", "8172", "88172",
                           "8174", "88174", "8177", "88177", "8179", "88179",
                           "8181", "88181", "8190", "88190", "8191", "88191")

  fb_data <- iRfcb::ifcb_get_ferrybox_data(
    unique(sample_times),
    ferrybox_folder = ferrybox_path,
    parameters = ferrybox_parameters
  )

  # Apply QC flags and rename
  if (nrow(fb_data) > 0 && "8063" %in% names(fb_data)) {
    fb_data$chl <- ifelse(
      !is.na(fb_data[["88063"]]) & fb_data[["88063"]] == 1,
      fb_data[["8063"]], NA_real_
    )
  } else {
    fb_data$chl <- NA_real_
  }

  fb_data
}

#' Create wide-format summary for a region
#'
#' @param station_summary Aggregated station data from
#'   \code{aggregate_station_data()}.
#' @param coast "EAST" for Baltic Sea or "WEST" for West Coast.
#' @return A wide-format data.frame with scientific names as rows and
#'   station-date as columns.
#' @export
create_wide_summary <- function(station_summary, coast) {
  region_data <- station_summary[station_summary$COAST == coast, ]

  if (nrow(region_data) == 0) {
    return(data.frame(scientific_name = character(0)))
  }

  region_data$station_date <- paste(region_data$STATION_NAME_SHORT,
                                    region_data$visit_date, sep = "_")

  wide <- tidyr::pivot_wider(
    region_data[, c("name", "station_date", "biovolume_mm3_per_liter")],
    names_from = "station_date",
    values_from = "biovolume_mm3_per_liter"
  )
  names(wide)[1] <- "scientific_name"

  # Order columns by date then station
  data_cols <- names(wide)[-1]
  if (length(data_cols) > 0) {
    parts <- data.frame(
      col = data_cols,
      date = as.Date(sub(".*_", "", data_cols)),
      station = sub("_[^_]+$", "", data_cols),
      stringsAsFactors = FALSE
    )
    parts <- parts[order(parts$date, parts$station), ]
    wide <- wide[, c("scientific_name", parts$col)]
  }

  wide
}

#' Apply class invalidation
#'
#' Replaces invalidated class names with "unclassified" in the classification
#' data.
#'
#' @param classifications A data.frame with a \code{class_name} column.
#' @param invalidated_classes Character vector of class names to invalidate.
#' @return A new data.frame with invalidated classes set to "unclassified".
#' @keywords internal
apply_invalidation <- function(classifications, invalidated_classes) {
  result <- classifications
  result$class_name <- ifelse(
    result$class_name %in% invalidated_classes,
    "unclassified",
    result$class_name
  )
  result
}
