#' Fetch metadata from the IFCB Dashboard
#'
#' Wraps \code{iRfcb::ifcb_download_dashboard_metadata()} and extracts
#' available cruise numbers.
#'
#' @param dashboard_url Dashboard base URL.
#' @param dataset_name Dataset name (e.g. "RV_Svea").
#' @return A list with \code{metadata} (data.frame) and \code{cruise_numbers}
#'   (character vector, possibly empty if no cruise column exists).
#' @export
fetch_dashboard_metadata <- function(dashboard_url, dataset_name = NULL) {
  metadata <- iRfcb::ifcb_download_dashboard_metadata(
    dashboard_url,
    dataset_name = dataset_name,
    quiet = TRUE
  )

  cruise_numbers <- character(0)
  if ("cruise" %in% names(metadata)) {
    cruise_numbers <- unique(metadata$cruise)
    cruise_numbers <- cruise_numbers[!is.na(cruise_numbers) & nzchar(cruise_numbers)]
  }

  list(metadata = metadata, cruise_numbers = cruise_numbers)
}

#' Filter metadata by cruise number or date range
#'
#' @param metadata Dashboard metadata data.frame.
#' @param cruise Optional cruise number to filter on.
#' @param date_from Optional start date (Date or character yyyy-mm-dd).
#' @param date_to Optional end date.
#' @return Filtered metadata data.frame.
#' @export
filter_metadata <- function(metadata, cruise = NULL, date_from = NULL, date_to = NULL) {
  if (!is.null(cruise) && nzchar(cruise) && "cruise" %in% names(metadata)) {
    return(metadata[metadata$cruise == cruise, ])
  }

  if (!is.null(date_from) && !is.null(date_to)) {
    # Parse sample_time or timestamp column
    time_col <- if ("sample_time" %in% names(metadata)) "sample_time" else "timestamp"
    sample_dates <- as.Date(metadata[[time_col]])
    date_from <- as.Date(date_from)
    date_to <- as.Date(date_to)
    return(metadata[sample_dates >= date_from & sample_dates <= date_to, ])
  }

  metadata
}

#' Download raw IFCB files for selected bins
#'
#' Downloads .roi, .adc, and .hdr files to local storage. Skips files that
#' already exist.
#'
#' @param dashboard_url Dashboard base URL.
#' @param sample_ids Character vector of sample PIDs.
#' @param dest_dir Destination directory.
#' @param progress_callback Optional function(current, total, message) for
#'   progress updates.
#' @return Invisible NULL.
#' @export
download_raw_data <- function(dashboard_url, sample_ids, dest_dir,
                              progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Check which samples already exist
  existing <- tools::file_path_sans_ext(
    list.files(dest_dir, pattern = "\\.roi$", recursive = TRUE)
  )
  needed <- setdiff(sample_ids, existing)

  if (length(needed) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback(length(sample_ids), length(sample_ids),
                        "Raw data already downloaded")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress_callback)) {
    progress_callback(0, length(needed), "Downloading raw data...")
  }

  iRfcb::ifcb_download_dashboard_data(
    dashboard_url = dashboard_url,
    samples = needed,
    file_types = c("roi", "adc", "hdr"),
    dest_dir = dest_dir,
    quiet = TRUE
  )

  if (!is.null(progress_callback)) {
    progress_callback(length(needed), length(needed), "Raw data downloaded")
  }

  invisible(NULL)
}

#' Download feature files for selected bins
#'
#' @param dashboard_url Dashboard base URL (must include dataset path for
#'   features).
#' @param sample_ids Character vector of sample PIDs.
#' @param dest_dir Destination directory.
#' @param progress_callback Optional progress callback.
#' @return Invisible NULL.
#' @export
download_features <- function(dashboard_url, sample_ids, dest_dir,
                              progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  existing <- tools::file_path_sans_ext(
    list.files(dest_dir, pattern = "\\.csv$", recursive = TRUE)
  )
  needed <- setdiff(sample_ids, existing)

  if (length(needed) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback(length(sample_ids), length(sample_ids),
                        "Features already downloaded")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress_callback)) {
    progress_callback(0, length(needed), "Downloading features...")
  }

  iRfcb::ifcb_download_dashboard_data(
    dashboard_url = dashboard_url,
    samples = needed,
    file_types = "features",
    dest_dir = dest_dir,
    quiet = TRUE
  )

  if (!is.null(progress_callback)) {
    progress_callback(length(needed), length(needed), "Features downloaded")
  }

  invisible(NULL)
}

#' Copy classification H5 files for selected bins
#'
#' Copies matching \code{*_class.h5} files from the classification path to
#' local storage. Files are matched by sample name prefix.
#'
#' @param classification_path Source directory containing .h5 files.
#' @param sample_ids Character vector of sample PIDs.
#' @param dest_dir Destination directory.
#' @param progress_callback Optional progress callback.
#' @return Invisible character vector of copied file paths.
#' @export
copy_classification_files <- function(classification_path, sample_ids,
                                      dest_dir, progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  all_h5 <- list.files(classification_path, pattern = "_class.*\\.h5$",
                       full.names = TRUE, recursive = TRUE)

  # Match by sample name prefix
  h5_basenames <- basename(all_h5)
  h5_sample_names <- sub("_class.*\\.h5$", "", h5_basenames)

  matched_idx <- which(h5_sample_names %in% sample_ids)
  matched_files <- all_h5[matched_idx]

  # Skip already copied
  existing_h5 <- list.files(dest_dir, pattern = "\\.h5$", recursive = TRUE)
  existing_samples <- sub("_class.*\\.h5$", "", existing_h5)
  needed_idx <- which(!h5_sample_names[matched_idx] %in% existing_samples)
  to_copy <- matched_files[needed_idx]

  if (!is.null(progress_callback)) {
    progress_callback(
      length(matched_files) - length(to_copy),
      length(matched_files),
      paste0("Copying ", length(to_copy), " classification files...")
    )
  }

  copied <- character(0)
  for (f in to_copy) {
    dest <- file.path(dest_dir, basename(f))
    file.copy(f, dest, overwrite = FALSE)
    copied <- c(copied, dest)
  }

  if (!is.null(progress_callback)) {
    progress_callback(length(matched_files), length(matched_files),
                      "Classification files ready")
  }

  invisible(copied)
}

#' Read classifications from H5 files
#'
#' Reads thresholded class assignments from H5 classification files.
#'
#' @param h5_dir Directory containing .h5 files.
#' @param sample_ids Optional character vector of sample PIDs to read.
#'   If NULL, reads all .h5 files in the directory.
#' @return A data.frame with columns: sample_name, roi_number, class_name,
#'   score.
#' @export
read_h5_classifications <- function(h5_dir, sample_ids = NULL) {
  h5_files <- list.files(h5_dir, pattern = "_class.*\\.h5$",
                         full.names = TRUE, recursive = TRUE)

  if (!is.null(sample_ids)) {
    h5_samples <- sub("_class.*\\.h5$", "", basename(h5_files))
    h5_files <- h5_files[h5_samples %in% sample_ids]
  }

  if (length(h5_files) == 0) {
    return(data.frame(
      sample_name = character(0),
      roi_number = integer(0),
      class_name = character(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(h5_files, function(h5_path) {
    tryCatch({
      h5 <- hdf5r::H5File$new(h5_path, "r")
      on.exit(h5$close_all(), add = TRUE)

      roi_numbers <- h5[["roi_numbers"]]$read()
      class_names <- h5[["class_name"]]$read()
      output_scores <- h5[["output_scores"]]$read()
      scores <- apply(output_scores, 2, max)

      sample_name <- sub("_class.*\\.h5$", "", basename(h5_path))

      data.frame(
        sample_name = sample_name,
        roi_number = as.integer(roi_numbers),
        class_name = class_names,
        score = scores,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("Failed to read H5 file: ", basename(h5_path), " - ", e$message,
              call. = FALSE)
      NULL
    })
  })

  do.call(rbind, Filter(Negate(is.null), results))
}
