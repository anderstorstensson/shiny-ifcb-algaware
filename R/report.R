#' Generate the AlgAware Word report
#'
#' Creates a Word document with biomass maps, heatmaps, stacked bar charts,
#' station sections, and image mosaics.
#'
#' @param output_path Path for the output .docx file.
#' @param station_summary Aggregated station data.
#' @param baltic_wide Wide-format Baltic summary.
#' @param westcoast_wide Wide-format West Coast summary.
#' @param baltic_mosaics Named list of mosaic images (Baltic).
#' @param westcoast_mosaics Named list of mosaic images (West Coast).
#' @param taxa_lookup Optional taxa lookup table with \code{HAB} column.
#' @param cruise_info Character string with cruise/date information for title.
#' @return Invisible path to the created document.
#' @export
generate_report <- function(output_path, station_summary,
                            baltic_wide, westcoast_wide,
                            baltic_mosaics = list(),
                            westcoast_mosaics = list(),
                            taxa_lookup = NULL,
                            cruise_info = "") {
  template <- system.file("templates", "report_template.docx",
                          package = "algaware")

  doc <- officer::read_docx(template)

  # Title
  doc <- officer::body_add_par(doc, "AlgAware Report", style = "heading 1")
  doc <- officer::body_add_par(doc, cruise_info, style = "Normal")
  doc <- officer::body_add_par(doc, "")

  # Swedish summary
  doc <- officer::body_add_par(doc, "Sammanfattning", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    "[Skriv sammanfattning pa svenska har.]",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")

  # English summary
  doc <- officer::body_add_par(doc, "Summary", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    "[Write English summary here.]",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")

  # Biomass maps
  doc <- officer::body_add_par(doc, "Biomass and Chlorophyll", style = "heading 2")
  maps <- create_biomass_maps(station_summary)

  map_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(map_file, maps$biomass_map, width = 7, height = 5, dpi = 300)
  doc <- officer::body_add_img(doc, map_file, width = 6, height = 4.3)
  doc <- officer::body_add_par(
    doc,
    "Figure 1. Total carbon biomass at AlgAware stations.",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")

  chl_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(chl_file, maps$chl_map, width = 7, height = 5, dpi = 300)
  doc <- officer::body_add_img(doc, chl_file, width = 6, height = 4.3)
  doc <- officer::body_add_par(
    doc,
    "Figure 2. Chlorophyll fluorescence at AlgAware stations.",
    style = "Normal"
  )
  doc <- officer::body_add_par(doc, "")

  # Heatmaps
  fig_num <- 3L

  if (nrow(baltic_wide) > 0 && ncol(baltic_wide) > 1) {
    doc <- officer::body_add_par(doc, "Baltic Sea - Biovolume Heatmap",
                                 style = "heading 2")
    hm <- create_heatmap(baltic_wide, taxa_lookup = taxa_lookup, title = "Baltic Sea")
    hm_file <- tempfile(fileext = ".png")
    hm_height <- max(4, min(12, nrow(baltic_wide) * 0.25 + 2))
    ggplot2::ggsave(hm_file, hm, width = 8, height = hm_height, dpi = 300)
    doc <- officer::body_add_img(doc, hm_file, width = 6,
                                 height = hm_height * 6 / 8)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Biovolume heatmap for Baltic Sea stations."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  if (nrow(westcoast_wide) > 0 && ncol(westcoast_wide) > 1) {
    doc <- officer::body_add_par(doc, "West Coast - Biovolume Heatmap",
                                 style = "heading 2")
    hm <- create_heatmap(westcoast_wide, taxa_lookup = taxa_lookup, title = "West Coast")
    hm_file <- tempfile(fileext = ".png")
    hm_height <- max(4, min(12, nrow(westcoast_wide) * 0.25 + 2))
    ggplot2::ggsave(hm_file, hm, width = 8, height = hm_height, dpi = 300)
    doc <- officer::body_add_img(doc, hm_file, width = 6,
                                 height = hm_height * 6 / 8)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Biovolume heatmap for West Coast stations."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  # Stacked bar charts
  if (nrow(baltic_wide) > 0 && ncol(baltic_wide) > 1) {
    doc <- officer::body_add_par(doc, "Baltic Sea - Relative Biovolume",
                                 style = "heading 2")
    sb <- create_stacked_bar(baltic_wide, taxa_lookup = taxa_lookup, title = "Baltic Sea")
    sb_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(sb_file, sb, width = 8, height = 5, dpi = 300)
    doc <- officer::body_add_img(doc, sb_file, width = 6, height = 3.75)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Relative biovolume of top 10 taxa at Baltic Sea stations."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  if (nrow(westcoast_wide) > 0 && ncol(westcoast_wide) > 1) {
    doc <- officer::body_add_par(doc, "West Coast - Relative Biovolume",
                                 style = "heading 2")
    sb <- create_stacked_bar(westcoast_wide, taxa_lookup = taxa_lookup, title = "West Coast")
    sb_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(sb_file, sb, width = 8, height = 5, dpi = 300)
    doc <- officer::body_add_img(doc, sb_file, width = 6, height = 3.75)
    doc <- officer::body_add_par(
      doc,
      paste0("Figure ", fig_num,
             ". Relative biovolume of top 10 taxa at West Coast stations."),
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
    fig_num <- fig_num + 1L
  }

  # Station sections
  doc <- officer::body_add_par(doc, "Station Reports", style = "heading 2")

  visits <- unique(station_summary[, c("STATION_NAME", "STATION_NAME_SHORT",
                                       "COAST", "visit_date")])
  visits <- visits[order(visits$COAST, visits$visit_date,
                         visits$STATION_NAME), ]

  current_region <- ""
  for (i in seq_len(nrow(visits))) {
    region <- ifelse(visits$COAST[i] == "EAST", "Baltic Sea", "West Coast")
    if (region != current_region) {
      doc <- officer::body_add_par(doc, region, style = "heading 3")
      current_region <- region
    }

    station_header <- paste0(visits$STATION_NAME_SHORT[i], " - ",
                             visits$visit_date[i])
    doc <- officer::body_add_par(doc, station_header, style = "heading 4")
    doc <- officer::body_add_par(
      doc,
      "[Write station description here.]",
      style = "Normal"
    )
    doc <- officer::body_add_par(doc, "")
  }

  # Image mosaics
  doc <- officer::body_add_par(doc, "Image Mosaics", style = "heading 2")
  hab_species <- get_hab_species(taxa_lookup)

  if (length(baltic_mosaics) > 0) {
    doc <- officer::body_add_par(doc, "Baltic Sea", style = "heading 3")
    mosaic_num <- 1L
    for (taxon in names(baltic_mosaics)) {
      mosaic_file <- tempfile(fileext = ".png")
      magick::image_write(baltic_mosaics[[taxon]], mosaic_file)
      info <- magick::image_info(baltic_mosaics[[taxon]])
      # Scale to fit page width (6 inches max)
      display_width <- min(6, info$width / 300)
      display_height <- display_width * info$height / info$width
      hab_note <- if (taxon %in% hab_species) " *" else ""
      doc <- officer::body_add_par(doc, paste0(taxon, hab_note),
                                   style = "heading 4")
      doc <- officer::body_add_img(doc, mosaic_file,
                                   width = display_width,
                                   height = display_height)
      caption <- paste0("Mosaic ", mosaic_num, ". Example images of ", taxon,
                        " from Baltic Sea stations.")
      if (taxon %in% hab_species) {
        caption <- paste0(caption, " * HAB species.")
      }
      doc <- officer::body_add_par(doc, caption, style = "Normal")
      doc <- officer::body_add_par(doc, "")
      mosaic_num <- mosaic_num + 1L
    }
  }

  if (length(westcoast_mosaics) > 0) {
    doc <- officer::body_add_par(doc, "West Coast", style = "heading 3")
    mosaic_num <- 1L
    for (taxon in names(westcoast_mosaics)) {
      mosaic_file <- tempfile(fileext = ".png")
      magick::image_write(westcoast_mosaics[[taxon]], mosaic_file)
      info <- magick::image_info(westcoast_mosaics[[taxon]])
      display_width <- min(6, info$width / 300)
      display_height <- display_width * info$height / info$width
      hab_note <- if (taxon %in% hab_species) " *" else ""
      doc <- officer::body_add_par(doc, paste0(taxon, hab_note),
                                   style = "heading 4")
      doc <- officer::body_add_img(doc, mosaic_file,
                                   width = display_width,
                                   height = display_height)
      caption <- paste0("Mosaic ", mosaic_num, ". Example images of ", taxon,
                        " from West Coast stations.")
      if (taxon %in% hab_species) {
        caption <- paste0(caption, " * HAB species.")
      }
      doc <- officer::body_add_par(doc, caption, style = "Normal"
      )
      doc <- officer::body_add_par(doc, "")
      mosaic_num <- mosaic_num + 1L
    }
  }

  print(doc, target = output_path)
  invisible(output_path)
}
