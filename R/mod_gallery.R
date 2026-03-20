#' Gallery Module UI
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_gallery_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Toolbar
    shiny::div(
      class = "d-flex align-items-start gap-2 mb-3 p-2 bg-light rounded toolbar-row",
      style = "min-height: 110px;",

      # Region toggle
      shiny::div(
        style = "margin-top: 6px;",
        shiny::radioButtons(ns("region_toggle"), NULL,
                            choices = c("Baltic Sea" = "EAST",
                                        "West Coast" = "WEST"),
                            selected = "EAST", inline = FALSE)
      ),

      shiny::div(class = "vr mx-2", style = "align-self: stretch;"),

      # Class navigation
      shiny::actionButton(ns("prev_class"), "",
                          icon = shiny::icon("arrow-left"),
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px; position: relative; z-index: 10;"),
      shiny::div(
        style = "width: 390px; min-width: 390px; max-width: 390px; text-align: center; height: 95px; overflow: visible; position: relative;",
        shiny::selectizeInput(ns("class_select"), NULL, choices = NULL,
                              width = "100%",
                              options = list(
                                placeholder = "Select class...",
                                maxOptions = 500,
                                dropdownParent = "body"
                              )),
        shiny::uiOutput(ns("class_header"))
      ),
      shiny::actionButton(ns("next_class"), "",
                          icon = shiny::icon("arrow-right"),
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px; position: relative; z-index: 10;"),

      shiny::div(class = "vr mx-2", style = "align-self: stretch;"),

      # Selection buttons
      shiny::actionButton(ns("select_page"), "Select Page",
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px;"),
      shiny::actionButton(ns("deselect"), "Deselect",
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px;"),

      # Measure tool
      shiny::actionButton(ns("measure_toggle"), label = shiny::icon("ruler"),
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px;",
                          title = "Measure: click and drag on images"),

      shiny::div(class = "vr mx-2", style = "align-self: stretch;"),

      # Page size
      shiny::selectInput(ns("page_size"), NULL,
                         choices = c("50", "100", "200"),
                         selected = "100", width = "80px"),

      # Pagination
      shiny::actionButton(ns("prev_page"), "",
                          icon = shiny::icon("chevron-left"),
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px;"),
      shiny::uiOutput(ns("page_info"), inline = TRUE),
      shiny::actionButton(ns("next_page"), "",
                          icon = shiny::icon("chevron-right"),
                          class = "btn-outline-secondary btn-sm",
                          style = "margin-top: 6px;")
    ),

    # Gallery area
    shiny::div(
      id = ns("gallery_drag_area"),
      class = "gallery-drag-area",
      shiny::uiOutput(ns("image_gallery"))
    ),

    # Selection box overlay
    shiny::div(id = ns("selection_box"), class = "selection-box")
  )
}

#' Gallery Module Server
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values with settings.
#' @return NULL (side effects only).
#' @export
mod_gallery_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page <- shiny::reactiveVal(1L)

    # Current region's class list
    region_classes <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$classifications)
      region <- input$region_toggle

      region_samples <- if (region == "EAST") rv$baltic_samples else rv$westcoast_samples
      region_data <- rv$classifications[
        rv$classifications$sample_name %in% region_samples &
        rv$classifications$class_name != "unclassified",
      ]
      sort(unique(region_data$class_name))
    })

    # Current class images
    current_images <- shiny::reactive({
      classes <- region_classes()
      shiny::req(length(classes) > 0, rv$current_class_idx)

      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]
      region_samples <- if (input$region_toggle == "EAST") {
        rv$baltic_samples
      } else {
        rv$westcoast_samples
      }

      rv$classifications[
        rv$classifications$class_name == current_class &
        rv$classifications$sample_name %in% region_samples,
      ]
    })

    # Paginated images
    paginated <- shiny::reactive({
      imgs <- current_images()
      shiny::req(nrow(imgs) > 0)
      ps <- as.integer(input$page_size)
      p <- page()
      total_pages <- ceiling(nrow(imgs) / ps)
      p <- min(p, total_pages)
      start <- (p - 1) * ps + 1
      end <- min(p * ps, nrow(imgs))
      imgs[start:end, ]
    })

    # Flag to suppress selectize observer during programmatic updates
    updating_select <- shiny::reactiveVal(FALSE)

    # Update selectize when classes or current index change
    shiny::observe({
      classes <- region_classes()
      # Explicit dependency on current_class_idx
      current_idx <- rv$current_class_idx
      idx <- min(current_idx, max(1L, length(classes)))
      selected <- if (length(classes) > 0) classes[idx] else NULL
      updating_select(TRUE)
      shiny::updateSelectizeInput(session, "class_select",
                                  choices = classes,
                                  selected = selected)
    })

    # Jump to class when user picks from selectize
    shiny::observeEvent(input$class_select, {
      if (updating_select()) {
        updating_select(FALSE)
        return()
      }
      classes <- region_classes()
      if (length(classes) == 0) return()
      new_idx <- match(input$class_select, classes)
      if (!is.na(new_idx) && new_idx != rv$current_class_idx) {
        rv$current_class_idx <- new_idx
        page(1L)
      }
    })

    # Class header (detail info below selectize)
    output$class_header <- shiny::renderUI({
      classes <- region_classes()
      if (length(classes) == 0) return(shiny::p("No classes"))

      idx <- min(rv$current_class_idx, length(classes))
      current_class <- classes[idx]
      total_imgs <- nrow(current_images())

      # Check if non-biological
      non_bio <- parse_non_bio_classes(config$non_biological_classes)
      is_non_bio <- current_class %in% non_bio

      # Map class_name to scientific name
      taxa <- rv$taxa_lookup
      sci_name <- current_class
      if (!is.null(taxa)) {
        match_idx <- match(current_class, taxa$clean_names)
        if (!is.na(match_idx)) sci_name <- taxa$name[match_idx]
      }

      # Check HAB status
      hab_species <- get_hab_species(rv$taxa_lookup)
      is_hab <- !is_non_bio && sci_name %in% hab_species

      # Determine display name (scientific name for biological classes)
      name_display <- NULL
      if (!is_non_bio) {
        show_sci <- !is.null(taxa) && !is.na(match(current_class, taxa$clean_names))
        if (show_sci) {
          name_display <- shiny::span(
            style = "font-size: 11px; color: #17a2b8; font-style: italic;",
            sci_name
          )
        }
      }

      shiny::div(
        if (is_hab) {
          shiny::span(
            style = paste0("background: #dc3545; color: white; ",
                           "padding: 1px 6px; border-radius: 3px; ",
                           "font-size: 10px; margin-right: 4px; ",
                           "vertical-align: middle;"),
            "HAB"
          )
        },
        if (is_non_bio) {
          shiny::span(
            style = paste0("background: #6c757d; color: white; ",
                           "padding: 1px 6px; border-radius: 3px; ",
                           "font-size: 10px; margin-right: 4px; ",
                           "vertical-align: middle;"),
            "Non-biological"
          )
        },
        if (!is.null(name_display)) name_display,
        shiny::br(),
        shiny::span(
          style = "font-size: 11px; color: #666;",
          paste0("Class ", idx, " of ", length(classes),
                 " (", total_imgs, " images)")
        )
      )
    })

    # Page info
    output$page_info <- shiny::renderUI({
      imgs <- current_images()
      ps <- as.integer(input$page_size)
      total_pages <- max(1, ceiling(nrow(imgs) / ps))
      shiny::span(
        style = "font-size: 12px; min-width: 60px; text-align: center;",
        paste0(page(), "/", total_pages)
      )
    })

    # Navigation
    shiny::observeEvent(input$prev_class, {
      if (rv$current_class_idx > 1) {
        rv$current_class_idx <- rv$current_class_idx - 1L
        page(1L)
      }
    })

    shiny::observeEvent(input$next_class, {
      classes <- region_classes()
      if (rv$current_class_idx < length(classes)) {
        rv$current_class_idx <- rv$current_class_idx + 1L
        page(1L)
      }
    })

    shiny::observeEvent(input$region_toggle, {
      rv$current_class_idx <- 1L
      rv$current_region <- input$region_toggle
      page(1L)
    })

    shiny::observeEvent(input$prev_page, {
      if (page() > 1) page(page() - 1L)
    })

    shiny::observeEvent(input$next_page, {
      imgs <- current_images()
      ps <- as.integer(input$page_size)
      total_pages <- ceiling(nrow(imgs) / ps)
      if (page() < total_pages) page(page() + 1L)
    })

    # Toggle image selection
    shiny::observeEvent(input$toggle_image, {
      img_id <- input$toggle_image$img
      if (img_id %in% rv$selected_images) {
        rv$selected_images <- setdiff(rv$selected_images, img_id)
      } else {
        rv$selected_images <- c(rv$selected_images, img_id)
      }
    })

    # Drag select
    shiny::observeEvent(input$drag_select, {
      new_imgs <- input$drag_select$images
      rv$selected_images <- union(rv$selected_images, new_imgs)
    })

    # Select current page
    shiny::observeEvent(input$select_page, {
      imgs <- tryCatch(paginated(), error = function(e) NULL)
      if (is.null(imgs) || nrow(imgs) == 0) return()
      img_ids <- paste0(imgs$sample_name, "_", imgs$roi_number)
      rv$selected_images <- union(rv$selected_images, img_ids)
    })

    # Deselect all
    shiny::observeEvent(input$deselect, {
      rv$selected_images <- character(0)
    })

    # Measure tool
    measure_active <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$measure_toggle, {
      active <- !measure_active()
      measure_active(active)
      session$sendCustomMessage("measureMode", active)
      session$sendCustomMessage("toggleMeasureBtn",
        list(id = ns("measure_toggle"), active = active))
    })

    # Send pixels_per_micron on session start
    shiny::observe({
      session$sendCustomMessage("updatePixelsPerMicron",
                                config$pixels_per_micron)
    })

    # Render gallery
    output$image_gallery <- shiny::renderUI({
      imgs <- tryCatch(paginated(), error = function(e) NULL)
      if (is.null(imgs) || nrow(imgs) == 0) {
        return(shiny::div(
          class = "text-center text-muted p-5",
          shiny::h4("No images to display"),
          shiny::p("Load data or select a different class/region.")
        ))
      }

      # Extract PNGs for current page
      storage <- config$local_storage_path
      raw_dir <- file.path(storage, "raw")
      png_dir <- file.path(tempdir(), "algaware_gallery",
                           session$token)

      for (samp in unique(imgs$sample_name)) {
        samp_dir <- file.path(png_dir, samp)
        samp_rois <- imgs$roi_number[imgs$sample_name == samp]

        # Check which ROIs still need extraction
        expected_pngs <- paste0(samp, "_",
                                sprintf("%05d", samp_rois), ".png")
        existing_pngs <- if (dir.exists(samp_dir)) {
          list.files(samp_dir)
        } else {
          character(0)
        }
        missing_rois <- samp_rois[!expected_pngs %in% existing_pngs]

        if (length(missing_rois) == 0) next

        roi_file <- list.files(raw_dir,
                               pattern = paste0(samp, "\\.roi$"),
                               recursive = TRUE, full.names = TRUE)
        if (length(roi_file) > 0) {
          tryCatch(
            iRfcb::ifcb_extract_pngs(roi_file[1], png_dir,
                                     ROInumbers = missing_rois,
                                     verbose = FALSE),
            error = function(e) NULL
          )
        }
      }

      # Register resource path
      resource_name <- paste0("gallery_", session$token)
      shiny::addResourcePath(resource_name, png_dir)

      # Build station name lookup from matched metadata
      station_lookup <- stats::setNames(
        rv$matched_metadata$STATION_NAME,
        rv$matched_metadata$pid
      )

      # Build image cards
      cards <- lapply(seq_len(nrow(imgs)), function(i) {
        row <- imgs[i, ]
        file_name <- paste0(row$sample_name, "_",
                            sprintf("%05d", row$roi_number), ".png")
        img_id <- paste0(row$sample_name, "_", row$roi_number)
        is_selected <- img_id %in% rv$selected_images
        src <- paste0(resource_name, "/", row$sample_name, "/", file_name)
        station <- unname(station_lookup[row$sample_name])
        if (is.na(station)) station <- ""

        border_style <- if (is_selected) {
          "border: 3px solid #007bff; background-color: #e7f1ff;"
        } else {
          "border: 1px solid #ddd;"
        }

        shiny::div(
          class = paste("image-card", if (is_selected) "selected"),
          `data-img` = img_id,
          style = paste0(
            "display: inline-block; margin: 4px; padding: 4px; ",
            "border-radius: 5px; cursor: pointer; ", border_style
          ),
          shiny::tags$img(
            src = src,
            style = "display: block; max-height: 120px;",
            onerror = paste0(
              "this.style.display='none';",
              "this.nextSibling.style.display='block';"
            )
          ),
          shiny::div(
            style = paste0(
              "width: 80px; height: 60px; background: #f0f0f0; ",
              "display: none; line-height: 60px; text-align: center; ",
              "font-size: 10px;"
            ),
            "Not found"
          ),
          shiny::div(
            style = "font-size: 9px; text-align: center; margin-top: 2px;",
            station,
            if (!is.null(row$score)) {
              shiny::span(
                style = "color: #999; margin-left: 4px;",
                paste0(round(row$score * 100, 1), "%")
              )
            }
          )
        )
      })

      shiny::div(
        style = "display: flex; flex-wrap: wrap; align-items: flex-start;",
        cards
      )
    })
  })
}
