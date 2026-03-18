ui <- bslib::page_sidebar(
  title = "AlgAware",
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),

  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "gallery.js")
  ),

  # Sidebar
  sidebar = bslib::sidebar(
    width = 350,

    # Settings button
    mod_settings_ui("settings"),

    hr(),

    # Data loader
    mod_data_loader_ui("data_loader"),

    hr(),

    # Validation controls
    conditionalPanel(
      condition = "output.data_loaded",
      mod_validation_ui("validation")
    ),

    # Report generation
    conditionalPanel(
      condition = "output.data_loaded",
      mod_report_ui("report")
    )
  ),

  # Main panel with tabs
  bslib::navset_card_tab(
    id = "main_tabs",

    bslib::nav_panel(
      "Validate",
      icon = shiny::icon("images"),
      mod_gallery_ui("gallery")
    ),

    bslib::nav_panel(
      "Maps",
      icon = shiny::icon("map"),
      conditionalPanel(
        condition = "output.data_loaded",
        plotOutput("biomass_map", height = "400px"),
        plotOutput("chl_map", height = "400px")
      )
    ),

    bslib::nav_panel(
      "Plots",
      icon = shiny::icon("chart-bar"),
      conditionalPanel(
        condition = "output.data_loaded",
        h4("Baltic Sea"),
        plotOutput("baltic_heatmap", height = "500px"),
        plotOutput("baltic_stacked_bar", height = "400px"),
        hr(),
        h4("West Coast"),
        plotOutput("westcoast_heatmap", height = "500px"),
        plotOutput("westcoast_stacked_bar", height = "400px")
      )
    ),

    bslib::nav_panel(
      "Summary",
      icon = shiny::icon("table"),
      conditionalPanel(
        condition = "output.data_loaded",
        DT::DTOutput("summary_table")
      )
    )
  )
)
