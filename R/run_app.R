#' Launch the AlgAware Shiny Application
#'
#' Start the interactive AlgAware application for IFCB phytoplankton
#' data processing, validation, and report generation.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#' @return A Shiny app object (invisibly).
#' @export
#' @examples
#' \dontrun{
#'   algaware::launch_app()
#' }
launch_app <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "algaware")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `algaware`.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
