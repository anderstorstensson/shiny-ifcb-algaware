#' Load the bundled taxa lookup table
#'
#' Returns the pre-built mapping from classifier class names to WoRMS
#' scientific names and AphiaIDs.
#'
#' @return A data.frame with columns \code{clean_names}, \code{name},
#'   \code{AphiaID}.
#' @export
load_taxa_lookup <- function() {
  lookup_file <- system.file("extdata", "taxa_lookup.csv",
                             package = "algaware")
  utils::read.csv(lookup_file, stringsAsFactors = FALSE)
}
