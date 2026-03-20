test_that("get_hab_species returns empty vector for NULL input", {
  result <- algaware:::get_hab_species(NULL)
  expect_equal(result, character(0))
})

test_that("get_hab_species returns empty vector when no HAB column", {
  taxa <- data.frame(
    name = c("Species A", "Species B"),
    stringsAsFactors = FALSE
  )
  result <- algaware:::get_hab_species(taxa)
  expect_equal(result, character(0))
})

test_that("get_hab_species extracts HAB species", {
  taxa <- data.frame(
    name = c("Species A", "Species B", "Species C"),
    HAB = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::get_hab_species(taxa)
  expect_setequal(result, c("Species A", "Species C"))
})

test_that("create_heatmap returns ggplot", {
  wide <- data.frame(
    scientific_name = c("Taxon A", "Taxon B"),
    `STN1_2022-01-01` = c(10, 20),
    `STN2_2022-01-02` = c(30, 40),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_heatmap(wide, title = "Test")
  expect_s3_class(p, "ggplot")
})

test_that("create_heatmap annotates HAB species", {
  wide <- data.frame(
    scientific_name = c("HAB Taxon", "Normal Taxon"),
    `STN1_2022-01-01` = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  taxa <- data.frame(
    name = c("HAB Taxon", "Normal Taxon"),
    HAB = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  p <- create_heatmap(wide, taxa_lookup = taxa, title = "HAB Test")
  expect_s3_class(p, "ggplot")
  # Check caption mentions HAB
  expect_true(any(grepl("HAB", p$labels$caption)))
})

test_that("create_stacked_bar returns ggplot", {
  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:12),
    `STN1_2022-01-01` = runif(12, 1, 100),
    `STN2_2022-01-02` = runif(12, 1, 100),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_stacked_bar(wide, n_top = 5, title = "Test")
  expect_s3_class(p, "ggplot")
})

test_that("create_stacked_bar groups Other correctly", {
  wide <- data.frame(
    scientific_name = paste0("Taxon_", 1:15),
    `STN1_2022-01-01` = seq(15, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  p <- create_stacked_bar(wide, n_top = 3, title = "Test")
  expect_s3_class(p, "ggplot")
})
