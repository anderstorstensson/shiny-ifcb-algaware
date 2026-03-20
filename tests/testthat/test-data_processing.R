test_that("apply_invalidation replaces classes with unclassified", {
  classifications <- data.frame(
    sample_name = c("s1", "s1", "s1"),
    roi_number = c(1L, 2L, 3L),
    class_name = c("Skeletonema", "detritus", "Chaetoceros"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, c("detritus"))
  expect_equal(result$class_name, c("Skeletonema", "unclassified", "Chaetoceros"))
})

test_that("apply_invalidation does not modify original", {
  classifications <- data.frame(
    sample_name = "s1",
    roi_number = 1L,
    class_name = "detritus",
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, c("detritus"))
  expect_equal(classifications$class_name, "detritus")
  expect_equal(result$class_name, "unclassified")
})

test_that("apply_invalidation with empty invalidated list", {
  classifications <- data.frame(
    sample_name = c("s1", "s1"),
    roi_number = c(1L, 2L),
    class_name = c("A", "B"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::apply_invalidation(classifications, character(0))
  expect_equal(result$class_name, c("A", "B"))
})

test_that("identify_diatom_classes finds known diatoms", {
  taxa_lookup <- data.frame(
    clean_names = c("Skeletonema_marinoi", "Chaetoceros_sp",
                    "Dinophysis_sp", "Navicula_sp"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::identify_diatom_classes(taxa_lookup)
  expect_true("Skeletonema_marinoi" %in% result)
  expect_true("Chaetoceros_sp" %in% result)
  expect_true("Navicula_sp" %in% result)
  expect_false("Dinophysis_sp" %in% result)
})

test_that("create_wide_summary handles empty data", {
  station_summary <- data.frame(
    COAST = character(0),
    STATION_NAME_SHORT = character(0),
    visit_date = character(0),
    name = character(0),
    biovolume_mm3_per_liter = numeric(0),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "EAST")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("create_wide_summary pivots correctly", {
  station_summary <- data.frame(
    COAST = c("EAST", "EAST", "EAST", "EAST"),
    STATION_NAME_SHORT = c("BY5", "BY5", "BY31", "BY31"),
    visit_date = c("2022-01-01", "2022-01-01", "2022-01-02", "2022-01-02"),
    name = c("Taxon A", "Taxon B", "Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "EAST")
  expect_true("scientific_name" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_true(ncol(result) > 1)
})

test_that("create_wide_summary filters by coast", {
  station_summary <- data.frame(
    COAST = c("EAST", "WEST"),
    STATION_NAME_SHORT = c("BY5", "ANHOLT"),
    visit_date = c("2022-01-01", "2022-01-01"),
    name = c("Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- create_wide_summary(station_summary, "WEST")
  expect_equal(nrow(result), 1)
})

test_that("collect_ferrybox_data returns empty df for invalid path", {
  result <- collect_ferrybox_data(
    as.POSIXct(c("2022-01-01", "2022-01-02")),
    ""
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("collect_ferrybox_data returns empty df for missing dir", {
  result <- collect_ferrybox_data(
    as.POSIXct(c("2022-01-01", "2022-01-02")),
    "/nonexistent/ferrybox/path"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
