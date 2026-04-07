make_lims <- function(dates) {
  data.frame(
    sample_date = as.Date(dates),
    value       = seq_along(dates),
    stringsAsFactors = FALSE
  )
}

# ---- filter_lims_by_date_range ----

test_that("filter_lims_by_date_range keeps rows within range", {
  df <- make_lims(c("2024-06-01", "2024-06-05", "2024-06-10"))
  result <- filter_lims_by_date_range(df,
                                      as.Date("2024-06-04"),
                                      as.Date("2024-06-09"))
  expect_equal(nrow(result), 1L)
  expect_equal(result$sample_date, as.Date("2024-06-05"))
})

test_that("filter_lims_by_date_range includes boundary dates", {
  df <- make_lims(c("2024-06-01", "2024-06-05", "2024-06-10"))
  result <- filter_lims_by_date_range(df,
                                      as.Date("2024-06-01"),
                                      as.Date("2024-06-10"))
  expect_equal(nrow(result), 3L)
})

test_that("filter_lims_by_date_range returns empty df unchanged", {
  df <- make_lims(character(0))
  result <- filter_lims_by_date_range(df,
                                      as.Date("2024-06-01"),
                                      as.Date("2024-06-10"))
  expect_equal(nrow(result), 0L)
})

test_that("filter_lims_by_date_range returns NULL unchanged", {
  result <- filter_lims_by_date_range(NULL,
                                      as.Date("2024-06-01"),
                                      as.Date("2024-06-10"))
  expect_null(result)
})

test_that("filter_lims_by_date_range drops NA dates", {
  df <- data.frame(
    sample_date = as.Date(c("2024-06-05", NA)),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  result <- filter_lims_by_date_range(df,
                                      as.Date("2024-06-01"),
                                      as.Date("2024-06-10"))
  expect_equal(nrow(result), 1L)
})

# ---- extract_current_year_from_ctd ----

test_that("extract_current_year_from_ctd returns year of most recent date", {
  df <- data.frame(
    sample_date = as.Date(c("2023-05-01", "2024-06-15", "2022-12-31")),
    stringsAsFactors = FALSE
  )
  expect_equal(extract_current_year_from_ctd(df), 2024L)
})

test_that("extract_current_year_from_ctd ignores NA dates", {
  df <- data.frame(
    sample_date = as.Date(c("2023-05-01", NA, "2022-12-31")),
    stringsAsFactors = FALSE
  )
  expect_equal(extract_current_year_from_ctd(df), 2023L)
})

test_that("extract_current_year_from_ctd returns current year when all NA", {
  df <- data.frame(sample_date = as.Date(NA), stringsAsFactors = FALSE)
  expect_equal(extract_current_year_from_ctd(df),
               as.integer(format(Sys.Date(), "%Y")))
})

# ---- compute_region_plot_height ----

test_that("compute_region_plot_height returns minimum 300px for 1 station", {
  df <- data.frame(
    canonical_name = "StationA",
    region = "Bornholm",
    stringsAsFactors = FALSE
  )
  expect_equal(compute_region_plot_height(df, "Bornholm"), "300px")
})

test_that("compute_region_plot_height scales with station count", {
  df <- data.frame(
    canonical_name = c("A", "B", "C"),
    region = c("North", "North", "North"),
    stringsAsFactors = FALSE
  )
  expect_equal(compute_region_plot_height(df, "North"), "780px")
})

test_that("compute_region_plot_height only counts stations in target region", {
  df <- data.frame(
    canonical_name = c("A", "B", "C", "D"),
    region = c("North", "North", "South", "South"),
    stringsAsFactors = FALSE
  )
  expect_equal(compute_region_plot_height(df, "North"), "520px")
  expect_equal(compute_region_plot_height(df, "South"), "520px")
})
