# ---- validate_settings_inputs ----

test_that("validate_settings_inputs returns NULL for valid inputs", {
  result <- validate_settings_inputs(
    url = "https://example.com", ppm = 3.4, n_taxa = 5, n_imgs = 10
  )
  expect_null(result)
})

test_that("validate_settings_inputs allows empty URL", {
  result <- validate_settings_inputs(
    url = "", ppm = 3.4, n_taxa = 5, n_imgs = 10
  )
  expect_null(result)
})

test_that("validate_settings_inputs rejects non-http URL", {
  result <- validate_settings_inputs(
    url = "ftp://example.com", ppm = 3.4, n_taxa = 5, n_imgs = 10
  )
  expect_match(result, "http")
})

test_that("validate_settings_inputs rejects zero ppm", {
  result <- validate_settings_inputs(
    url = "", ppm = 0, n_taxa = 5, n_imgs = 10
  )
  expect_match(result, "[Pp]ixels per micron")
})

test_that("validate_settings_inputs rejects negative ppm", {
  result <- validate_settings_inputs(
    url = "", ppm = -1, n_taxa = 5, n_imgs = 10
  )
  expect_match(result, "[Pp]ixels per micron")
})

test_that("validate_settings_inputs rejects NA ppm", {
  result <- validate_settings_inputs(
    url = "", ppm = NA_real_, n_taxa = 5, n_imgs = 10
  )
  expect_match(result, "[Pp]ixels per micron")
})

test_that("validate_settings_inputs rejects n_taxa < 1", {
  result <- validate_settings_inputs(
    url = "", ppm = 3.4, n_taxa = 0, n_imgs = 10
  )
  expect_match(result, "[Tt]axa")
})

test_that("validate_settings_inputs rejects n_imgs < 1", {
  result <- validate_settings_inputs(
    url = "", ppm = 3.4, n_taxa = 5, n_imgs = 0
  )
  expect_match(result, "[Ii]mages per mosaic")
})

# ---- normalize_settings_path ----

test_that("normalize_settings_path returns empty string unchanged", {
  expect_equal(normalize_settings_path(""), "")
})

test_that("normalize_settings_path normalizes a real path", {
  result <- normalize_settings_path(tempdir())
  expect_true(nzchar(result))
  expect_false(grepl("//", result))
})

test_that("normalize_settings_path converts backslashes to forward slashes", {
  result <- normalize_settings_path("C:\\Users\\user\\classified")
  expect_false(grepl("\\\\", result))
  expect_true(grepl("/", result))
})

test_that("normalize_settings_path trims whitespace", {
  result <- normalize_settings_path("  /some/path  ")
  expect_false(grepl("^\\s|\\s$", result))
})

# ---- is_station_duplicate ----

test_that("is_station_duplicate returns FALSE for empty list", {
  expect_false(is_station_duplicate("A", list()))
})

test_that("is_station_duplicate detects existing station", {
  existing <- list(
    list(STATION_NAME = "ANHOLT E", COAST = "EAST"),
    list(STATION_NAME = "FLADEN", COAST = "WEST")
  )
  expect_true(is_station_duplicate("ANHOLT E", existing))
  expect_false(is_station_duplicate("BY5 BORNHOLMSDJ", existing))
})
