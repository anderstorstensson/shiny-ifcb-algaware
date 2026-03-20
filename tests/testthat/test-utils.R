test_that("default_settings returns expected structure", {
  s <- algaware:::default_settings()
  expect_type(s, "list")
  expect_true("dashboard_url" %in% names(s))
  expect_true("dashboard_dataset" %in% names(s))
  expect_true("classification_path" %in% names(s))
  expect_true("raw_data_path" %in% names(s))
  expect_true("ferrybox_path" %in% names(s))
  expect_true("local_storage_path" %in% names(s))
  expect_true("db_folder" %in% names(s))
  expect_true("non_biological_classes" %in% names(s))
  expect_true("annotator" %in% names(s))
  expect_true("pixels_per_micron" %in% names(s))
  expect_true("n_mosaic_taxa" %in% names(s))
  expect_true("n_mosaic_images" %in% names(s))
})

test_that("load_settings returns defaults when no saved file", {
  withr::with_envvar(c(R_USER_CONFIG_DIR = tempdir()), {
    # Use a unique temp dir that has no settings file
    tmp <- file.path(tempdir(), paste0("algaware_test_", Sys.getpid()))
    withr::with_options(list(), {
      s <- load_settings()
      expect_type(s, "list")
      expect_true("dashboard_url" %in% names(s))
    })
  })
})

test_that("save_settings and load_settings roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("test_settings_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  settings_path <- file.path(tmp_dir, "settings.json")

  # Mock the settings path
  mockery::stub(save_settings, "get_settings_path", settings_path)
  mockery::stub(load_settings, "get_settings_path", settings_path)

  test_settings <- algaware:::default_settings()
  test_settings$dashboard_url <- "https://test.example.com"
  test_settings$annotator <- "test_user"

  save_settings(test_settings)
  loaded <- load_settings()

  expect_equal(loaded$dashboard_url, "https://test.example.com")
  expect_equal(loaded$annotator, "test_user")
})

test_that("parse_non_bio_classes splits correctly", {
  result <- algaware:::parse_non_bio_classes("detritus,Air_bubbles, Beads ,mix")
  expect_equal(result, c("detritus", "Air_bubbles", "Beads", "mix"))
})

test_that("parse_non_bio_classes handles empty string", {
  result <- algaware:::parse_non_bio_classes("")
  expect_length(result, 0)
})

test_that("parse_non_bio_classes handles single class", {
  result <- algaware:::parse_non_bio_classes("detritus")
  expect_equal(result, "detritus")
})

test_that("get_config_dir creates directory", {
  tmp <- file.path(tempdir(), paste0("config_test_", Sys.getpid()))
  mockery::stub(algaware:::get_config_dir, "tools::R_user_dir",
                file.path(tmp, "algaware"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- algaware:::get_config_dir()
  expect_true(dir.exists(result))
})

test_that("read_roi_dimensions returns empty data.frame for missing file", {
  result <- algaware:::read_roi_dimensions("/nonexistent/path.adc")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("roi_number", "width", "height", "roi_area"))
})
