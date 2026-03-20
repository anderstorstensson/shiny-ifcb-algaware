test_that("filter_metadata by cruise", {
  metadata <- data.frame(
    pid = c("s1", "s2", "s3"),
    cruise = c("C001", "C001", "C002"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02", "2022-01-03")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, cruise = "C001")
  expect_equal(nrow(result), 2)
  expect_true(all(result$cruise == "C001"))
})

test_that("filter_metadata by date range", {
  metadata <- data.frame(
    pid = c("s1", "s2", "s3"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-15", "2022-02-01")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, date_from = "2022-01-10",
                             date_to = "2022-01-20")
  expect_equal(nrow(result), 1)
  expect_equal(result$pid, "s2")
})

test_that("filter_metadata returns all when no filters", {
  metadata <- data.frame(
    pid = c("s1", "s2"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata)
  expect_equal(nrow(result), 2)
})

test_that("filter_metadata handles empty cruise string", {
  metadata <- data.frame(
    pid = c("s1", "s2"),
    cruise = c("C001", "C002"),
    sample_time = as.POSIXct(c("2022-01-01", "2022-01-02")),
    stringsAsFactors = FALSE
  )

  result <- filter_metadata(metadata, cruise = "")
  expect_equal(nrow(result), 2)
})

test_that("read_h5_classifications returns empty df for empty dir", {
  tmp_dir <- file.path(tempdir(), paste0("h5_empty_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- read_h5_classifications(tmp_dir)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("sample_name", "roi_number", "class_name", "score"))
})

test_that("download_raw_data creates dest_dir", {
  tmp_dir <- file.path(tempdir(), paste0("raw_test_", Sys.getpid()))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  mockery::stub(download_raw_data, "iRfcb::ifcb_download_dashboard_data", NULL)

  download_raw_data("https://example.com", c("sample1"), tmp_dir)
  expect_true(dir.exists(tmp_dir))
})

test_that("download_raw_data skips existing files", {
  tmp_dir <- file.path(tempdir(), paste0("raw_skip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create an existing .roi file
  writeLines("", file.path(tmp_dir, "sample1.roi"))

  callback_called <- FALSE
  callback <- function(current, total, msg) {
    callback_called <<- TRUE
  }

  download_raw_data("https://example.com", c("sample1"), tmp_dir,
                     progress_callback = callback)
  expect_true(callback_called)
})

test_that("download_features skips existing files", {
  tmp_dir <- file.path(tempdir(), paste0("feat_skip_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dir, "sample1.csv"))

  callback_msgs <- character(0)
  callback <- function(current, total, msg) {
    callback_msgs <<- c(callback_msgs, msg)
  }

  download_features("https://example.com", c("sample1"), tmp_dir,
                     progress_callback = callback)
  expect_true(any(grepl("already downloaded", callback_msgs)))
})

test_that("copy_classification_files skips existing files", {
  tmp_src <- file.path(tempdir(), paste0("class_src_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest_", Sys.getpid()))
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  writeLines("", file.path(tmp_dest, "D20220101T000000_IFCB134_class.h5"))

  callback_msgs <- character(0)
  callback <- function(current, total, msg) {
    callback_msgs <<- c(callback_msgs, msg)
  }

  copy_classification_files(
    tmp_src,
    c("D20220101T000000_IFCB134"),
    tmp_dest,
    progress_callback = callback
  )
  expect_true(any(grepl("already copied", callback_msgs)))
})

test_that("copy_classification_files finds files in yearly subdirs", {
  tmp_src <- file.path(tempdir(), paste0("class_src2_", Sys.getpid()))
  tmp_dest <- file.path(tempdir(), paste0("class_dest2_", Sys.getpid()))
  year_dir <- file.path(tmp_src, "class2022_v3")
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dest, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(tmp_src, tmp_dest), recursive = TRUE), add = TRUE)

  h5_file <- file.path(year_dir, "D20220101T000000_IFCB134_class.h5")
  writeLines("fake h5", h5_file)

  result <- copy_classification_files(
    tmp_src,
    c("D20220101T000000_IFCB134"),
    tmp_dest
  )

  expect_true(file.exists(
    file.path(tmp_dest, "D20220101T000000_IFCB134_class.h5")
  ))
})
