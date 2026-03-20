test_that("create_mosaic errors with no images", {
  expect_error(create_mosaic(character(0)), "No images provided")
})

test_that("compute_median_color returns hex string", {
  # Create a small test image
  img <- magick::image_blank(10, 10, color = "#FF0000")
  result <- algaware:::compute_median_color(list(img))
  expect_match(result, "^#[0-9A-F]{6}$")
})

test_that("compute_median_color handles errors gracefully", {
  result <- algaware:::compute_median_color(list("not an image"))
  expect_equal(result, "#F0F0F0")
})

test_that("create_mosaic works with real images", {
  tmp_dir <- file.path(tempdir(), paste0("mosaic_test_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create test PNG files
  paths <- character(0)
  for (i in 1:6) {
    path <- file.path(tmp_dir, paste0("img_", i, ".png"))
    img <- magick::image_blank(50, 40, color = "white")
    magick::image_write(img, path)
    paths <- c(paths, path)
  }

  result <- create_mosaic(paths, n_images = 6)
  expect_s3_class(result, "magick-image")

  info <- magick::image_info(result)
  expect_true(info$width > 0)
  expect_true(info$height > 0)
})

test_that("create_mosaic samples when too many images", {
  tmp_dir <- file.path(tempdir(), paste0("mosaic_sample_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  paths <- character(0)
  for (i in 1:10) {
    path <- file.path(tmp_dir, paste0("img_", i, ".png"))
    img <- magick::image_blank(30, 30, color = "gray")
    magick::image_write(img, path)
    paths <- c(paths, path)
  }

  result <- create_mosaic(paths, n_images = 4)
  expect_s3_class(result, "magick-image")
})

test_that("justify_row centers single image", {
  img <- magick::image_blank(50, 30, color = "white")
  result <- algaware:::justify_row(
    list(img), 50, target_row_width = 200,
    target_height = 30, bg_color = "#F0F0F0"
  )
  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 200)
})

test_that("justify_row distributes padding for multiple images", {
  img1 <- magick::image_blank(40, 30, color = "white")
  img2 <- magick::image_blank(40, 30, color = "white")
  result <- algaware:::justify_row(
    list(img1, img2), c(40, 40),
    target_row_width = 200, target_height = 30,
    bg_color = "#F0F0F0"
  )
  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 200)
})
