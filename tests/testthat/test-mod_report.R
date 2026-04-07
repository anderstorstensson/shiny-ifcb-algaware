test_that("build_readiness_items has four entries", {
  items <- build_readiness_items(TRUE, FALSE, NULL, NULL)
  expect_length(items, 4L)
})

test_that("build_readiness_items marks data_loaded correctly", {
  items_yes <- build_readiness_items(TRUE,  FALSE, NULL, NULL)
  items_no  <- build_readiness_items(FALSE, FALSE, NULL, NULL)
  expect_true(items_yes[[1]]$ok)
  expect_false(items_no[[1]]$ok)
})

test_that("build_readiness_items marks ctd_loaded correctly", {
  items <- build_readiness_items(TRUE, TRUE, NULL, NULL)
  expect_true(items[[2]]$ok)
})

test_that("build_readiness_items marks mosaics by NULL check", {
  fake_mosaic <- list(width = 1800)
  items <- build_readiness_items(TRUE, FALSE, fake_mosaic, NULL)
  expect_true(items[[3]]$ok)
  expect_false(items[[4]]$ok)
})

test_that("build_readiness_items treats NA data_loaded as FALSE", {
  items <- build_readiness_items(NA, FALSE, NULL, NULL)
  expect_false(items[[1]]$ok)
})

test_that("build_readiness_items labels are non-empty strings", {
  items <- build_readiness_items(FALSE, FALSE, NULL, NULL)
  labels <- vapply(items, function(x) x$label, "")
  expect_true(all(nzchar(labels)))
})
