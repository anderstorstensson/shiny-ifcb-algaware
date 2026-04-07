make_imgs <- function(n) {
  data.frame(id = seq_len(n), stringsAsFactors = FALSE)
}

test_that("paginate_images returns first page correctly", {
  imgs <- make_imgs(25)
  result <- paginate_images(imgs, page = 1L, page_size = 10L)
  expect_equal(nrow(result), 10L)
  expect_equal(result$id, 1:10)
})

test_that("paginate_images returns last page with fewer rows", {
  imgs <- make_imgs(25)
  result <- paginate_images(imgs, page = 3L, page_size = 10L)
  expect_equal(nrow(result), 5L)
  expect_equal(result$id, 21:25)
})

test_that("paginate_images clamps page to last valid page", {
  imgs <- make_imgs(15)
  result <- paginate_images(imgs, page = 99L, page_size = 10L)
  expect_equal(nrow(result), 5L)
  expect_equal(result$id, 11:15)
})

test_that("paginate_images returns all rows when page_size >= nrow", {
  imgs <- make_imgs(8)
  result <- paginate_images(imgs, page = 1L, page_size = 100L)
  expect_equal(nrow(result), 8L)
})

test_that("paginate_images returns second page correctly", {
  imgs <- make_imgs(30)
  result <- paginate_images(imgs, page = 2L, page_size = 10L)
  expect_equal(result$id, 11:20)
})
