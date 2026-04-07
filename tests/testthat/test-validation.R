test_that("parse_image_ids parses simple IDs correctly", {
  ids <- c("D20221023T000155_IFCB134_42", "D20221023T000155_IFCB134_100")
  result <- algaware:::parse_image_ids(ids)
  expect_equal(nrow(result), 2)
  expect_equal(result$sample_name, c("D20221023T000155_IFCB134",
                                     "D20221023T000155_IFCB134"))
  expect_equal(result$roi_number, c(42L, 100L))
})

test_that("parse_image_ids handles single ID", {
  result <- algaware:::parse_image_ids("D20221023T000155_IFCB134_1")
  expect_equal(nrow(result), 1)
  expect_equal(result$sample_name, "D20221023T000155_IFCB134")
  expect_equal(result$roi_number, 1L)
})

test_that("get_region_context returns NULL class when no classes", {
  rv <- list(
    current_region = "EAST",
    baltic_samples = "sample1",
    westcoast_samples = character(0),
    classifications = data.frame(
      sample_name = character(0),
      class_name = character(0),
      stringsAsFactors = FALSE
    ),
    current_class_idx = 1L
  )
  result <- algaware:::get_region_context(rv)
  expect_null(result$current_class)
  expect_equal(length(result$classes), 0)
})

test_that("get_region_context includes unclassified in class list", {
  rv <- list(
    current_region = "EAST",
    baltic_samples = "sample1",
    westcoast_samples = character(0),
    classifications = data.frame(
      sample_name = c("sample1", "sample1"),
      class_name = c("ClassA", "unclassified"),
      stringsAsFactors = FALSE
    ),
    current_class_idx = 1L
  )
  result <- algaware:::get_region_context(rv)
  expect_true("unclassified" %in% result$classes)
  expect_true("ClassA" %in% result$classes)
})

test_that("get_region_context resolves EAST region correctly", {
  rv <- list(
    current_region = "EAST",
    baltic_samples = c("s1", "s2"),
    westcoast_samples = "s3",
    classifications = data.frame(
      sample_name = c("s1", "s1", "s2", "s3"),
      class_name = c("diatom", "dino", "diatom", "cocco"),
      stringsAsFactors = FALSE
    ),
    current_class_idx = 1L
  )
  result <- algaware:::get_region_context(rv)
  expect_equal(result$region_samples, c("s1", "s2"))
  expect_equal(result$classes, c("diatom", "dino"))
  expect_equal(result$current_class, "diatom")
})
