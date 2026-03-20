test_that("load_taxa_lookup returns a data.frame", {
  lookup <- load_taxa_lookup()
  expect_s3_class(lookup, "data.frame")
  expect_true("clean_names" %in% names(lookup))
  expect_true("name" %in% names(lookup))
  expect_true("AphiaID" %in% names(lookup))
  expect_true(nrow(lookup) > 0)
})
