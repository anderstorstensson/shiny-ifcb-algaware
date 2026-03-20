test_that("strip_markdown removes bold markers", {
  result <- algaware:::strip_markdown("This is **bold** text")
  expect_equal(result, "This is bold text")
})

test_that("strip_markdown removes italic markers", {
  result <- algaware:::strip_markdown("This is *italic* text")
  expect_equal(result, "This is italic text")
})

test_that("strip_markdown preserves HAB asterisks after species names", {
  result <- algaware:::strip_markdown("Alexandrium catenella* was observed")
  expect_equal(result, "Alexandrium catenella* was observed")
})

test_that("strip_markdown handles mixed formatting", {
  result <- algaware:::strip_markdown("**Bold** and *italic* with species*")
  expect_equal(result, "Bold and italic with species*")
})

test_that("strip_markdown trims whitespace", {
  result <- algaware:::strip_markdown("  hello  ")
  expect_equal(result, "hello")
})

test_that("format_report_paragraph returns fpar with no taxa_lookup", {
  result <- algaware:::format_report_paragraph("Hello world")
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph returns fpar with empty taxa_lookup", {
  taxa <- data.frame(name = character(0), italic = logical(0),
                     stringsAsFactors = FALSE)
  result <- algaware:::format_report_paragraph("Hello world", taxa)
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph italicizes species names", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    italic = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "Skeletonema marinoi was dominant.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_station_data_for_prompt produces expected output", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    biovolume_mm3_per_liter = c(0.5, 0.1),
    carbon_ug_per_liter = c(10.0, 2.0),
    counts_per_liter = c(1000, 200),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_type(result, "character")
  expect_true(grepl("BY31", result))
  expect_true(grepl("Baltic Sea", result))
  expect_true(grepl("Skeletonema marinoi", result))
})

test_that("llm_available returns logical", {
  result <- llm_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})
