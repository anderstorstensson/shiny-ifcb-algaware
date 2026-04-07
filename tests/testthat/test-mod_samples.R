test_that("build_sample_table returns correct columns", {
  meta <- data.frame(
    pid             = c("samp_001", "samp_002"),
    STATION_NAME    = c("Station A", "Station B"),
    STATION_NAME_SHORT = c("A", "B"),
    COAST           = c("EAST", "WEST"),
    sample_time     = as.POSIXct(c("2024-06-01 10:00:00", "2024-06-01 11:00:00")),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0))

  expect_named(result, c("Status", "pid", "STATION_NAME_SHORT", "STATION_NAME",
                         "Region", "Time", "ROI_MB"))
  expect_equal(nrow(result), 2)
})

test_that("build_sample_table marks excluded samples correctly", {
  meta <- data.frame(
    pid             = c("samp_001", "samp_002", "samp_003"),
    STATION_NAME    = c("A", "B", "C"),
    STATION_NAME_SHORT = c("A", "B", "C"),
    COAST           = c("EAST", "EAST", "WEST"),
    sample_time     = as.POSIXct(c("2024-06-01", "2024-06-02", "2024-06-03")),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = "samp_002")

  expect_equal(result$Status[result$pid == "samp_001"], "Included")
  expect_equal(result$Status[result$pid == "samp_002"], "Excluded")
  expect_equal(result$Status[result$pid == "samp_003"], "Included")
})

test_that("build_sample_table maps COAST to Region labels", {
  meta <- data.frame(
    pid             = c("e", "w", "x"),
    STATION_NAME    = c("A", "B", "C"),
    STATION_NAME_SHORT = c("A", "B", "C"),
    COAST           = c("EAST", "WEST", "OTHER"),
    sample_time     = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03")),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0))

  expect_equal(result$Region[result$pid == "e"], "Baltic Sea")
  expect_equal(result$Region[result$pid == "w"], "West Coast")
  expect_equal(result$Region[result$pid == "x"], "OTHER")
})

test_that("build_sample_table deduplicates rows", {
  meta <- data.frame(
    pid             = c("samp_001", "samp_001"),
    STATION_NAME    = c("A", "A"),
    STATION_NAME_SHORT = c("A", "A"),
    COAST           = c("EAST", "EAST"),
    sample_time     = as.POSIXct(c("2024-06-01", "2024-06-01")),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0))

  expect_equal(nrow(result), 1)
})

test_that("build_sample_table sorts by sample_time then STATION_NAME", {
  meta <- data.frame(
    pid             = c("b", "a", "c"),
    STATION_NAME    = c("B", "A", "C"),
    STATION_NAME_SHORT = c("B", "A", "C"),
    COAST           = c("EAST", "EAST", "EAST"),
    sample_time     = as.POSIXct(c("2024-06-02", "2024-06-01", "2024-06-03")),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0))

  expect_equal(result$pid, c("a", "b", "c"))
})

test_that("build_sample_table sets ROI_MB to NA when raw_dir is NULL", {
  meta <- data.frame(
    pid             = "samp_001",
    STATION_NAME    = "A",
    STATION_NAME_SHORT = "A",
    COAST           = "EAST",
    sample_time     = as.POSIXct("2024-06-01"),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0),
                               raw_dir = NULL)

  expect_true(is.na(result$ROI_MB))
})

test_that("build_sample_table sets ROI_MB to NA when raw_dir does not exist", {
  meta <- data.frame(
    pid             = "samp_001",
    STATION_NAME    = "A",
    STATION_NAME_SHORT = "A",
    COAST           = "EAST",
    sample_time     = as.POSIXct("2024-06-01"),
    stringsAsFactors = FALSE
  )

  result <- build_sample_table(meta, excluded_samples = character(0),
                               raw_dir = "/nonexistent/path/raw")

  expect_true(is.na(result$ROI_MB))
})

test_that("mod_samples_server exclude_selected adds to excluded_samples", {
  rv <- shiny::reactiveValues(
    data_loaded         = TRUE,
    excluded_samples    = character(0),
    matched_metadata_all = data.frame(
      pid              = c("s1", "s2"),
      STATION_NAME     = c("A", "B"),
      STATION_NAME_SHORT = c("A", "B"),
      COAST            = c("EAST", "WEST"),
      sample_time      = as.POSIXct(c("2024-01-01", "2024-01-02")),
      stringsAsFactors = FALSE
    )
  )
  config <- list(local_storage_path = tempdir())

  shiny::testServer(mod_samples_server, args = list(rv = rv, config = config), {
    # Simulate row 1 selected and exclude button clicked
    session$setInputs(samples_table_rows_selected = 1L,
                      exclude_selected = 1)
    expect_true("s1" %in% rv$excluded_samples)
    expect_false("s2" %in% rv$excluded_samples)
  })
})

test_that("mod_samples_server include_selected removes from excluded_samples", {
  rv <- shiny::reactiveValues(
    data_loaded         = TRUE,
    excluded_samples    = c("s1", "s2"),
    matched_metadata_all = data.frame(
      pid              = c("s1", "s2"),
      STATION_NAME     = c("A", "B"),
      STATION_NAME_SHORT = c("A", "B"),
      COAST            = c("EAST", "WEST"),
      sample_time      = as.POSIXct(c("2024-01-01", "2024-01-02")),
      stringsAsFactors = FALSE
    )
  )
  config <- list(local_storage_path = tempdir())

  shiny::testServer(mod_samples_server, args = list(rv = rv, config = config), {
    session$setInputs(samples_table_rows_selected = 1L,
                      include_selected = 1)
    expect_false("s1" %in% rv$excluded_samples)
    expect_true("s2" %in% rv$excluded_samples)
  })
})

test_that("mod_samples_server include_all clears excluded_samples", {
  rv <- shiny::reactiveValues(
    data_loaded         = TRUE,
    excluded_samples    = c("s1", "s2"),
    matched_metadata_all = data.frame(
      pid              = c("s1", "s2"),
      STATION_NAME     = c("A", "B"),
      STATION_NAME_SHORT = c("A", "B"),
      COAST            = c("EAST", "WEST"),
      sample_time      = as.POSIXct(c("2024-01-01", "2024-01-02")),
      stringsAsFactors = FALSE
    )
  )
  config <- list(local_storage_path = tempdir())

  shiny::testServer(mod_samples_server, args = list(rv = rv, config = config), {
    session$setInputs(include_all = 1)
    expect_equal(rv$excluded_samples, character(0))
  })
})
