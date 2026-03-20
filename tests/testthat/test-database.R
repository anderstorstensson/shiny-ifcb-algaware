test_that("get_db_path constructs correct path", {
  result <- get_db_path("/some/folder")
  expect_equal(result, "/some/folder/annotations.sqlite")
})

test_that("init_db_schema creates tables", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  algaware:::init_db_schema(con)

  tables <- DBI::dbListTables(con)
  expect_true("annotations" %in% tables)
  expect_true("class_lists" %in% tables)
  expect_true("class_taxonomy" %in% tables)
  expect_true("global_class_list" %in% tables)
})

test_that("init_db_schema is idempotent", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  algaware:::init_db_schema(con)
  expect_no_error(algaware:::init_db_schema(con))
})

test_that("save_annotations_db returns TRUE for empty input", {
  result <- save_annotations_db(
    tempfile(),
    data.frame(sample_name = character(0), roi_number = integer(0),
               class_name = character(0))
  )
  expect_true(result)
})

test_that("save_annotations_db and load_annotations_db roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("db_test_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("D20220101T000000_IFCB134", "D20220101T000000_IFCB134"),
    roi_number = c(1L, 2L),
    class_name = c("Skeletonema", "Chaetoceros"),
    stringsAsFactors = FALSE
  )

  result <- save_annotations_db(db_path, annotations, annotator = "test_user")
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 2)
  expect_true("Skeletonema" %in% loaded$class_name)
  expect_true("Chaetoceros" %in% loaded$class_name)
  expect_true(all(loaded$annotator == "test_user"))
})

test_that("save_annotations_db validates class names against class list", {
  tmp_dir <- file.path(tempdir(), paste0("db_valid_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("sample1", "sample1"),
    roi_number = c(1L, 2L),
    class_name = c("ValidClass", "InvalidClass"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- save_annotations_db(db_path, annotations,
                                   class_list = c("ValidClass")),
    "Rejected annotations with invalid class names"
  )
  expect_true(result)

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 1)
  expect_equal(loaded$class_name, "ValidClass")
})

test_that("load_annotations_db returns empty df for missing file", {
  result <- load_annotations_db("/nonexistent/annotations.sqlite")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("sample_name" %in% names(result))
})

test_that("load_annotations_db filters by sample_names", {
  tmp_dir <- file.path(tempdir(), paste0("db_filter_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  annotations <- data.frame(
    sample_name = c("sample_A", "sample_A", "sample_B"),
    roi_number = c(1L, 2L, 1L),
    class_name = c("ClassA", "ClassB", "ClassC"),
    stringsAsFactors = FALSE
  )

  save_annotations_db(db_path, annotations)

  loaded <- load_annotations_db(db_path, sample_names = "sample_A")
  expect_equal(nrow(loaded), 2)
  expect_true(all(loaded$sample_name == "sample_A"))
})

test_that("save and load global_class_list roundtrip", {
  tmp_dir <- file.path(tempdir(), paste0("db_gcl_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  classes <- c("Skeletonema", "Chaetoceros", "detritus")
  result <- save_global_class_list_db(db_path, classes)
  expect_true(result)

  loaded <- load_global_class_list_db(db_path)
  expect_equal(loaded, classes)
})

test_that("save_global_class_list_db returns TRUE for empty input", {
  result <- save_global_class_list_db(tempfile(), character(0))
  expect_true(result)
  result2 <- save_global_class_list_db(tempfile(), NULL)
  expect_true(result2)
})

test_that("load_global_class_list_db returns NULL for missing file", {
  result <- load_global_class_list_db("/nonexistent/annotations.sqlite")
  expect_null(result)
})

test_that("load_global_class_list_db returns NULL for empty table", {
  tmp_dir <- file.path(tempdir(), paste0("db_empty_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  algaware:::init_db_schema(con)
  DBI::dbDisconnect(con)

  result <- load_global_class_list_db(db_path)
  expect_null(result)
})

test_that("resolve_class_list delegates to load_global_class_list_db", {
  tmp_dir <- file.path(tempdir(), paste0("db_resolve_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")
  save_global_class_list_db(db_path, c("A", "B", "C"))

  result <- resolve_class_list(db_path)
  expect_equal(result, c("A", "B", "C"))
})

test_that("save_global_class_list_db replaces existing data", {
  tmp_dir <- file.path(tempdir(), paste0("db_replace_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  save_global_class_list_db(db_path, c("A", "B"))
  save_global_class_list_db(db_path, c("X", "Y", "Z"))

  result <- load_global_class_list_db(db_path)
  expect_equal(result, c("X", "Y", "Z"))
})

test_that("save_annotations_db upserts (replaces on conflict)", {
  tmp_dir <- file.path(tempdir(), paste0("db_upsert_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "annotations.sqlite")

  ann1 <- data.frame(
    sample_name = "sample1", roi_number = 1L,
    class_name = "ClassA", stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, ann1, annotator = "user1")

  ann2 <- data.frame(
    sample_name = "sample1", roi_number = 1L,
    class_name = "ClassB", stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, ann2, annotator = "user2")

  loaded <- load_annotations_db(db_path)
  expect_equal(nrow(loaded), 1)
  expect_equal(loaded$class_name, "ClassB")
  expect_equal(loaded$annotator, "user2")
})
