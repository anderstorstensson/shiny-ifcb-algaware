#' Get path to the annotations SQLite database
#'
#' Uses the same database file as ClassiPyR for annotation compatibility.
#'
#' @param db_folder Path to the database directory.
#' @return Path to the SQLite database file.
#' @export
get_db_path <- function(db_folder) {
  file.path(db_folder, "annotations.sqlite")
}

#' Initialize the annotations database schema
#'
#' Creates tables compatible with ClassiPyR's schema.
#'
#' @param con A DBI connection object.
#' @return Invisible NULL.
#' @keywords internal
init_db_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS annotations (
      sample_name TEXT NOT NULL,
      roi_number  INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      annotator   TEXT,
      timestamp   TEXT DEFAULT (datetime('now')),
      is_manual   INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (sample_name, roi_number)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_lists (
      sample_name TEXT NOT NULL,
      class_index INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      PRIMARY KEY (sample_name, class_index)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS class_taxonomy (
      class_name      TEXT PRIMARY KEY,
      aphia_id        TEXT NOT NULL,
      scientific_name TEXT,
      accepted_name   TEXT,
      accepted_aphia_id TEXT,
      updated_at      TEXT DEFAULT (datetime('now'))
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS global_class_list (
      class_index INTEGER PRIMARY KEY,
      class_name  TEXT NOT NULL
    )
  ")

  # Migration: add is_manual column to existing databases

  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(annotations)")
  if (!"is_manual" %in% cols$name) {
    DBI::dbExecute(con,
      "ALTER TABLE annotations ADD COLUMN is_manual INTEGER NOT NULL DEFAULT 1")
  }

  invisible(NULL)
}

#' Save selected annotations to SQLite
#'
#' Stores annotations for selected images. Compatible with ClassiPyR's
#' annotation format.
#'
#' @param db_path Path to the SQLite database file.
#' @param annotations A data.frame with columns: \code{sample_name},
#'   \code{roi_number}, \code{class_name}.
#' @param annotator Name of the annotator.
#' @param class_list Character vector of all class names (for class_lists
#'   table).
#' @return Logical TRUE on success, FALSE on failure.
#' @export
save_annotations_db <- function(db_path, annotations, annotator = "",
                                class_list = character(0)) {
  if (nrow(annotations) == 0) return(TRUE)

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tryCatch({
    DBI::dbExecute(con, "BEGIN TRANSACTION")

    # Upsert annotations
    for (i in seq_len(nrow(annotations))) {
      DBI::dbExecute(con, "
        INSERT OR REPLACE INTO annotations
          (sample_name, roi_number, class_name, annotator, timestamp, is_manual)
        VALUES (?, ?, ?, ?, datetime('now'), 1)
      ", params = list(
        annotations$sample_name[i],
        as.integer(annotations$roi_number[i]),
        annotations$class_name[i],
        annotator
      ))
    }

    # Save class list per sample
    if (length(class_list) > 0) {
      samples <- unique(annotations$sample_name)
      for (samp in samples) {
        DBI::dbExecute(con,
          "DELETE FROM class_lists WHERE sample_name = ?",
          params = list(samp))
        for (j in seq_along(class_list)) {
          DBI::dbExecute(con, "
            INSERT INTO class_lists (sample_name, class_index, class_name)
            VALUES (?, ?, ?)
          ", params = list(samp, j, class_list[j]))
        }
      }
    }

    DBI::dbExecute(con, "COMMIT")
    TRUE
  }, error = function(e) {
    tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    warning("Failed to save annotations: ", e$message, call. = FALSE)
    FALSE
  })
}

#' Load annotations from SQLite
#'
#' @param db_path Path to the SQLite database file.
#' @param sample_names Optional character vector of sample names to filter.
#' @return A data.frame of annotations.
#' @export
load_annotations_db <- function(db_path, sample_names = NULL) {
  if (!file.exists(db_path)) {
    return(data.frame(sample_name = character(0),
                      roi_number = integer(0),
                      class_name = character(0),
                      annotator = character(0),
                      timestamp = character(0),
                      stringsAsFactors = FALSE))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  if (!is.null(sample_names) && length(sample_names) > 0) {
    placeholders <- paste(rep("?", length(sample_names)), collapse = ", ")
    query <- paste0("SELECT * FROM annotations WHERE sample_name IN (",
                    placeholders, ")")
    DBI::dbGetQuery(con, query, params = as.list(sample_names))
  } else {
    DBI::dbGetQuery(con, "SELECT * FROM annotations")
  }
}
