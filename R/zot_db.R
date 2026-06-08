#' Create a Realistic In-Memory Zotero Test Database
#'
#' This function simulates Zotero's exact relational database schema in-memory
#' by directly loading the pre-defined package datasets (e.g., \code{zot_creators_raw}).
#'
#' @return A DBI connection to a temporary, populated SQLite database in RAM.
#' @import dbplyr
#' @export
zot_mock_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Copy package datasets dynamically into the SQLite tables
  dplyr::copy_to(
    con,
    zot_creators_raw,
    name = "creators",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    zot_items_raw,
    name = "items",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    zot_item_creators_raw,
    name = "itemCreators",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    zot_fields_raw,
    name = "fields",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    zot_item_data_raw,
    name = "itemData",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    zot_item_data_values_raw,
    name = "itemDataValues",
    temporary = FALSE,
    overwrite = TRUE
  )

  cli::cli_alert_success("Zotero mock database successfully created in memory!")

  con
}

#' Get Flat view of literature
#'
#' @param con Active DBI database connection.
#' @noRd
zot_get_flat_view <- function(con) {
  items_db <- dplyr::tbl(con, "items")
  creators_db <- dplyr::tbl(con, "creators")
  item_creators_db <- dplyr::tbl(con, "itemCreators")
  fields_db <- dplyr::tbl(con, "fields")
  item_data_db <- dplyr::tbl(con, "itemData")
  item_data_values_db <- dplyr::tbl(con, "itemDataValues")

  # 1. Extract titles from the EAV structure
  titles <- item_data_db |>
    dplyr::inner_join(fields_db, by = "fieldID") |>
    dplyr::filter(fieldName == "title") |>
    dplyr::inner_join(item_data_values_db, by = "valueID") |>
    dplyr::select(itemID, title = value) |>
    dplyr::collect()

  # 2. Fetch and aggregate authors per item
  authors <- item_creators_db |>
    dplyr::inner_join(creators_db, by = "creatorID") |>
    dplyr::mutate(author_name = paste0(lastName, ", ", firstName)) |>
    dplyr::select(itemID, author_name, orderIndex) |>
    dplyr::collect() |>
    dplyr::group_by(itemID) |>
    dplyr::arrange(orderIndex) |>
    dplyr::summarise(
      all_authors = paste(author_name, collapse = " | "),
      .groups = "drop"
    )

  # 3. Join everything together
  flat_view <- items_db |>
    dplyr::select(itemID) |>
    dplyr::collect() |>
    dplyr::inner_join(titles, by = "itemID") |>
    dplyr::left_join(authors, by = "itemID")

  flat_view
}

# ==============================================================================
# zotcleaner: Database Connection and Backup Utilities
# ==============================================================================

#' Backup the Zotero Database
#'
#' Creates a timestamped backup copy of the Zotero SQLite database in the
#' current working directory. Highly recommended before running any cleaning
#' operations on a real database.
#'
#' @param path Character. Path to the original Zotero SQLite database file.
#' @return Character. The path to the created backup file (invisibly).
#' @importFrom cli cli_abort cli_alert_success
#' @export
zot_backup_db <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("The database file {.val {path}} does not exist.")
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_name <- paste0("zotero_backup_", timestamp, ".sqlite")
  backup_path <- file.path(getwd(), backup_name)

  success <- file.copy(from = path, to = backup_path, overwrite = FALSE)

  if (success) {
    cli::cli_alert_success(
      "Database backup successfully created: {.val {backup_path}}"
    )
  } else {
    cli::cli_abort("Failed to create database backup.")
  }

  return(invisible(backup_path))
}

#' Connect to a Real Zotero Database
#'
#' Establishes a DBI connection to a local Zotero SQLite database.
#'
#' @param path Character. Path to the Zotero SQLite database file (typically 'zotero.sqlite').
#' @return A DBI connection object to the Zotero database.
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom cli cli_abort cli_alert_success
#' @export
zot_connect_db <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("The database file {.val {path}} does not exist.")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  cli::cli_alert_success(
    "Successfully connected to Zotero database at {.val {path}}"
  )

  return(con)
}

#' Disconnect from the Zotero Database
#'
#' Safely closes the connection to the SQLite database (mock or real) and
#' provides a status message.
#'
#' @param con An active DBI connection.
#' @return Invisible TRUE if successful.
#' @importFrom DBI dbDisconnect dbIsValid
#' @importFrom cli cli_alert_success cli_alert_info
#' @export
zot_disconnect_db <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
    cli::cli_alert_success("Database connection successfully closed.")
  } else {
    cli::cli_alert_info("Database connection was already closed or invalid.")
  }
  return(invisible(TRUE))
}
