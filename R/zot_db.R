# ==============================================================================
# zotcleaner: Test Environment and Realistic Zotero Database Mock
# ==============================================================================

library(dplyr)
library(tibble)
library(DBI)
library(RSQLite)
library(dbplyr)
library(cli)

# ==============================================================================
# DATASETS
# ==============================================================================

#' Zotero Mock Creators Data
#'
#' A dataset simulating the 'creators' table in a Zotero database.
#' Contains messy author names, capitalization errors, swapped Asian names,
#' and misclassified institutions to test cleaning functions.
#'
#' @format A data frame (tibble) with 20 rows and 4 variables:
#' \describe{
#'   \item{creatorID}{Primary key for the creator.}
#'   \item{lastName}{Last name or institutional name.}
#'   \item{firstName}{First name.}
#'   \item{fieldMode}{0 for two-field mode (persons), 1 for single-field mode (institutions).}
#' }
"zot_creators_raw" <- tibble::tibble(
  creatorID = 1:20,
  lastName = c(
    "Harrison",
    "Harrison",
    "Harrison",
    "Harrison",
    "Taylor",
    "Taylor",
    "Organization",
    "Laboratory",
    "Collaboration",
    "Jian",
    "Ying",
    "ALEXANDER",
    "nakamura",
    "von weizsäcker",
    "Einstein",
    "Curie",
    "Watson",
    "Crick",
    "Turing",
    "Lovelace"
  ),
  firstName = c(
    "A.",
    "Andrew",
    "Andrew P.",
    "A.",
    "S.",
    "Sarah",
    "World Health",
    "European Molecular Biology",
    "LIGO Scientific",
    "Wang",
    "Zhang",
    "Robert",
    "yuki",
    "carl friedrich",
    "Albert",
    "Marie",
    "James",
    "Francis",
    "Alan",
    "Ada"
  ),
  fieldMode = c(rep(0, 6), 0, 0, 0, rep(0, 11))
)

#' Zotero Mock Items Data
#'
#' A dataset simulating the 'items' table in a Zotero database.
#'
#' @format A data frame (tibble) with 15 rows and 2 variables:
#' \describe{
#'   \item{itemID}{Primary key for the item (publication).}
#'   \item{itemTypeID}{Type of publication (1 = Journal Article, 2 = Book, 3 = Conference Paper).}
#' }
"zot_items_raw" <- tibble::tibble(
  itemID = 101:115,
  itemTypeID = c(1, 2, 1, 1, 1, 1, 3, 1, 1, 1, 2, 1, 1, 3, 1)
)

#' Zotero Mock Item Creators Link Data
#'
#' A dataset simulating the 'itemCreators' table, linking publications (items)
#' to their authors (creators) and defining author order.
#'
#' @format A data frame (tibble) with 15 rows and 4 variables:
#' \describe{
#'   \item{itemID}{Foreign key referencing items.}
#'   \item{creatorID}{Foreign key referencing creators.}
#'   \item{creatorTypeID}{Type of creator (usually 1 for Author).}
#'   \item{orderIndex}{Position in the author list (0 = first author).}
#' }
"zot_item_creators_raw" <- tibble::tibble(
  itemID = c(
    101,
    101,
    102,
    103,
    103,
    104,
    105,
    105,
    106,
    107,
    107,
    108,
    109,
    110,
    110,
    111,
    112,
    112,
    113,
    114,
    115
  ),
  creatorID = c(
    1,
    16,
    2,
    9,
    5,
    3,
    18,
    19,
    6,
    8,
    20,
    13,
    17,
    7,
    14,
    11,
    12,
    15,
    21,
    4,
    14
  ),
  creatorTypeID = rep(1, 21),
  orderIndex = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0)
)

#' Zotero Mock Fields Data
#'
#' A dataset simulating the EAV 'fields' table, defining metadata field types.
#'
#' @format A data frame (tibble) with 3 rows and 2 variables:
#' \describe{
#'   \item{fieldID}{Primary key for the field.}
#'   \item{fieldName}{Name of the field (e.g., "title", "publicationTitle").}
#' }
"zot_fields_raw" <- tibble::tibble(
  fieldID = 1:3,
  fieldName = c("title", "publicationTitle", "date")
)

#' Zotero Mock Item Data Values
#'
#' A dataset simulating the 'itemDataValues' table, storing all unique
#' metadata strings (titles, journal names, dates, etc.).
#'
#' @format A data frame (tibble) with 32 rows and 2 variables:
#' \describe{
#'   \item{valueID}{Primary key for the string value.}
#'   \item{value}{The actual text string.}
#' }
"zot_item_data_values_raw" <- tibble::tibble(
  valueID = 1:32,
  value = c(
    "OBSERVATION OF GRAVITATIONAL WAVES FROM A BINARY BLACK HOLE MERGER",
    "A Practical Guide to Relativistic Cosmology and Spacetime Geometry",
    "Multimessenger Observations of a Binary Neutron Star Coalescence",
    "An Introduction to Quantum Field Theory in Curved Spacetime",
    "Molecular Structure of Nucleic Acids: A Structure for Deoxyribose Nucleic Acid",
    "The role of <i>Arabidopsis thaliana</i> CRY2 in circadian clock regulation &amp; flowering time",
    "CRISPR-Cas9 gene editing in <sub>in vivo</sub> models of muscular dystrophy &apos;a breakthrough&apos;",
    "SYNTHESIS AND CHARACTERIZATION OF NOVEL GRAPHENE OXIDE NANOCOMPOSITES",
    "Radioactive properties of Radium and Polonium isotopes",
    "World Health Report on Global Pandemic Preparedness",
    "Clinical study of traditional medicines in East Asian populations",
    "On the Electrodynamics of Moving Bodies",
    "Sketch of the Analytical Engine Invented by Charles Babbage",
    "Computing Machinery and Intelligence",
    "A Method for Obtaining Digital Signatures and Public-Key Cryptosystems",
    "Physical Review Letters",
    "Phys Rev Lett",
    "Phys. Rev. Lett.",
    "Nature",
    "Nature Biotechnology",
    "Nat Biotechnol",
    "Nat. Biotechnol.",
    "Journal of the American Chemical Society",
    "J Am Chem Soc",
    "J. Am. Chem. Soc.",
    "The Lancet",
    "Journal of Virology",
    "2016",
    "1953",
    "2020",
    "1905",
    "1950"
  )
)

#' Zotero Mock Item Data Link Table
#'
#' A dataset simulating the 'itemData' table, linking publications to their
#' specific metadata values.
#'
#' @format A data frame (tibble) with 32 rows and 3 variables:
#' \describe{
#'   \item{itemID}{Foreign key referencing items.}
#'   \item{fieldID}{Foreign key referencing fields.}
#'   \item{valueID}{Foreign key referencing itemDataValues.}
#' }
"zot_item_data_raw" <- tibble::tibble(
  itemID = c(
    101:115,
    101,
    102,
    103,
    105,
    106,
    107,
    108,
    109,
    110,
    112,
    114,
    115,
    101,
    105,
    106,
    112,
    114
  ),
  fieldID = c(rep(1, 15), rep(2, 12), rep(3, 5)),
  valueID = c(
    1:15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32
  )
)


# ==============================================================================
# MAIN MOCK FUNCTION
# ==============================================================================

#' Create a Realistic In-Memory Zotero Test Database
#'
#' This function simulates Zotero's exact relational database schema in-memory.
#' It utilizes pre-defined package datasets (e.g., zot_creators_raw) to populate
#' the SQLite tables.
#'
#' @return A DBI connection to a temporary, populated SQLite database in RAM.
#' @export
zot_mock_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

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

  return(con)
}

# ==============================================================================
# CONNECTION & HELPER FUNCTIONS
# ==============================================================================

#' Backup the Zotero Database
#'
#' Creates a timestamped backup copy of the Zotero SQLite database in the
#' current working directory. Highly recommended before running any cleaning
#' operations on a real database.
#'
#' @param path Character. Path to the original Zotero SQLite database file.
#' @return Character. The path to the created backup file (invisibly).
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

#' Retrieve a Flat View of All Items, Titles, and Authors
#'
#' Helper function to demonstrate the "before and after" effects of database cleaning.
#'
#' @param con An active DBI connection to a Zotero database.
#' @return A data frame containing item IDs, clean titles, and concatenated author names.
#' @export
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

  return(flat_view)
}
