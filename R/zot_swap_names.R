# ==============================================================================
# zotcleaner: First/Last Name Swapping Operations
# ==============================================================================

#' Handle Swapped First and Last Names in Zotero
#'
#' These functions allow you to systematically identify and correct instances
#' where an author's first and last names have been imported in reverse order
#' (highly common with Asian names or imports that lacked proper comma delimiters).
#'
#' @param con An active DBI connection to a Zotero database.
#' @param family_names Character vector of typical family names to look for
#'   in the `firstName` field.
#' @param creator_ids A numeric vector of creatorIDs whose first and last names
#'   should be swapped.
#'
#' @return
#'   * `zot_find_swapped_names`: A tibble containing the suspicious creators.
#'   * `zot_swap_names`: Invisible TRUE if successful, FALSE if cancelled.
#'
#' @examples
#' # 1. Create a clean in-memory test database
#' mock_db <- zot_mock_db()
#'
#' # 2. Find authors where typical Asian last names are in the first name field
#' swapped_authors <- zot_find_swapped_names(mock_db)
#' print(swapped_authors)
#'
#' # 3. Swap the names back to correct order programmatically
#' # (In an interactive session, leaving out target_id/prompting works automatically)
#' zot_swap_names(
#'   con = mock_db,
#'   creator_ids = swapped_authors$creatorID
#' )
#'
#' # 4. Disconnect safely
#' zot_disconnect_db(mock_db)
#'
#' @rdname zot_swap_names
#' @order 1
#' @export
zot_find_swapped_names <- function(
  con,
  family_names = c(
    "Wang",
    "Li",
    "Zhang",
    "Liu",
    "Chen",
    "Yang",
    "Huang",
    "Zhao",
    "Wu",
    "Zhou",
    "Kim",
    "Lee",
    "Park",
    "Choi"
  )
) {
  cli::cli_h2("Scanning for swapped first and last names")

  creators_db <- dplyr::tbl(con, "creators") |>
    dplyr::filter(fieldMode == 0 | is.na(fieldMode)) |>
    dplyr::collect()

  # Create a regex pattern to find these family names as whole words in the firstName field
  pattern <- paste0("(?i)\\b(", paste(family_names, collapse = "|"), ")\\b")

  matches <- creators_db |>
    dplyr::filter(grepl(pattern, firstName, perl = TRUE))

  if (nrow(matches) == 0) {
    cli::cli_alert_success("No suspicious swapped names found.")
  } else {
    cli::cli_alert_info(
      "Found {.val {nrow(matches)}} author(s) where the first name looks like a typical last name."
    )
  }

  return(matches)
}

#' @rdname zot_swap_names
#' @order 2
#' @export
zot_swap_names <- function(con, creator_ids) {
  if (length(creator_ids) == 0) {
    cli::cli_abort("You must provide at least one {.arg creator_id} to swap.")
  }

  creators_db <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID %in% creator_ids) |>
    dplyr::collect()

  if (nrow(creators_db) == 0) {
    cli::cli_abort(
      "None of the provided {.arg creator_ids} were found in the database."
    )
  }

  # 1. Interactive confirmation
  if (interactive()) {
    cli::cli_h2("Name Swapping Preview")
    cli::cli_text(
      "The first and last names of the following authors will be swapped:"
    )

    for (i in seq_len(min(5, nrow(creators_db)))) {
      cli::cli_bullets(c(
        "x" = "Current: Last: {.val {creators_db$lastName[i]}}, First: {.val {creators_db$firstName[i]}}",
        "v" = "{.strong Target:  Last: {creators_db$firstName[i]}, First: {creators_db$lastName[i]}}"
      ))
    }

    if (nrow(creators_db) > 5) {
      cli::cli_text("{.style_italic ... and {nrow(creators_db) - 5} more.}")
    }

    cat("\n")
    ans <- readline("Do you want to apply these swaps? (y/n): ")
    if (tolower(trimws(ans)) != "y") {
      cli::cli_alert_warning("Swap cancelled.")
      return(invisible(FALSE))
    }
  }

  # 2. Execute the database updates
  cli::cli_h2("Updating database")

  # We use the fetched values to safely swap them via parameterized query
  update_query <- "UPDATE creators SET lastName = ?, firstName = ? WHERE creatorID = ?"

  # Note the swapped order: we put the old firstName into lastName, and old lastName into firstName
  params <- list(
    creators_db$firstName,
    creators_db$lastName,
    creators_db$creatorID
  )
  res <- DBI::dbExecute(con, update_query, params = params)

  cli::cli_alert_success(
    "Successfully swapped names for {.val {res}} author(s)!"
  )
  return(invisible(TRUE))
}
