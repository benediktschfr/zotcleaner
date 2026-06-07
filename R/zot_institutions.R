#' Find Misclassified Institutional Authors
#'
#' Scans the 'creators' table for entries currently classified as persons
#' (fieldMode = 0) but containing keywords strongly suggesting they are
#' actually institutions or corporate authors (e.g., "Organization",
#' "Society", "University").
#'
#' @param con An active DBI connection to a Zotero database.
#' @param custom_keywords Optional character vector of additional regex keywords
#'   to search for (case-insensitive).
#'
#' @return A tibble containing the suspicious creators.
#' @export
zot_find_institutions <- function(con, custom_keywords = NULL) {
  cli::cli_h2("Scanning for misclassified institutions")

  # Standard keywords that typically indicate an institutional author
  keywords <- c(
    "organization",
    "organisation",
    "society",
    "association",
    "laboratory",
    "collaboration",
    "institute",
    "institution",
    "department",
    "university",
    "center",
    "centre",
    "group",
    "committee",
    "agency",
    "board",
    "council",
    "foundation",
    "ministry",
    "hospital",
    "clinic"
  )

  if (!is.null(custom_keywords)) {
    keywords <- c(keywords, tolower(custom_keywords))
  }

  pattern <- paste0("(?i)\\b(", paste(keywords, collapse = "|"), ")\\b")

  # Fetch all creators currently classified as persons (fieldMode = 0 or NA)
  # Some older DB structures might have NA for fieldMode
  creators_db <- dplyr::tbl(con, "creators") |>
    dplyr::filter(fieldMode == 0 | is.na(fieldMode)) |>
    dplyr::collect()

  # Search locally using base R grepl
  matches <- creators_db |>
    dplyr::filter(
      grepl(pattern, lastName, perl = TRUE) |
        grepl(pattern, firstName, perl = TRUE)
    )

  if (nrow(matches) == 0) {
    cli::cli_alert_success(
      "No misclassified institutional authors found! Everything looks clean."
    )
  } else {
    cli::cli_alert_info(
      "Found {.val {nrow(matches)}} suspicious author(s) that look like institutions."
    )
  }

  return(matches)
}

#' Fix Misclassified Institutional Authors
#'
#' Converts selected creators from 'person' mode (fieldMode = 0) to
#' 'institution' mode (fieldMode = 1). It automatically concatenates the
#' firstName and lastName fields and stores the full name in the lastName
#' field, which is Zotero's standard for single-field corporate authors.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param creator_ids A numeric vector of creatorIDs to convert.
#'
#' @return Invisible TRUE if successful, FALSE if cancelled.
#' @export
zot_fix_institutions <- function(con, creator_ids) {
  if (length(creator_ids) == 0) {
    cli::cli_abort("You must provide at least one {.arg creator_id} to fix.")
  }

  creators_db <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID %in% creator_ids) |>
    dplyr::collect()

  if (nrow(creators_db) == 0) {
    cli::cli_abort(
      "None of the provided {.arg creator_ids} were found in the database."
    )
  }

  # Prepare the new names (concatenating first and last name safely)
  fixes <- creators_db |>
    dplyr::mutate(
      new_name = trimws(paste(
        ifelse(is.na(firstName) | firstName == "", "", firstName),
        ifelse(is.na(lastName) | lastName == "", "", lastName)
      ))
    )

  # 1. Interactive confirmation
  if (interactive()) {
    cli::cli_h2("Institution Conversion Preview")
    cli::cli_text(
      "The following entries will be converted to single-field institutions (fieldMode = 1):"
    )

    for (i in seq_len(min(5, nrow(fixes)))) {
      cli::cli_bullets(c(
        "x" = "First: {.val {fixes$firstName[i]}} | Last: {.val {fixes$lastName[i]}}",
        "v" = "{.strong {fixes$new_name[i]}}"
      ))
    }

    if (nrow(fixes) > 5) {
      cli::cli_text("{.style_italic ... and {nrow(fixes) - 5} more.}")
    }

    cat("\n")
    ans <- readline("Do you want to apply these conversions? (y/n): ")
    if (tolower(trimws(ans)) != "y") {
      cli::cli_alert_warning("Conversion cancelled.")
      return(invisible(FALSE))
    }
  }

  # 2. Execute the database updates
  cli::cli_h2("Updating database")

  # We update each creator: set fieldMode to 1, put full name in lastName, clear firstName
  update_query <- "UPDATE creators SET fieldMode = 1, lastName = ?, firstName = '' WHERE creatorID = ?"

  # Vectorized execution via list
  params <- list(fixes$new_name, fixes$creatorID)
  res <- DBI::dbExecute(con, update_query, params = params)

  cli::cli_alert_success(
    "Successfully converted {.val {res}} creator(s) to institutional authors!"
  )
  return(invisible(TRUE))
}
