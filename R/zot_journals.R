#' Find Journals in the Zotero Database
#'
#' Searches the 'itemDataValues' table for specific journal names (publicationTitle).
#' This is the first step before merging duplicate or inconsistently abbreviated journals.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param pattern A character string containing a regular expression to search for.
#' @param ignore_case Logical. Should the search be case-insensitive? Default is TRUE.
#'
#' @return A tibble containing the matching journal strings and their valueIDs.
#' @export
zot_find_journals <- function(con, pattern, ignore_case = TRUE) {
  fields_db <- dplyr::tbl(con, "fields")
  item_data_db <- dplyr::tbl(con, "itemData")
  item_data_values_db <- dplyr::tbl(con, "itemDataValues")

  # Get the fieldID for 'publicationTitle'
  pub_field_id <- fields_db |>
    dplyr::filter(fieldName == "publicationTitle") |>
    dplyr::pull(fieldID)

  if (length(pub_field_id) == 0) {
    cli::cli_abort(
      "Could not find the 'publicationTitle' field in the database."
    )
  }

  # Fetch all unique journal names currently in use
  journals_raw <- item_data_db |>
    dplyr::filter(fieldID == pub_field_id) |>
    dplyr::inner_join(item_data_values_db, by = "valueID") |>
    dplyr::select(valueID, journal = value) |>
    dplyr::distinct() |>
    dplyr::collect()

  # Search locally using base R grepl
  matches <- journals_raw |>
    dplyr::filter(grepl(pattern, journal, ignore.case = ignore_case)) |>
    dplyr::arrange(journal)

  if (nrow(matches) == 0) {
    cli::cli_alert_info(
      "No journals found matching the pattern: {.val {pattern}}"
    )
  }

  return(matches)
}

#' Merge Duplicate Journals in Zotero
#'
#' Merges multiple journal string IDs into a single target ID. It updates all
#' publication links in the 'itemData' table to point to the target ID
#' and cleans up the orphaned string fragments.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param merge_ids A numeric vector of valueIDs that should be merged.
#' @param target_id (Optional) The specific valueID that should be kept. If NULL
#' and the session is interactive, a selection menu is presented.
#'
#' @return Invisible TRUE if successful, FALSE if cancelled or failed.
#' @export
zot_merge_journals <- function(con, merge_ids, target_id = NULL) {
  fields_db <- dplyr::tbl(con, "fields")
  pub_field_id <- fields_db |>
    dplyr::filter(fieldName == "publicationTitle") |>
    dplyr::pull(fieldID)

  # 1. Fetch details of the selected IDs
  journals_db <- dplyr::tbl(con, "itemDataValues") |>
    dplyr::filter(valueID %in% merge_ids) |>
    dplyr::collect()

  if (nrow(journals_db) < 2) {
    cli::cli_abort(
      "You must provide at least two valid {.arg valueID}s to perform a merge."
    )
  }

  manual_entry <- FALSE
  new_journal_name <- ""

  # 2. Interactive Selection (if target_id is missing)
  if (is.null(target_id)) {
    if (!interactive()) {
      cli::cli_abort(
        "Non-interactive session requires explicitly providing a {.arg target_id}."
      )
    }

    cli::cli_h2("Journal Selection")
    cli::cli_text(
      "Multiple variations found. Which one is the {.strong CORRECT} format?"
    )
    choices <- sprintf("%s (ID: %d)", journals_db$value, journals_db$valueID)

    # Add the manual entry option at the end
    choices <- c(choices, "None of the above (Enter manually)")

    selection <- utils::menu(
      choices,
      title = "Select the target journal to keep:"
    )

    if (selection == 0) {
      cli::cli_alert_warning("Operation cancelled by user.")
      return(invisible(FALSE))
    }

    # Handle manual entry
    if (selection == length(choices)) {
      manual_entry <- TRUE
      target_id <- journals_db$valueID[1] # Arbitrarily pick the first ID to become the new master

      cli::cli_h2("Manual Journal Entry")
      new_journal_name <- trimws(readline("Enter correct Journal Name: "))

      if (nchar(new_journal_name) == 0) {
        cli::cli_abort("Journal name cannot be empty.")
      }

      target_name <- new_journal_name
    } else {
      target_id <- journals_db$valueID[selection]
      target_name <- journals_db$value[journals_db$valueID == target_id]
    }
  } else {
    # If target_id was provided non-interactively
    target_name <- journals_db$value[journals_db$valueID == target_id]
  }

  # Validate target_id
  if (!target_id %in% merge_ids) {
    cli::cli_abort(
      "The {.arg target_id} must be one of the IDs provided in {.arg merge_ids}."
    )
  }

  duplicate_ids <- setdiff(merge_ids, target_id)

  # 3. Confirmation Prompt
  if (interactive()) {
    cli::cli_alert_danger(
      "All {.val {length(merge_ids)}} selected journals will be permanently merged to:"
    )
    cli::cli_bullets(c(
      "*" = "{.strong {target_name}} (ID: {.val {target_id}})"
    ))

    ans <- readline("Do you want to proceed? (y/n): ")
    if (tolower(trimws(ans)) != "y") {
      cli::cli_alert_warning("Merge cancelled.")
      return(invisible(FALSE))
    }
  }

  # 4. Execute the SQL Merge
  cli::cli_h2("Executing Merge")

  # If manual entry was chosen, update the target ID's string in the database first
  if (manual_entry) {
    query_manual_update <- "UPDATE itemDataValues SET value = ? WHERE valueID = ?"
    DBI::dbExecute(
      con,
      query_manual_update,
      params = list(new_journal_name, target_id)
    )
    cli::cli_alert_success(
      "Updated master ID {.val {target_id}} to new name {.strong {target_name}}."
    )
  }

  # Create placeholders for the SQL IN clause (e.g., "?, ?, ?")
  placeholders <- paste(rep("?", length(duplicate_ids)), collapse = ", ")

  # Step A: Update itemData (repoint the specific field to the target ID)
  query_update <- sprintf(
    "UPDATE itemData SET valueID = ? WHERE valueID IN (%s) AND fieldID = ?",
    placeholders
  )
  params_update <- c(target_id, duplicate_ids, pub_field_id)

  res_links <- DBI::dbExecute(
    con,
    query_update,
    params = as.list(params_update)
  )
  cli::cli_alert_success(
    "Re-linked {.val {res_links}} publication(s) to the master journal ID."
  )

  # Step B: Clean up orphaned strings (Only delete if they are no longer used by ANY item)
  query_cleanup <- sprintf(
    "DELETE FROM itemDataValues WHERE valueID IN (%s) AND valueID NOT IN (SELECT valueID FROM itemData)",
    placeholders
  )
  res_cleanup <- DBI::dbExecute(
    con,
    query_cleanup,
    params = as.list(duplicate_ids)
  )
  cli::cli_alert_success(
    "Cleaned up {.val {res_cleanup}} orphaned string(s) from the database."
  )

  cli::cli_alert_success(
    "Journals successfully merged into {.strong {target_name}}!"
  )
  return(invisible(TRUE))
}
