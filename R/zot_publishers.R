#' Find Publishers in the Zotero Database
#'
#' Searches the 'itemDataValues' table for specific publisher names.
#' This is the first step before merging duplicate or inconsistently named publishers.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param pattern A character string containing a regular expression to search for.
#' @param ignore_case Logical. Should the search be case-insensitive? Default is TRUE.
#'
#' @return A tibble containing the matching publisher strings and their valueIDs.
#' @export
zot_find_publishers <- function(con, pattern, ignore_case = TRUE) {
  fields_db <- dplyr::tbl(con, "fields")
  item_data_db <- dplyr::tbl(con, "itemData")
  item_data_values_db <- dplyr::tbl(con, "itemDataValues")

  # Get the fieldID for 'publisher'
  pub_field_id <- fields_db |>
    dplyr::filter(fieldName == "publisher") |>
    dplyr::pull(fieldID)

  if (length(pub_field_id) == 0) {
    cli::cli_abort("Could not find the 'publisher' field in the database.")
  }

  # Fetch all unique publisher names currently in use
  publishers_raw <- item_data_db |>
    dplyr::filter(fieldID == pub_field_id) |>
    dplyr::inner_join(item_data_values_db, by = "valueID") |>
    dplyr::select(valueID, publisher = value) |>
    dplyr::distinct() |>
    dplyr::collect()

  # Search locally using base R grepl
  matches <- publishers_raw |>
    dplyr::filter(grepl(pattern, publisher, ignore.case = ignore_case)) |>
    dplyr::arrange(publisher)

  if (nrow(matches) == 0) {
    cli::cli_alert_info(
      "No publishers found matching the pattern: {.val {pattern}}"
    )
  }

  return(matches)
}

#' Merge Duplicate Publishers in Zotero
#'
#' Merges multiple publisher string IDs into a single target ID. It updates all
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
zot_merge_publishers <- function(con, merge_ids, target_id = NULL) {
  fields_db <- dplyr::tbl(con, "fields")
  pub_field_id <- fields_db |>
    dplyr::filter(fieldName == "publisher") |>
    dplyr::pull(fieldID)

  # 1. Fetch details of the selected IDs
  publishers_db <- dplyr::tbl(con, "itemDataValues") |>
    dplyr::filter(valueID %in% merge_ids) |>
    dplyr::collect()

  if (nrow(publishers_db) < 2) {
    cli::cli_abort(
      "You must provide at least two valid {.arg valueID}s to perform a merge."
    )
  }

  manual_entry <- FALSE
  new_publisher_name <- ""

  # 2. Interactive Selection (if target_id is missing)
  if (is.null(target_id)) {
    if (!interactive()) {
      cli::cli_abort(
        "Non-interactive session requires explicitly providing a {.arg target_id}."
      )
    }

    cli::cli_h2("Publisher Selection")
    cli::cli_text(
      "Multiple variations found. Which one is the {.strong CORRECT} format?"
    )
    choices <- sprintf(
      "%s (ID: %d)",
      publishers_db$value,
      publishers_db$valueID
    )

    # Add the manual entry option at the end
    choices <- c(choices, "None of the above (Enter manually)")

    selection <- utils::menu(
      choices,
      title = "Select the target publisher to keep:"
    )

    if (selection == 0) {
      cli::cli_alert_warning("Operation cancelled by user.")
      return(invisible(FALSE))
    }

    # Handle manual entry
    if (selection == length(choices)) {
      manual_entry <- TRUE
      target_id <- publishers_db$valueID[1] # Arbitrarily pick the first ID to become the new master

      cli::cli_h2("Manual Publisher Entry")
      new_publisher_name <- trimws(readline("Enter correct Publisher Name: "))

      if (nchar(new_publisher_name) == 0) {
        cli::cli_abort("Publisher name cannot be empty.")
      }

      target_name <- new_publisher_name
    } else {
      target_id <- publishers_db$valueID[selection]
      target_name <- publishers_db$value[publishers_db$valueID == target_id]
    }
  } else {
    # If target_id was provided non-interactively
    target_name <- publishers_db$value[publishers_db$valueID == target_id]
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
      "All {.val {length(merge_ids)}} selected publishers will be permanently merged to:"
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
      params = list(new_publisher_name, target_id)
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
    "Re-linked {.val {res_links}} publication(s) to the master publisher ID."
  )

  # Step B: Clean up orphaned strings
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
    "Publishers successfully merged into {.strong {target_name}}!"
  )
  return(invisible(TRUE))
}
