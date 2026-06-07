#' Find Authors in the Zotero Database
#'
#' Searches the 'creators' table for a specific pattern in either the first
#' or last name. This is the first step before merging duplicate authors.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param pattern A character string containing a regular expression to search for.
#' @param ignore_case Logical. Should the search be case-insensitive? Default is TRUE.
#'
#' @return A tibble containing the matching creators.
#' @export
zot_find_authors <- function(con, pattern, ignore_case = TRUE) {
  creators_db <- dplyr::tbl(con, "creators") |> dplyr::collect()

  # Search locally using base R grepl for maximum regex compatibility
  matches <- creators_db |>
    dplyr::filter(
      grepl(pattern, lastName, ignore.case = ignore_case) |
        grepl(pattern, firstName, ignore.case = ignore_case)
    )

  if (nrow(matches) == 0) {
    cli::cli_alert_info(
      "No authors found matching the pattern: {.val {pattern}}"
    )
  }

  return(matches)
}

#' Merge Duplicate Authors in Zotero
#'
#' Merges multiple creator IDs into a single target ID. It updates all
#' publication links in the 'itemCreators' table to point to the target ID
#' and removes the now orphaned duplicate IDs from the 'creators' table.
#'
#' If run interactively without a `target_id`, it will prompt the user to
#' select the correct author name from a menu and ask for confirmation.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param merge_ids A numeric vector of creatorIDs that should be merged.
#' @param target_id (Optional) The specific creatorID that should be kept. If NULL
#' and the session is interactive, a selection menu is presented.
#'
#' @return Invisible TRUE if successful, FALSE if cancelled or failed.
#' @export
zot_merge_authors <- function(con, merge_ids, target_id = NULL) {
  # 1. Fetch details of the selected IDs
  creators_db <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID %in% merge_ids) |>
    dplyr::collect()

  if (nrow(creators_db) < 2) {
    cli::cli_abort(
      "You must provide at least two valid {.arg creatorID}s to perform a merge."
    )
  }

  manual_entry <- FALSE
  new_last <- ""
  new_first <- ""

  # 2. Interactive Selection (if target_id is missing)
  if (is.null(target_id)) {
    if (!interactive()) {
      cli::cli_abort(
        "Non-interactive session requires explicitly providing a {.arg target_id}."
      )
    }

    cli::cli_h2("Author Selection")
    cli::cli_text(
      "Multiple variations found. Which one is the {.strong CORRECT} spelling?"
    )
    choices <- sprintf(
      "%s, %s (ID: %d)",
      creators_db$lastName,
      creators_db$firstName,
      creators_db$creatorID
    )

    # Add the manual entry option at the end
    choices <- c(choices, "None of the above (Enter manually)")

    selection <- utils::menu(
      choices,
      title = "Select the target author to keep:"
    )

    if (selection == 0) {
      cli::cli_alert_warning("Operation cancelled by user.")
      return(invisible(FALSE))
    }

    # Handle manual entry
    if (selection == length(choices)) {
      manual_entry <- TRUE
      target_id <- creators_db$creatorID[1] # Arbitrarily pick the first ID to become the new master

      cli::cli_h2("Manual Name Entry")
      new_last <- trimws(readline("Enter correct Last Name: "))
      new_first <- trimws(readline(
        "Enter correct First Name (leave empty if none): "
      ))

      target_name <- sprintf("%s, %s", new_last, new_first)
      # Clean up trailing comma if first name is empty
      target_name <- sub(", $", "", target_name)
    } else {
      target_id <- creators_db$creatorID[selection]
      target_author <- creators_db[creators_db$creatorID == target_id, ]
      target_name <- sprintf(
        "%s, %s",
        target_author$lastName,
        target_author$firstName
      )
    }
  } else {
    # If target_id was provided non-interactively
    target_author <- creators_db[creators_db$creatorID == target_id, ]
    target_name <- sprintf(
      "%s, %s",
      target_author$lastName,
      target_author$firstName
    )
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
      "All {.val {length(merge_ids)}} selected authors will be permanently merged to:"
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

  # If manual entry was chosen, update the target ID's name in the database first
  if (manual_entry) {
    query_manual_update <- "UPDATE creators SET lastName = ?, firstName = ? WHERE creatorID = ?"
    DBI::dbExecute(
      con,
      query_manual_update,
      params = list(new_last, new_first, target_id)
    )
    cli::cli_alert_success(
      "Updated master ID {.val {target_id}} to new name {.strong {target_name}}."
    )
  }

  # Create placeholders for the SQL IN clause (e.g., "?, ?, ?")
  placeholders <- paste(rep("?", length(duplicate_ids)), collapse = ", ")

  # Step A: Update itemCreators (repoint to the target ID)
  query_update <- sprintf(
    "UPDATE itemCreators SET creatorID = ? WHERE creatorID IN (%s)",
    placeholders
  )
  params_update <- c(target_id, duplicate_ids) # First param is target_id, rest are duplicates

  res_links <- DBI::dbExecute(
    con,
    query_update,
    params = as.list(params_update)
  )
  cli::cli_alert_success(
    "Re-linked {.val {res_links}} publication(s) to the master ID."
  )

  # Step B: Delete the orphaned duplicate authors from the creators table
  query_delete <- sprintf(
    "DELETE FROM creators WHERE creatorID IN (%s)",
    placeholders
  )

  res_delete <- DBI::dbExecute(
    con,
    query_delete,
    params = as.list(duplicate_ids)
  )
  cli::cli_alert_success(
    "Deleted {.val {res_delete}} orphaned duplicate author(s)."
  )

  cli::cli_alert_success(
    "Authors successfully merged into {.strong {target_name}}!"
  )
  return(invisible(TRUE))
}
