#' Smart Case Converter (Gruber/Pagaltzis Logic)
#'
#' An internal helper function that implements the John Gruber and
#' Aristotle Pagaltzis Title Case logic natively in R. It intelligently handles
#' small words, preserves internal capitalization (e.g., "MacBook", "CRISPR"),
#' ignores URLs/HTML tags, and handles punctuation rules.
#'
#' @param text Character vector of titles.
#' @param method "sentence" or "title".
#' @return Character vector of converted titles.
#' @noRd
zot_apply_smart_casing <- function(text, method = c("sentence", "title")) {
  method <- match.arg(method)

  # The list of "small words"
  small_words <- c(
    "a",
    "an",
    "and",
    "as",
    "at",
    "but",
    "by",
    "en",
    "for",
    "if",
    "in",
    "of",
    "on",
    "or",
    "the",
    "to",
    "v",
    "v.",
    "via",
    "vs",
    "vs."
  )

  # A robust tokenizer regex that safely isolates protected strings
  token_pattern <- paste(
    "<[^>]+>", # 1. HTML tags
    "[a-zA-Z0-9_.-]+://[^\\s]+", # 2. URLs
    "[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]+", # 3. Emails
    "[[:alpha:]]+(?:['’][[:alpha:]]+)*", # 4. Words (including apostrophes)
    "[^[:alpha:]<]+", # 5. Non-words (spaces, punctuation)
    "<", # 6. Literal '<' catch-all
    sep = "|"
  )

  vapply(
    text,
    function(s) {
      if (is.na(s) || nchar(s) == 0) {
        return(s)
      }

      # Lowercase the entire string if it contains no lowercase letters
      if (!grepl("[a-z]", s)) {
        s <- tolower(s)
      }

      # Tokenize the string
      tokens <- stringr::str_extract_all(s, token_pattern)[[1]]

      is_first_word <- TRUE
      after_terminal <- FALSE

      for (i in seq_along(tokens)) {
        tok <- tokens[i]

        # Skip protected tokens (HTML, URL, Email)
        if (grepl("^<[^>]+>$|://|@", tok)) {
          is_first_word <- FALSE
          after_terminal <- FALSE
          next
        }

        # If it is a non-word sequence (spaces, punctuation)
        if (!grepl("[[:alpha:]]", tok)) {
          # Check if it contains terminal punctuation (:, ., ?, !)
          if (grepl("[:.?!]", tok)) {
            after_terminal <- TRUE
          }
          next
        }

        # It is a word token!
        # Rule: Preserve words with internal capitals (e.g., "MacBook", "mRNA")
        has_internal_caps <- grepl(".+[A-Z]", tok)

        if (has_internal_caps) {
          is_first_word <- FALSE
          after_terminal <- FALSE
          next
        }

        is_small_word <- tolower(tok) %in% small_words
        # Check if this is the very last word in the string
        is_last_word <- !any(grepl(
          "[[:alpha:]]",
          tokens[(i + 1):length(tokens)]
        ))

        if (method == "title") {
          if (
            is_first_word || after_terminal || is_last_word || !is_small_word
          ) {
            tokens[i] <- paste0(
              toupper(substr(tok, 1, 1)),
              tolower(substr(tok, 2, nchar(tok)))
            )
          } else {
            tokens[i] <- tolower(tok)
          }
        } else if (method == "sentence") {
          if (is_first_word || after_terminal) {
            tokens[i] <- paste0(
              toupper(substr(tok, 1, 1)),
              tolower(substr(tok, 2, nchar(tok)))
            )
          } else {
            tokens[i] <- tolower(tok)
          }
        }

        is_first_word <- FALSE
        after_terminal <- FALSE
      }

      res <- paste(tokens, collapse = "")

      # Gruber's specific Title Case hyphenation rule (e.g., "Stand-In")
      if (method == "title") {
        small_re <- paste0(
          "(?i)(?<=[[:alpha:]]-)(",
          paste(small_words, collapse = "|"),
          ")(\\b)(?!-)"
        )
        res <- gsub(small_re, "\\u\\1", res, perl = TRUE)
      }

      return(res)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Clean Publication Titles in Zotero
#'
#' Scans the Zotero database for titles containing HTML tags, HTML entities
#' (like &amp;), or titles written entirely in UPPERCASE. It provides an
#' interactive review before cleaning the database in-place.
#'
#' @param con An active DBI connection to a Zotero database.
#' @param fix_html Logical. Should HTML tags and entities be stripped/decoded?
#' @param fix_uppercase Logical. Should ALL-CAPS titles be converted?
#' @param to_case Character. If `fix_uppercase` is TRUE, what case should be applied?
#'   Options are "sentence" (default, recommended by Zotero) or "title".
#'
#' @return Invisible TRUE if successful, FALSE otherwise.
#' @export
zot_clean_titles <- function(
  con,
  fix_html = TRUE,
  fix_uppercase = TRUE,
  to_case = c("sentence", "title")
) {
  to_case <- match.arg(to_case)
  cli::cli_h2("Scanning for dirty titles")

  # Fetch the necessary tables
  fields_db <- dplyr::tbl(con, "fields")
  item_data_db <- dplyr::tbl(con, "itemData")
  item_data_values_db <- dplyr::tbl(con, "itemDataValues")

  # Get the fieldID for 'title'
  title_field_id <- fields_db |>
    dplyr::filter(fieldName == "title") |>
    dplyr::pull(fieldID)

  if (length(title_field_id) == 0) {
    cli::cli_abort("Could not find the 'title' field in the database.")
  }

  # Get all title values
  titles_raw <- item_data_db |>
    dplyr::filter(fieldID == title_field_id) |>
    dplyr::inner_join(item_data_values_db, by = "valueID") |>
    dplyr::select(valueID, value) |>
    dplyr::distinct() |>
    dplyr::collect()

  # Prepare a dataframe for the cleaned values
  titles_clean <- titles_raw |>
    dplyr::mutate(clean_value = value)

  # 1. Fix HTML tags and entities
  if (fix_html) {
    titles_clean <- titles_clean |>
      dplyr::mutate(
        # Strip XML/HTML tags like <i>, </i>, <sub>
        clean_value = gsub("<[^>]+>", "", clean_value),
        # Decode common HTML entities
        clean_value = gsub("&amp;", "&", clean_value),
        clean_value = gsub("&apos;", "'", clean_value),
        clean_value = gsub("&quot;", "\"", clean_value),
        clean_value = gsub("&lt;", "<", clean_value),
        clean_value = gsub("&gt;", ">", clean_value)
      )
  }

  # 2. Fix UPPERCASE (and ALL-LOWERCASE) using Smart Casing
  if (fix_uppercase) {
    titles_clean <- titles_clean |>
      dplyr::mutate(
        # Identify titles that are entirely uppercase or entirely lowercase
        needs_casing = (grepl("[A-Z]", clean_value) &
          clean_value == toupper(clean_value)) |
          (grepl("[a-z]", clean_value) & clean_value == tolower(clean_value)),
        clean_value = ifelse(
          needs_casing,
          zot_apply_smart_casing(clean_value, method = to_case),
          clean_value
        )
      ) |>
      dplyr::select(-needs_casing)
  }

  # Find which titles actually changed
  changes <- titles_clean |>
    dplyr::filter(value != clean_value)

  if (nrow(changes) == 0) {
    cli::cli_alert_success("All titles are already clean! No changes needed.")
    return(invisible(TRUE))
  }

  cli::cli_alert_info(
    "Found {.val {nrow(changes)}} title(s) that need cleaning."
  )

  # 3. Interactive confirmation
  if (interactive()) {
    cli::cli_text("\nPreview of changes:")
    for (i in seq_len(min(5, nrow(changes)))) {
      cli::cli_bullets(c(
        "x" = "{.val {changes$value[i]}}",
        "v" = "{.strong {changes$clean_value[i]}}"
      ))
    }
    if (nrow(changes) > 5) {
      cli::cli_text("{.style_italic ... and {nrow(changes) - 5} more.}")
    }

    cat("\n")
    ans <- readline(
      "Do you want to apply these changes to the database? (y/n): "
    )
    if (tolower(trimws(ans)) != "y") {
      cli::cli_alert_warning("Title cleaning cancelled.")
      return(invisible(FALSE))
    }
  }

  # 4. Execute the update
  cli::cli_h2("Updating database")

  update_query <- "UPDATE itemDataValues SET value = ? WHERE valueID = ?"

  # Create lists for parameters to use vectorization in DBI
  params <- list(changes$clean_value, changes$valueID)
  res <- DBI::dbExecute(con, update_query, params = params)

  cli::cli_alert_success(
    "Successfully cleaned {.val {res}} title(s) in the database!"
  )
  return(invisible(TRUE))
}
