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
"zot_creators_raw"

#' Zotero Mock Items Data
#'
#' A dataset simulating the 'items' table in a Zotero database.
#'
#' @format A data frame (tibble) with 15 rows and 2 variables:
#' \describe{
#'   \item{itemID}{Primary key for the item (publication).}
#'   \item{itemTypeID}{Type of publication (1 = Journal Article, 2 = Book, 3 = Conference Paper).}
#' }
"zot_items_raw"

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
"zot_item_creators_raw"

#' Zotero Mock Fields Data
#'
#' A dataset simulating the EAV 'fields' table, defining metadata field types.
#'
#' @format A data frame (tibble) with 3 rows and 2 variables:
#' \describe{
#'   \item{fieldID}{Primary key for the field.}
#'   \item{fieldName}{Name of the field (e.g., "title", "publicationTitle").}
#' }
"zot_fields_raw"

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
"zot_item_data_values_raw"

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
"zot_item_data_raw"
