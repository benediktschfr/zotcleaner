# zotcleaner

<!-- badges: start -->
<!-- badges: end -->

`zotcleaner` is an R package designed for automated database maintenance and metadata quality control of local [Zotero](https://www.zotero.org/) libraries. It provides a robust, programmatic workflow to scan, clean, and merge messy Zotero SQLite database entries natively in R.

While originally developed as a private tool to maintain a highly consistent personal research library, `zotcleaner` is shared openly because it can be highly beneficial for researchers, bibliometicians, and anyone else struggling with corrupt, duplicated, or poorly formatted metadata resulting from inconsistent web translators, library imports, or software migrations (e.g., from Citavi).

## Database Safety First
Modifying an active SQLite database can be risky. To protect your library, `zotcleaner` enforces a strict safety workflow:

1. **Backup**: Always create a timestamped backup before initiating any database transaction.
2. **In-Memory Testing**: Development, prototyping, and testing are done entirely on a realistic in-memory database mock.
3. **Safe Disconnect**: Database connections are verified and safely terminated to prevent locking issues.

---

## Installation
You can install the development version of `zotcleaner` directly from GitHub using the modern `pak` package manager:

```r
# If pak is not installed:
# install.packages("pak")

# Install zotcleaner from GitHub
pak::pak("benediktschfr/zotcleaner")
```

---

## Core Features and Workflow
The core functionality of `zotcleaner` revolves around scanning the relational structure of Zotero (which utilizes an Entity-Attribute-Value model for metadata fields) and safely updating values in-place. 

The following complete example demonstrates every major cleaning feature of the package using the built-in, realistic mock database.

### Full Mock Demonstration
```r
library(zotcleaner)

# 1. Establish a safe, in-memory test environment
# This database matches the real Zotero schema and is pre-populated with messy data.
con <- zot_mock_db()


# 2. Clean Publication Titles (HTML tags, entities, and UPPERCASE titles)
# This will strip tags like <i>, convert &amp; to &, and apply sentence case 
# while preserving internal capitalization (e.g., "mRNA", "MacBook").
zot_clean_titles(
  con = con,
  fix_html = TRUE,
  fix_uppercase = TRUE,
  to_case = "sentence"
)


# 3. Find and Merge Duplicate Authors
# Find all spelling variations of "Harrison" (e.g., A. Harrison vs. Andrew P. Harrison)
duplicate_authors <- zot_find_authors(con, pattern = "Harrison")
duplicate_authors

# Merge them programmatically into a single target master ID (ID: 3)
# In an interactive R session, leaving target_id = NULL triggers a secure menu.
zot_merge_authors(
  con = con,
  merge_ids = duplicate_authors$creatorID,
  target_id = 3
)


# 4. Resolve Swapped First/Last Names
# Many imports place Asian family names (e.g., Wang, Zhang) in the first name field.
swapped_names <- zot_find_swapped_names(con)
swapped_names

# Swap the first and last name columns back to their correct orientation
zot_swap_names(
  con = con,
  creator_ids = swapped_names$creatorID
)


# 5. Correct Misclassified Institutional Authors
# Finds corporate authors (like "World Health Organization") incorrectly split into
# person-style first/last names, joins them, and flags them as single-field (fieldMode = 1).
institutions <- zot_find_institutions(con)
institutions

zot_fix_institutions(
  con = con,
  creator_ids = institutions$creatorID
)


# 6. Resolve Duplicate Publishers
# Searches the database for variations of publishers (e.g., "Springer" vs "Springer-Verlag")
publishers <- zot_find_publishers(con, pattern = "Springer")
publishers

# Merge all duplicates into the master publisher ID (ID: 33)
zot_merge_publishers(
  con = con,
  merge_ids = publishers$valueID,
  target_id = 33
)


# 7. Safely disconnect from the database
zot_disconnect_db(con)
```

---

## Production Workflow
Once you are confident with the programmatic steps using the mock environment, you can run the cleaning steps on your real Zotero SQLite file.

```r
library(zotcleaner)

# Define path to your actual Zotero database
db_path <- "~/Zotero/zotero.sqlite"

# Create a timestamped backup in your current working directory
zot_backup_db(db_path)

# Connect to the production database
con <- zot_connect_db(db_path)

# Execute your cleaning pipeline...
# zot_clean_titles(con)

# Disconnect
zot_disconnect_db(con)
```

---

## Technical Details: The Zotero Relational Model
Under the hood, `zotcleaner` is optimized for Zotero’s internal data structures:

* **Item Creators**: Author relationships and orders are defined in `itemCreators` which links back to the master `creators` table. Merging authors redirects these links and sweeps away orphan records.
* **Fields and Values**: Text fields such as `title`, `publicationTitle` (journals), and `publisher` are saved as reusable strings inside `itemDataValues`. Changing or merging them rewires the links inside `itemData` and cleans up unused (orphaned) string rows to maintain a lightweight database file.

---

## Contributions
Contributions, feedback, and issue reports are highly welcome. 

* **Bug Reports**: If you find an edge case where Title Case/Sentence Case formatting behaves unexpectedly (e.g., with specific acronyms or complex punctuation), please open an issue with a reproducible example.
* **Pull Requests**: Feel free to submit pull requests for additional metadata fields (such as cleaning up URLs, DOIs, or tags).

Please ensure that you run `devtools::test()` and confirm that all unit tests pass before submitting your contributions.

---

## License
This package is licensed under the MIT License. See the `LICENSE` file for details.