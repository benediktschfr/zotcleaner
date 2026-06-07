#' Create a Realistic In-Memory Zotero Test Database
#'
#' This function simulates Zotero's exact relational database schema in-memory.
#' It populates essential tables (creators, items, itemCreators, fields,
#' itemData, and itemDataValues) with realistic examples spanning astrophysics,
#' molecular biology, quantum chemistry, medicine, and computer science.
#'
#' @return A DBI connection to a temporary, populated SQLite database in RAM.
#' @export
zot_mock_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # ----------------------------------------------------------------------------
  # 1. TABLE: creators (Authors) - Length: 20
  # fieldMode: 0 = Two fields (First/Last name), 1 = One field (Institution)
  # ----------------------------------------------------------------------------
  creators_raw <- tibble::tibble(
    creatorID = 1:20,
    lastName = c(
      # Case 1: Split main author (Andrew P. Harrison) [IDs: 1, 2, 3, 4]
      "Harrison",
      "Harrison",
      "Harrison",
      "Harrison",

      # Case 2: Split co-author (Sarah Taylor) [IDs: 5, 6]
      "Taylor",
      "Taylor",

      # Case 3: Institutions mistakenly imported as split First/Last names [IDs: 7, 8, 9]
      "Organization",
      "Laboratory",
      "Collaboration",

      # Case 4: Swapped Chinese names (Last name imported into first name field) [IDs: 10, 11]
      "Jian",
      "Ying",

      # Case 5: Messy casing [IDs: 12, 13, 14]
      "ALEXANDER",
      "nakamura",
      "von weizsäcker",

      # Case 6: Clean control entries [IDs: 15, 16, 17, 18, 19, 20]
      "Einstein",
      "Curie",
      "Watson",
      "Crick",
      "Turing",
      "Lovelace"
    ),
    firstName = c(
      # Harrison variations
      "A.",
      "Andrew",
      "Andrew P.",
      "A.",

      # Taylor variations
      "S.",
      "Sarah",

      # Institutions (First name field mistakenly contains the beginning)
      "World Health",
      "European Molecular Biology",
      "LIGO Scientific",

      # Chinese names (Given name field contains the family name: Wang Jian, Zhang Ying)
      "Wang",
      "Zhang",

      # Casing errors
      "Robert",
      "yuki",
      "carl friedrich",

      # Clean entries
      "Albert",
      "Marie",
      "James",
      "Francis",
      "Alan",
      "Ada"
    ),
    fieldMode = c(
      rep(0, 6), # Harrison & Taylor (correctly set as persons)
      0,
      0,
      0, # Institutions mistakenly imported as persons (fieldMode = 0)!
      rep(0, 11) # All others default to person mode
    )
  )

  # ----------------------------------------------------------------------------
  # 2. TABLE: items (Publications) - Length: 15
  # itemTypeID: 1 = Journal Article, 2 = Book, 3 = Conference Paper
  # ----------------------------------------------------------------------------
  items_raw <- tibble::tibble(
    itemID = 101:115,
    itemTypeID = c(
      1,
      2,
      1,
      1, # Astrophysics & Physics
      1,
      1,
      3, # Molecular Biology & Genetics
      1,
      1, # Quantum Chemistry & Materials Science
      1,
      2,
      1, # Medicine & Virology
      1,
      3,
      1 # Computer Science & AI
    )
  )

  # ----------------------------------------------------------------------------
  # 3. TABLE: itemCreators (Relational Join Table) - Length: 21
  # creatorTypeID: 1 = Author
  # orderIndex: Position in author list (0 = first author, 1 = second, etc.)
  # ----------------------------------------------------------------------------
  item_creators_raw <- tibble::tibble(
    itemID = c(
      101,
      101, # Astrophysics (Harrison, A. & Einstein, Albert)
      102, # Physics Book (Harrison, Andrew)
      103,
      103, # Gravitational Waves (LIGO Collaboration & Taylor, S.)
      104, # Quantum Paper (Harrison, Andrew P.)
      105,
      105, # Biology (Watson & Crick)
      106, # Biology HTML Title (Taylor, Sarah)
      107,
      107, # Bioinformatics (EMBL & Turing, Alan)
      108, # Chemistry UPPERCASE (ALEXANDER, Robert)
      109, # Chemistry (Curie, Marie)
      110,
      110, # Virology (WHO & Nakamura, Yuki)
      111, # Medical Book (Wang Jian)
      112,
      112, # Medicine (Zhang Ying & von Weizsäcker)
      113, # Computer Science (Lovelace, Ada)
      114, # AI Conference Paper (Harrison, A. - ID 4)
      115 # Computer Science (Nakamura, Yuki)
    ),
    creatorID = c(
      1,
      15, # Harrison (1) & Einstein (15)
      2, # Harrison (2)
      9,
      5, # LIGO (9) & Taylor (5)
      3, # Harrison (3)
      17,
      18, # Watson (17) & Crick (18)
      6, # Taylor (6)
      8,
      19, # EMBL (8) & Turing (19)
      12, # ALEXANDER, Robert (12)
      16, # Curie, Marie (16)
      7,
      13, # WHO (7) & Nakamura (13)
      10, # Wang Jian (10)
      11,
      14, # Zhang Ying (11) & von Weizsäcker (14)
      20, # Lovelace (20)
      4, # Harrison (4)
      13 # Nakamura (13)
    ),
    creatorTypeID = rep(1, 21), # Recycled to full length of 21
    orderIndex = c(
      0,
      1, # Item 101
      0, # Item 102
      0,
      1, # Item 103
      0, # Item 104
      0,
      1, # Item 105
      0, # Item 106
      0,
      1, # Item 107
      0, # Item 108
      0, # Item 109
      0,
      1, # Item 110
      0, # Item 111
      0,
      1, # Item 112
      0, # Item 113
      0, # Item 114
      0 # Item 115
    ) # Recycled to full length of 21
  )

  # ----------------------------------------------------------------------------
  # REAL ZOTERO METADATA ARCHITECTURE (EAV Structure) - Length: 32
  # ----------------------------------------------------------------------------
  fields_raw <- tibble::tibble(
    fieldID = 1:3,
    fieldName = c("title", "publicationTitle", "date")
  )

  item_data_values_raw <- tibble::tibble(
    valueID = 1:32,
    value = c(
      # 1-15: TITLES (Contains various formatting errors)
      "OBSERVATION OF GRAVITATIONAL WAVES FROM A BINARY BLACK HOLE MERGER", # 1: UPPERCASE
      "A Practical Guide to Relativistic Cosmology and Spacetime Geometry", # 2
      "Multimessenger Observations of a Binary Neutron Star Coalescence", # 3
      "An Introduction to Quantum Field Theory in Curved Spacetime", # 4
      "Molecular Structure of Nucleic Acids: A Structure for Deoxyribose Nucleic Acid", # 5
      "The role of <i>Arabidopsis thaliana</i> CRY2 in circadian clock regulation &amp; flowering time", # 6: HTML noise
      "CRISPR-Cas9 gene editing in <sub>in vivo</sub> models of muscular dystrophy &apos;a breakthrough&apos;", # 7: HTML noise
      "SYNTHESIS AND CHARACTERIZATION OF NOVEL GRAPHENE OXIDE NANOCOMPOSITES", # 8: UPPERCASE
      "Radioactive properties of Radium and Polonium isotopes", # 9
      "World Health Report on Global Pandemic Preparedness", # 10
      "Clinical study of traditional medicines in East Asian populations", # 11
      "On the Electrodynamics of Moving Bodies", # 12
      "Sketch of the Analytical Engine Invented by Charles Babbage", # 13
      "Computing Machinery and Intelligence", # 14
      "A Method for Obtaining Digital Signatures and Public-Key Cryptosystems", # 15

      # 16-27: JOURNALS (Inconsistencies)
      "Physical Review Letters", # 16
      "Phys Rev Lett", # 17
      "Phys. Rev. Lett.", # 18
      "Nature", # 19
      "Nature Biotechnology", # 20
      "Nat Biotechnol", # 21
      "Nat. Biotechnol.", # 22
      "Journal of the American Chemical Society", # 23
      "J Am Chem Soc", # 24
      "J. Am. Chem. Soc.", # 25
      "The Lancet", # 26
      "Journal of Virology", # 27

      # 28-32: DATES
      "2016",
      "1953",
      "2020",
      "1905",
      "1950"
    )
  )

  item_data_raw <- tibble::tibble(
    itemID = c(
      # Titles (fieldID = 1)
      101:115,
      # Journals (fieldID = 2)
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
      # Dates (fieldID = 3)
      101,
      105,
      106,
      112,
      114
    ),
    fieldID = c(
      rep(1, 15), # All 15 have titles
      rep(2, 12), # 12 have journals
      rep(3, 5) # 5 have dates
    ),
    valueID = c(
      # Title IDs
      1:15,
      # Journal IDs
      16:27,
      # Date IDs
      28:32
    )
  )

  # ----------------------------------------------------------------------------
  # 4. WRITE TABLES TO DATABASE
  # ----------------------------------------------------------------------------
  dplyr::copy_to(
    con,
    creators_raw,
    name = "creators",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    items_raw,
    name = "items",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    item_creators_raw,
    name = "itemCreators",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    fields_raw,
    name = "fields",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    item_data_raw,
    name = "itemData",
    temporary = FALSE,
    overwrite = TRUE
  )
  dplyr::copy_to(
    con,
    item_data_values_raw,
    name = "itemDataValues",
    temporary = FALSE,
    overwrite = TRUE
  )

  cli::cli_alert_success("Zotero mock database successfully created in memory!")

  con
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

  flat_view
}
