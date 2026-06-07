library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests für die Suchfunktion (zot_find_institutions)
# ------------------------------------------------------------------------------

test_that("zot_find_institutions correctly identifies suspicious keywords", {
  con <- zot_mock_db()

  # Wir erwarten, dass IDs 7 ("Organization"), 8 ("Laboratory") und 9 ("Collaboration") gefunden werden
  matches <- zot_find_institutions(con)

  expect_equal(nrow(matches), 3)
  expect_true(all(matches$creatorID %in% c(7, 8, 9)))

  # Test mit custom keywords
  # Füge einen Fake-Eintrag in die DB ein, um custom_keywords zu testen
  DBI::dbExecute(
    con,
    "INSERT INTO creators (creatorID, firstName, lastName, fieldMode) VALUES (999, 'Acme', 'Corporation', 0)"
  )

  matches_custom <- zot_find_institutions(
    con,
    custom_keywords = c("Corporation")
  )
  # Sollte jetzt auch unsere ID 999 finden
  expect_true(999 %in% matches_custom$creatorID)

  DBI::dbDisconnect(con)
})

test_that("zot_find_institutions handles clean databases gracefully", {
  con <- zot_mock_db()

  # Lösche die verdächtigen Einträge aus der Mock-Datenbank
  DBI::dbExecute(con, "DELETE FROM creators WHERE creatorID IN (7, 8, 9)")

  # Jetzt sollte die Funktion nichts mehr finden
  expect_message(
    clean_matches <- zot_find_institutions(con),
    "No misclassified institutional authors found"
  )
  expect_equal(nrow(clean_matches), 0)

  DBI::dbDisconnect(con)
})

# ------------------------------------------------------------------------------
# Tests für das Fixing (zot_fix_institutions)
# ------------------------------------------------------------------------------

test_that("zot_fix_institutions correctly merges names and updates fieldMode", {
  con <- zot_mock_db()

  # IDs zu fixen: 7 (World Health + Organization)
  target_id <- 7

  # Vorher checken:
  before <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID == target_id) |>
    dplyr::collect()
  expect_equal(before$fieldMode[1], 0)
  expect_equal(before$firstName[1], "World Health")
  expect_equal(before$lastName[1], "Organization")

  # Ausführen (non-interactive überspringt das readline-Prompt, was für Tests perfekt ist)
  expect_invisible(zot_fix_institutions(con, creator_ids = target_id))

  # Nachher checken:
  after <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID == target_id) |>
    dplyr::collect()

  # fieldMode muss nun 1 sein
  expect_equal(after$fieldMode[1], 1)
  # firstName muss leer sein
  expect_equal(after$firstName[1], "")
  # lastName muss Vor- und Nachname zusammen enthalten
  expect_equal(after$lastName[1], "World Health Organization")

  DBI::dbDisconnect(con)
})

test_that("zot_fix_institutions fails gracefully on invalid inputs", {
  con <- zot_mock_db()

  # Leerer Vektor übergeben
  expect_error(
    zot_fix_institutions(con, creator_ids = numeric(0)),
    "You must provide at least one"
  )

  # IDs, die nicht in der Datenbank existieren
  expect_error(
    zot_fix_institutions(con, creator_ids = c(9999, 10000)),
    "None of the provided .* were found in the database"
  )

  DBI::dbDisconnect(con)
})
