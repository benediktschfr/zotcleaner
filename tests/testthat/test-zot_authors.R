# ==============================================================================
# zotcleaner: Unit Tests for Author Search and Merging
# ==============================================================================

library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests für die Suchfunktion (zot_find_authors)
# ------------------------------------------------------------------------------

test_that("zot_find_authors correctly finds authors and respects case sensitivity", {
  con <- zot_mock_db()

  # Search for "Harrison" (case-insensitive by default)
  matches <- zot_find_authors(con, "harrison", ignore_case = TRUE)

  # We expect 4 different variations of Harrison in our mock DB (IDs 1, 2, 3, 4)
  expect_equal(nrow(matches), 4)
  expect_true(all(matches$lastName == "Harrison"))

  # Search for "EINSTEIN" (case-insensitive)
  matches_einstein <- zot_find_authors(con, "EINSTEIN", ignore_case = TRUE)
  expect_equal(nrow(matches_einstein), 1)
  expect_equal(matches_einstein$firstName[1], "Albert")

  # Search with no matches
  expect_message(
    matches_none <- zot_find_authors(con, "NonExistentAuthor"),
    "No authors found matching"
  )
  expect_equal(nrow(matches_none), 0)

  DBI::dbDisconnect(con)
})

# ------------------------------------------------------------------------------
# Tests für das Zusammenführen (zot_merge_authors)
# ------------------------------------------------------------------------------

test_that("zot_merge_authors correctly updates references and deletes orphans", {
  con <- zot_mock_db()

  # Wir wollen alle Harrison-IDs (1, 2, 3, 4) auf die saubere ID 3 (Andrew P.) mergen
  merge_ids <- c(1, 2, 3, 4)
  target_id <- 3

  # Funktion ausführen (sollte unsichtbar TRUE zurückgeben)
  expect_invisible(zot_merge_authors(
    con,
    merge_ids = merge_ids,
    target_id = target_id
  ))

  # 1. Überprüfung der itemCreators Tabelle (Verknüpfungen)
  item_creators_db <- dplyr::tbl(con, "itemCreators") |> dplyr::collect()

  # Es darf keine Verknüpfung mehr zu den alten IDs 1, 2 oder 4 geben
  expect_false(any(item_creators_db$creatorID %in% c(1, 2, 4)))

  # ID 3 sollte jetzt 4 mal in der itemCreators Tabelle auftauchen
  expect_equal(sum(item_creators_db$creatorID == 3), 4)

  # 2. Überprüfung der creators Tabelle (Stammdaten)
  creators_db <- dplyr::tbl(con, "creators") |> dplyr::collect()

  # Die verwaisten Duplikate (1, 2, 4) müssen aus der Datenbank gelöscht sein
  expect_false(any(creators_db$creatorID %in% c(1, 2, 4)))

  # Der Master-Eintrag (ID 3) muss weiterhin existieren
  expect_true(any(creators_db$creatorID == 3))

  DBI::dbDisconnect(con)
})

test_that("zot_merge_authors fails gracefully on invalid inputs", {
  con <- zot_mock_db()

  # Zu wenige IDs übergeben (< 2)
  expect_error(
    zot_merge_authors(con, merge_ids = c(1), target_id = 1),
    "You must provide at least two valid"
  )

  # target_id ist nicht Teil der merge_ids
  expect_error(
    zot_merge_authors(con, merge_ids = c(1, 2), target_id = 3),
    "must be one of the IDs provided"
  )

  # Fehlende target_id in non-interactive Session
  # (testthat läuft by default non-interactive, daher greift der Schutzmechanismus)
  expect_error(
    zot_merge_authors(con, merge_ids = c(1, 2)),
    "Non-interactive session requires explicitly providing"
  )

  DBI::dbDisconnect(con)
})
