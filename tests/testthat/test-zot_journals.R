library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests für die Suchfunktion (zot_find_journals)
# ------------------------------------------------------------------------------

test_that("zot_find_journals correctly finds journals and respects case sensitivity", {
  con <- zot_mock_db()

  # Search for "Phys" (case-insensitive by default)
  # In our mock DB, we expect "Physical Review Letters", "Phys Rev Lett", "Phys. Rev. Lett." (IDs 16, 17, 18)
  matches <- zot_find_journals(con, "Phys", ignore_case = TRUE)

  expect_equal(nrow(matches), 3)
  expect_true(all(matches$valueID %in% c(16, 17, 18)))

  # Search for "NATURE" (case-insensitive)
  # Should find "Nature" and "Nature Biotechnology"
  matches_nature <- zot_find_journals(con, "NATURE", ignore_case = TRUE)
  expect_equal(nrow(matches_nature), 2)

  # Search with no matches
  expect_message(
    matches_none <- zot_find_journals(con, "NonExistentJournal"),
    "No journals found matching"
  )
  expect_equal(nrow(matches_none), 0)

  DBI::dbDisconnect(con)
})

# ------------------------------------------------------------------------------
# Tests für das Zusammenführen (zot_merge_journals)
# ------------------------------------------------------------------------------

test_that("zot_merge_journals correctly updates references and deletes orphans", {
  con <- zot_mock_db()

  # Wir mergen die Phys-Journals (IDs 16, 17, 18) auf den vollen Namen (ID 16)
  merge_ids <- c(16, 17, 18)
  target_id <- 16

  # Bevor wir mergen, schauen wir, wie oft die IDs in itemData verknüpft sind
  item_data_before <- dplyr::tbl(con, "itemData") |> dplyr::collect()
  count_before <- sum(item_data_before$valueID %in% merge_ids)

  # Funktion ausführen (sollte unsichtbar TRUE zurückgeben)
  expect_invisible(zot_merge_journals(
    con,
    merge_ids = merge_ids,
    target_id = target_id
  ))

  # 1. Überprüfung der itemData Tabelle (Verknüpfungen)
  item_data_after <- dplyr::tbl(con, "itemData") |> dplyr::collect()

  # Es darf keine Verknüpfung mehr zu den alten IDs 17 oder 18 geben
  expect_false(any(item_data_after$valueID %in% c(17, 18)))

  # ID 16 sollte jetzt die Summe aller vorherigen Verknüpfungen haben
  expect_equal(sum(item_data_after$valueID == 16), count_before)

  # 2. Überprüfung der itemDataValues Tabelle (Stammdaten / Strings)
  item_data_values_db <- dplyr::tbl(con, "itemDataValues") |> dplyr::collect()

  # Die verwaisten Duplikate (17, 18) müssen aus der Datenbank gelöscht sein
  expect_false(any(item_data_values_db$valueID %in% c(17, 18)))

  # Der Master-Eintrag (ID 16) muss weiterhin existieren
  expect_true(any(item_data_values_db$valueID == 16))

  DBI::dbDisconnect(con)
})

test_that("zot_merge_journals fails gracefully on invalid inputs", {
  con <- zot_mock_db()

  # Zu wenige IDs übergeben (< 2)
  expect_error(
    zot_merge_journals(con, merge_ids = c(16), target_id = 16),
    "You must provide at least two valid"
  )

  # target_id ist nicht Teil der merge_ids
  expect_error(
    zot_merge_journals(con, merge_ids = c(16, 17), target_id = 99),
    "must be one of the IDs provided"
  )

  # Fehlende target_id in non-interactive Session
  expect_error(
    zot_merge_journals(con, merge_ids = c(16, 17)),
    "Non-interactive session requires explicitly providing"
  )

  DBI::dbDisconnect(con)
})
