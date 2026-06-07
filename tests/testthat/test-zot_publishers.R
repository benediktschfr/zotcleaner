library(testthat)
library(DBI)
library(dplyr)

# Helper function to inject dummy publisher data into the mock DB
inject_mock_publishers <- function(con) {
  # 1. Add 'publisher' to fields table (assume ID 4)
  DBI::dbExecute(
    con,
    "INSERT INTO fields (fieldID, fieldName) VALUES (4, 'publisher')"
  )

  # 2. Add some messy publisher names to itemDataValues
  DBI::dbExecute(
    con,
    "INSERT INTO itemDataValues (valueID, value) VALUES (101, 'Springer')"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO itemDataValues (valueID, value) VALUES (102, 'Springer Verlag')"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO itemDataValues (valueID, value) VALUES (103, 'Springer-Verlag')"
  )

  # 3. Link them to some items in itemData (Item IDs 102, 111, 105 from our mock)
  DBI::dbExecute(
    con,
    "INSERT INTO itemData (itemID, fieldID, valueID) VALUES (102, 4, 101)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO itemData (itemID, fieldID, valueID) VALUES (111, 4, 102)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO itemData (itemID, fieldID, valueID) VALUES (105, 4, 103)"
  )
}

# ------------------------------------------------------------------------------
# Tests für die Suchfunktion (zot_find_publishers)
# ------------------------------------------------------------------------------

test_that("zot_find_publishers correctly finds publishers", {
  con <- zot_mock_db()
  inject_mock_publishers(con)

  # Search for "Springer"
  matches <- zot_find_publishers(con, "Springer", ignore_case = TRUE)

  expect_equal(nrow(matches), 3)
  expect_true(all(matches$valueID %in% c(101, 102, 103)))

  # Search with no matches
  expect_message(
    matches_none <- zot_find_publishers(con, "Penguin"),
    "No publishers found matching"
  )
  expect_equal(nrow(matches_none), 0)

  DBI::dbDisconnect(con)
})

# ------------------------------------------------------------------------------
# Tests für das Zusammenführen (zot_merge_publishers)
# ------------------------------------------------------------------------------

test_that("zot_merge_publishers correctly updates references and deletes orphans", {
  con <- zot_mock_db()
  inject_mock_publishers(con)

  # Wir mergen alle Springer-Varianten auf "Springer" (ID 101)
  merge_ids <- c(101, 102, 103)
  target_id <- 101

  # Bevor wir mergen, schauen wir, wie oft die IDs in itemData verknüpft sind
  item_data_before <- dplyr::tbl(con, "itemData") |> dplyr::collect()
  count_before <- sum(item_data_before$valueID %in% merge_ids)

  # Funktion ausführen
  expect_invisible(zot_merge_publishers(
    con,
    merge_ids = merge_ids,
    target_id = target_id
  ))

  # 1. Überprüfung der itemData Tabelle
  item_data_after <- dplyr::tbl(con, "itemData") |> dplyr::collect()

  # Keine Verknüpfung mehr zu den alten IDs 102 oder 103
  expect_false(any(item_data_after$valueID %in% c(102, 103)))

  # ID 101 sollte jetzt alle Verknüpfungen haben
  expect_equal(sum(item_data_after$valueID == 101), count_before)

  # 2. Überprüfung der itemDataValues Tabelle
  item_data_values_db <- dplyr::tbl(con, "itemDataValues") |> dplyr::collect()

  # Die verwaisten Duplikate (102, 103) müssen gelöscht sein
  expect_false(any(item_data_values_db$valueID %in% c(102, 103)))

  # Der Master-Eintrag (ID 101) muss existieren
  expect_true(any(item_data_values_db$valueID == 101))

  DBI::dbDisconnect(con)
})

test_that("zot_merge_publishers fails gracefully on invalid inputs", {
  con <- zot_mock_db()
  inject_mock_publishers(con)

  expect_error(
    zot_merge_publishers(con, merge_ids = c(101), target_id = 101),
    "You must provide at least two valid"
  )

  DBI::dbDisconnect(con)
})
