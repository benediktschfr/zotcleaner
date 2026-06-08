library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests for finding publishers (zot_find_publishers)
# ------------------------------------------------------------------------------

test_that("zot_find_publishers correctly finds active publishers", {
  con <- zot_mock_db()

  # Search for "Springer"
  # We expect "Springer" (33) and "Springer-Verlag" (34) since they are linked to items.
  # Note: "Springer Verlag" (35) is in the database but unlinked, so it won't be found.
  matches <- zot_find_publishers(con, "Springer", ignore_case = TRUE)

  expect_equal(nrow(matches), 2)
  expect_true(all(matches$valueID %in% c(33, 34)))

  # Search with no matches
  expect_message(
    matches_none <- zot_find_publishers(con, "Penguin"),
    "No publishers found matching"
  )
  expect_equal(nrow(matches_none), 0)

  zot_disconnect_db(con)
})

# ------------------------------------------------------------------------------
# Tests for merging publishers (zot_merge_publishers)
# ------------------------------------------------------------------------------

test_that("zot_merge_publishers correctly updates references and deletes orphans", {
  con <- zot_mock_db()

  # We merge the linked "Springer-Verlag" (34) and unlinked "Springer Verlag" (35)
  # into the master "Springer" (33)
  merge_ids <- c(33, 34, 35)
  target_id <- 33

  # Before merging, check how many times the IDs are referenced in itemData
  item_data_before <- dplyr::tbl(con, "itemData") |> dplyr::collect()
  count_before <- sum(item_data_before$valueID %in% merge_ids) # Should be 2 (ID 33 and 34)

  # Perform merge
  expect_invisible(zot_merge_publishers(
    con,
    merge_ids = merge_ids,
    target_id = target_id
  ))

  # 1. Verify itemData table link updates
  item_data_after <- dplyr::tbl(con, "itemData") |> dplyr::collect()

  # No references to old duplicate IDs should remain
  expect_false(any(item_data_after$valueID %in% c(34, 35)))

  # The master ID 33 should now hold all references
  expect_equal(sum(item_data_after$valueID == 33), count_before)

  # 2. Verify itemDataValues cleanup
  item_data_values_after <- dplyr::tbl(con, "itemDataValues") |>
    dplyr::collect()

  # Orphaned duplicate string entries should be deleted entirely
  expect_false(any(item_data_values_after$valueID %in% c(34, 35)))

  # Master publisher string entry must still exist
  expect_true(any(item_data_values_after$valueID == 33))

  zot_disconnect_db(con)
})

test_that("zot_merge_publishers fails gracefully on invalid inputs", {
  con <- zot_mock_db()

  # Too few IDs provided (< 2)
  expect_error(
    zot_merge_publishers(con, merge_ids = c(33), target_id = 33),
    "You must provide at least two valid"
  )

  # target_id is not in the merge_ids list
  expect_error(
    zot_merge_publishers(con, merge_ids = c(33, 34), target_id = 99),
    "must be one of the IDs provided"
  )

  zot_disconnect_db(con)
})
