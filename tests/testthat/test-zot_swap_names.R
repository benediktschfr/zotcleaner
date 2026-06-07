library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests for finding swapped names (zot_find_swapped_names)
# ------------------------------------------------------------------------------

test_that("zot_find_swapped_names identifies Asian family names in firstName field", {
  con <- zot_mock_db()

  # In our mock database, ID 10 is "Wang" (first) "Jian" (last)
  # ID 11 is "Zhang" (first) "Ying" (last)
  matches <- zot_find_swapped_names(con)

  expect_equal(nrow(matches), 2)
  expect_true(all(matches$creatorID %in% c(10, 11)))

  # Verify the values before swapping
  expect_equal(matches$firstName[matches$creatorID == 10], "Wang")
  expect_equal(matches$lastName[matches$creatorID == 10], "Jian")

  DBI::dbDisconnect(con)
})

# ------------------------------------------------------------------------------
# Tests for the swapping operation (zot_swap_names)
# ------------------------------------------------------------------------------

test_that("zot_swap_names correctly inverts firstName and lastName", {
  con <- zot_mock_db()

  # Let's swap ID 10 and 11
  target_ids <- c(10, 11)

  # Perform swap (non-interactive automatically skips prompt)
  expect_invisible(zot_swap_names(con, creator_ids = target_ids))

  # Verify changes in DB
  after <- dplyr::tbl(con, "creators") |>
    dplyr::filter(creatorID %in% target_ids) |>
    dplyr::collect() |>
    dplyr::arrange(creatorID)

  # ID 10 should now be Last: Wang, First: Jian
  expect_equal(after$lastName[after$creatorID == 10], "Wang")
  expect_equal(after$firstName[after$creatorID == 10], "Jian")

  # ID 11 should now be Last: Zhang, First: Ying
  expect_equal(after$lastName[after$creatorID == 11], "Zhang")
  expect_equal(after$firstName[after$creatorID == 11], "Ying")

  DBI::dbDisconnect(con)
})

test_that("zot_swap_names fails gracefully on invalid inputs", {
  con <- zot_mock_db()

  # Empty vector
  expect_error(
    zot_swap_names(con, creator_ids = numeric(0)),
    "You must provide at least one"
  )

  # Non-existent IDs
  expect_error(
    zot_swap_names(con, creator_ids = c(888, 999)),
    "None of the provided .* were found in the database"
  )

  DBI::dbDisconnect(con)
})
