# ==============================================================================
# zotcleaner: Unit Tests for Title Cleaning
# ==============================================================================

library(testthat)
library(DBI)
library(dplyr)

# ------------------------------------------------------------------------------
# Tests für die interne Smart-Casing-Funktion
# ------------------------------------------------------------------------------

test_that("zot_apply_smart_casing handles basic Sentence Case and Title Case", {
  # Basic lowercase to Sentence case
  expect_equal(
    zot_apply_smart_casing("a practical guide to statistics", "sentence"),
    "A practical guide to statistics"
  )

  # Basic lowercase to Title case (small words like 'a', 'to' stay lower)
  expect_equal(
    zot_apply_smart_casing("a practical guide to statistics", "title"),
    "A Practical Guide to Statistics"
  )

  # ALL CAPS to Sentence case
  expect_equal(
    zot_apply_smart_casing("THE EFFICACY OF SLEEP STUDIES", "sentence"),
    "The efficacy of sleep studies"
  )

  # ALL CAPS to Title case
  expect_equal(
    zot_apply_smart_casing("THE EFFICACY OF SLEEP STUDIES", "title"),
    "The Efficacy of Sleep Studies"
  )
})

test_that("zot_apply_smart_casing preserves internal capitals and acronyms", {
  # Mixed case input with acronyms should be preserved
  expect_equal(
    zot_apply_smart_casing("the use of CRISPR-Cas9 in mRNA research", "title"),
    "The Use of CRISPR-Cas9 in mRNA Research"
  )

  # Brand names or specific casing
  expect_equal(
    zot_apply_smart_casing("programming on a MacBook", "sentence"),
    "Programming on a MacBook"
  )
})

test_that("zot_apply_smart_casing ignores HTML tags and URLs", {
  # HTML tags should not be capitalized or altered
  expect_equal(
    zot_apply_smart_casing("the role of <i>Arabidopsis</i> in nature", "title"),
    "The Role of <i>Arabidopsis</i> in Nature"
  )

  # URLs should be skipped completely
  expect_equal(
    zot_apply_smart_casing(
      "visit http://example.com for more info",
      "sentence"
    ),
    "Visit http://example.com for more info"
  )
})

test_that("zot_apply_smart_casing applies Gruber hyphenation rules", {
  # "Stand-in" -> Title Case makes it "Stand-In"
  expect_equal(
    zot_apply_smart_casing("stand-in methodology", "title"),
    "Stand-In Methodology"
  )
})

# ------------------------------------------------------------------------------
# Tests für die Datenbank-Bereinigung (zot_clean_titles)
# ------------------------------------------------------------------------------

test_that("zot_clean_titles correctly updates the SQLite database", {
  # Wir nutzen unsere Mock-DB aus zot_mock_db.R
  # Hinweis: In einem echten Paket-Testlauf wird diese durch devtools::test() geladen.
  con <- zot_mock_db()

  # Wir rufen die Funktion auf (durch den Test wird interactive() FALSE sein,
  # also gibt es keinen Prompt, aber die Funktion läuft durch, wenn keine Bestätigung gefordert ist.
  # (Da unser Code bei interactive() == FALSE das readline überspringt, wird direkt gespeichert!)

  expect_invisible(zot_clean_titles(
    con,
    fix_html = TRUE,
    fix_uppercase = TRUE,
    to_case = "sentence"
  ))

  item_data_values_db <- dplyr::tbl(con, "itemDataValues") |> dplyr::collect()

  # Test 1: Wurde UPPERCASE erfolgreich in Sentence Case umgewandelt? (ID 1)
  # Alt: "OBSERVATION OF GRAVITATIONAL WAVES FROM A BINARY BLACK HOLE MERGER"
  val_1 <- item_data_values_db |>
    dplyr::filter(valueID == 1) |>
    dplyr::pull(value)
  expect_equal(
    val_1,
    "Observation of gravitational waves from a binary black hole merger"
  )

  # Test 2: Wurden HTML-Tags entfernt und &amp; konvertiert? (ID 6)
  # Alt: "The role of <i>Arabidopsis thaliana</i> CRY2 in circadian clock regulation &amp; flowering time"
  val_6 <- item_data_values_db |>
    dplyr::filter(valueID == 6) |>
    dplyr::pull(value)
  expect_equal(
    val_6,
    "The role of Arabidopsis thaliana CRY2 in circadian clock regulation & flowering time"
  )

  # Test 3: Saubere Titel wurden nicht angetastet (ID 2)
  # Alt: "A Practical Guide to Relativistic Cosmology and Spacetime Geometry"
  val_2 <- item_data_values_db |>
    dplyr::filter(valueID == 2) |>
    dplyr::pull(value)
  expect_equal(
    val_2,
    "A Practical Guide to Relativistic Cosmology and Spacetime Geometry"
  )

  # DB-Verbindung sauber trennen
  DBI::dbDisconnect(con)
})

test_that("zot_clean_titles returns early if everything is already clean", {
  con <- zot_mock_db()

  # Erster Durchlauf bereinigt alles
  zot_clean_titles(con, fix_html = TRUE, fix_uppercase = TRUE)

  # Zweiter Durchlauf sollte "already clean" erkennen und TRUE zurückgeben
  # Wir fangen die Message via cli ab (cli gibt Nachrichten meist als message aus)
  expect_message(
    zot_clean_titles(con, fix_html = TRUE, fix_uppercase = TRUE),
    "All titles are already clean"
  )

  DBI::dbDisconnect(con)
})
