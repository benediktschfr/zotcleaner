zot_creators_raw <- tibble::tibble(
  creatorID = 1:20,
  lastName = c(
    "Harrison",
    "Harrison",
    "Harrison",
    "Harrison",
    "Taylor",
    "Taylor",
    "Organization",
    "Laboratory",
    "Collaboration",
    "Jian",
    "Ying",
    "ALEXANDER",
    "nakamura",
    "von weizsecker",
    "Einstein",
    "Curie",
    "Watson",
    "Crick",
    "Turing",
    "Lovelace"
  ),
  firstName = c(
    "A.",
    "Andrew",
    "Andrew P.",
    "A.",
    "S.",
    "Sarah",
    "World Health",
    "European Molecular Biology",
    "LIGO Scientific",
    "Wang",
    "Zhang",
    "Robert",
    "yuki",
    "carl friedrich",
    "Albert",
    "Marie",
    "James",
    "Francis",
    "Alan",
    "Ada"
  ),
  fieldMode = c(rep(0, 6), 0, 0, 0, rep(0, 11))
)

use_data(zot_creators_raw, overwrite = TRUE)

zot_items_raw <- tibble::tibble(
  itemID = 101:115,
  itemTypeID = c(1, 2, 1, 1, 1, 1, 3, 1, 1, 1, 2, 1, 1, 3, 1)
)

use_data(zot_items_raw, overwrite = TRUE)

zot_item_creators_raw <- tibble::tibble(
  itemID = c(
    101,
    101,
    102,
    103,
    103,
    104,
    105,
    105,
    106,
    107,
    107,
    108,
    109,
    110,
    110,
    111,
    112,
    112,
    113,
    114,
    115
  ),
  creatorID = c(
    1,
    16,
    2,
    9,
    5,
    3,
    18,
    19,
    6,
    8,
    20,
    13,
    17,
    7,
    14,
    11,
    12,
    15,
    21,
    4,
    14
  ),
  creatorTypeID = rep(1, 21),
  orderIndex = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0)
)

use_data(zot_item_creators_raw, overwrite = TRUE)

zot_fields_raw <- tibble::tibble(
  fieldID = 1:3,
  fieldName = c("title", "publicationTitle", "date")
)

use_data(zot_fields_raw, overwrite = TRUE)

zot_item_data_values_raw <- tibble::tibble(
  valueID = 1:32,
  value = c(
    "OBSERVATION OF GRAVITATIONAL WAVES FROM A BINARY BLACK HOLE MERGER",
    "A Practical Guide to Relativistic Cosmology and Spacetime Geometry",
    "Multimessenger Observations of a Binary Neutron Star Coalescence",
    "An Introduction to Quantum Field Theory in Curved Spacetime",
    "Molecular Structure of Nucleic Acids: A Structure for Deoxyribose Nucleic Acid",
    "The role of <i>Arabidopsis thaliana</i> CRY2 in circadian clock regulation &amp; flowering time",
    "CRISPR-Cas9 gene editing in <sub>in vivo</sub> models of muscular dystrophy &apos;a breakthrough&apos;",
    "SYNTHESIS AND CHARACTERIZATION OF NOVEL GRAPHENE OXIDE NANOCOMPOSITES",
    "Radioactive properties of Radium and Polonium isotopes",
    "World Health Report on Global Pandemic Preparedness",
    "Clinical study of traditional medicines in East Asian populations",
    "On the Electrodynamics of Moving Bodies",
    "Sketch of the Analytical Engine Invented by Charles Babbage",
    "Computing Machinery and Intelligence",
    "A Method for Obtaining Digital Signatures and Public-Key Cryptosystems",
    "Physical Review Letters",
    "Phys Rev Lett",
    "Phys. Rev. Lett.",
    "Nature",
    "Nature Biotechnology",
    "Nat Biotechnol",
    "Nat. Biotechnol.",
    "Journal of the American Chemical Society",
    "J Am Chem Soc",
    "J. Am. Chem. Soc.",
    "The Lancet",
    "Journal of Virology",
    "2016",
    "1953",
    "2020",
    "1905",
    "1950"
  )
)

use_data(zot_item_data_values_raw, overwrite = TRUE)

zot_item_data_raw <- tibble::tibble(
  itemID = c(
    101:115,
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
    101,
    105,
    106,
    112,
    114
  ),
  fieldID = c(rep(1, 15), rep(2, 12), rep(3, 5)),
  valueID = c(
    1:15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32
  )
)

use_data(zot_item_data_raw, overwrite = TRUE)
