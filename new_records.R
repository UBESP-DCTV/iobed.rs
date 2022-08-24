
# Packages --------------------------------------------------------

library(checkmate)
library(testthat)
library(tidyverse)

# Functions -------------------------------------------------------

data_path <- file.path(
  "C:", "Users", "corrado.lanera",
  "Unit of Biostatistics Epidemiology and Public Health",
  "LAIMS - IOBED",
  "revisione", "exports", "merged_csv"
)

set_basename <- function(x) {
  purrr::set_names(x, basename(x))
}

remove_duplicates <- function(x, ...) {
  dupes <- janitor::get_dupes(x, ...)

  suppressMessages(
    dplyr::anti_join(x, dupes)
  )
}


# Tests -----------------------------------------------------------

# with_reporter() enclose in an interactive session all the testing
# environment and machinery of {testthat}.
# You can use default_reporter() for a general summary of your
# tests, or you can use check_reporter() for a detailed reporter
# that highlight error also
with_reporter(default_reporter(), {

  # with context() you can define general test headers
  context("Data")

  # test_that() enclose (sets) of supposed single-purpose tests
  test_that("data_path is correct", {
    # setup
    expected_content <- c("cinahl.marzo.csv", "cinahl.agosto.csv")

    # evaluation
    obtained_content <- list.files(data_path)

    # tests
    expected_content |> expect_subset(obtained_content)
  })

  test_that("set_basename works", {
    # setup
    x <- "path/to/file"

    # evaluation
    res <- set_basename(x)

    # test
    expect_equal(names(res), "file")
  })

  test_that("remove_duplicates works", {
    # setup
    db <- data.frame(a = 1:3, b = c(1, 1, 2))

    # eval
    res <- remove_duplicates(db)
    res_a <- remove_duplicates(db, -a)

    # test
    expect_equal(res, db)
    expect_equal(res_a, data.frame(a = 3, b = 2))
  })

})


# Code/Analyses ---------------------------------------------------

## For every folder get all its files

sr_data <- list.files(data_path, full.names = TRUE) |>
  set_basename() |>
  map(read_csv, show_col_types = FALSE) |>
  map(
    mutate,
    across(
      c(Issue, Date, Volume, Pages, `Num Pages`),
      as.character
    )
  ) |> map_int(nrow)
  bind_rows(.id = "source") |>
  separate(source, c("source", "month"), extra = "drop")

new_data <- sr_data |>
  with_groups(
    source,
    remove_duplicates,
    -source, -month, -Key, -contains("Date"), -where(is.logical),
    -`Abstract Note`, -`Manual Tags`, -Url, -Pages, -Issue, -Volume,
    -Extra, -`Num Pages`, -`Number Of Volumes`, -Number,
    -`Link Attachments`, -`Journal Abbreviation`, -Language,
    -`Publication Year`, -Rights, -Place, -Archive, -`Archive Location`,
    -Edition, -`Library Catalog`, -Series, -`Short Title`, -Type,
    -Editor, -Publisher
  )

new_data |>
  pull(source) |>
  table()



new_data |>
  # filter(source == "ieeex") |>
  select(
    -ISBN, -ISSN, -DOI,
    -Key, -`Abstract Note`, -`Manual Tags`, -Url,
    -`Num Pages`, -`Number Of Volumes`, -Number, -`Link Attachments`,
    -Pages, -Issue, -Volume, -Extra, -`Journal Abbreviation`,
    -Language, -`Publication Year`, -Rights, -Place, -Archive,
    -`Archive Location`, -Edition, -`Library Catalog`, -Series,
    -`Short Title`, -Type, -Editor, -Publisher,
    -contains("Date"), -where(is.logical)
  ) |>
  get_dupes("Title") |>
  janitor::remove_empty() |>
  arrange(Title) |>
  glimpse()

new_data |>
  filter(source != "acm") |>
  write_csv(
    paste0(
      str_remove_all(Sys.time(), "\\D"),
      "-",
      "new_records.csv"
    )
  )
