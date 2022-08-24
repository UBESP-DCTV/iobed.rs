
# Packages --------------------------------------------------------

library(checkmate)
library(testthat)
library(tidyverse)

# Functions -------------------------------------------------------

data_path <- file.path(
  "C:", "Users", "corra",
  "Unit of Biostatistics Epidemiology and Public Health",
  "LAIMS - IOBED",
  "revisione", "exports"
)

fix_names <- function(x) {
  x |>
    str_replace(
      "((.*/)*)(\\d).*[iI][eE]{3}.*$",
      "\\2ieee-\\3\\.ris"
    )
}


# Tests -----------------------------------------------------------

# with_reporter() enclosse in an interactive session all the testing
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
    expected_dirs <- c("04-08-2022", "marzo_2022")

    # evaluation
    obtained_dirs <- data_path |>
      list.dirs(full.names = FALSE, recursive = FALSE)

    # tests
    expect_subset(expected_dirs, obtained_dirs)
  })


  test_that("fix_names fixes the names", {
    # setup
    ieee_1 <- "1-IEEE Xplore Citation Download 2022.08.04.09.23.23.ris"
    expected_ieee_1 <- "ieee-1.ris"
    ieee_1_path <- "path/to/1-IEEE Xplore Citation Download 2022.08.04.09.23.23.ris"

    ieee_2 <- "2 - IEEE Xplore Citation Download 2022.08.04.09.24.09.ris"
    expected_ieee_2 <- "ieee-2.ris"

    ieee_3 <- "3- IEEE Xplore Citation Download 2022.08.04.09.24.54.ris"
    expected_ieee_3 <- "ieee-3.ris"


    acm <- "acm.ris"
    chinhal <- "chinhal.ris"
    cochrane <- "cochrane.ris"
    embase <- "embase.ris"
    pubmed <- "pubmed.txt"
    scopus <- "scopus.ris"
    wos <- "wos.ris"
    
    # evaluate
    res_ieee_1 <- fix_names(ieee_1) |> basename()
    res_ieee_1_path <- fix_names(ieee_1_path) |> basename()

    res_ieee_2 <- fix_names(ieee_2) |> basename()
    res_ieee_3 <- fix_names(ieee_3) |> basename()

    # tests
    expect_true(str_detect(ieee_1, "(.*/)*(\\d).*[iI][eE]{3}.*$"))
    res_ieee_1 |>  expect_equal(expected_ieee_1)
    expect_identical(res_ieee_1, res_ieee_1_path)

    res_ieee_2 |>  expect_equal(expected_ieee_2)
    res_ieee_3 |>  expect_equal(expected_ieee_3)

  })

})


# Code/Analyses ---------------------------------------------------

## For every folder get all its files

ris_files <- data_path |>
  list.dirs(recursive = FALSE) |>
  set_names() |>
  map(list.files, full.names = TRUE) |>
  map(set_names, basename)
