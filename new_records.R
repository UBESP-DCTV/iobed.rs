
# Packages --------------------------------------------------------

library(tidyverse)

# Functions -------------------------------------------------------

data_path <- file.path(
  "C:", "Users", "corrado.lanera",
  "Unit of Biostatistics Epidemiology and Public Health",
  "LAIMS - IOBED",
  "revisione", "exports"
)


# Tests -----------------------------------------------------------

library(testthat)
library(checkmate)

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


})


# Code/Analyses ---------------------------------------------------

## For every folder get all its files

ris_files <- data_path |>
  list.dirs(recursive = FALSE) |>
  set_names() |>
  map(list.files, full.names = TRUE) |>
  map(set_names, basename)
