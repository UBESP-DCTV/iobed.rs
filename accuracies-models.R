library(testthat)
library(checkmate)
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(rms)
options(datadist = "dd")


fix_perc <- function(p) {
  p |>
    str_remove_all("< ?")
    # stringr::str_split_fixed("-")
}


with_reporter(default_reporter(), {

  # with context() you can define general test headers
  context("Sums")

  # test_that() enclose (sets) of supposed single-purpose tests
  test_that("my_sum works", {
    # setup
    one <- 1
    two <- 2

    # evaluation
    result <- my_sum(one, two)
    result_na <- my_sum(one, two, NA)
    result_narm <- my_sum(one, two, NA, na.rm = TRUE)

    # tests
    expected <- 3
    result |> expect_equal(expected)
    result_na |> expect_equal(NA_real_)
    result_narm |> expect_equal(expected)
  })


})



# analisi ---------------------------------------------------------



iobed <- here("data-raw/db_long_iobed_rev.xlsx") |>
  read_xlsx() |>
  clean_names() |>
  remove_empty() |>
  remove_constant(na.rm = TRUE) |>
  mutate(
    # accuracy_percent = parse_number(accuracy_percent)
    # acc = accuracy_percent / 100
  )


iobed$accuracy_percent |> table()

glimpse(iobed)

dd <- datadist(iobed)



acc_mod <- Glm(
  accuracy_percent ~ ml_type,
  data = iobed,
  family = "quasibinomial",
  x = TRUE, y = TRUE, model = TRUE
)
