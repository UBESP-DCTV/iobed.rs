library(testthat)
library(checkmate)
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(rms)
options(datadist = "dd")




# Functions -------------------------------------------------------
fix_perc <- function(p, debug = FALSE) {
  parsed <- p |>
    stringr::str_remove("^\\D|\\D$") |>
    stringr::str_remove(" *±.*$") |>
    stringr::str_split("[-–]") # sono due trattini diversi...

  if (debug) return(parsed)

  purrr::map_dbl(parsed, ~mean(as.double(.x)))
}




# Tests -----------------------------------------------------------
with_reporter(check_reporter(), { #default_reporter()

  # with context() you can define general test headers
  context("iobed sr")

  # test_that() enclose (sets) of supposed single-purpose tests
  test_that("fix_perc works", {
    # setup
    comparators <- c("<1.1", "< 1.1", ">1.1", "> 1.1")
    ranges <- c("1.2-2.3", "94.5–98.9") # sono due trattini diversi!!
    msd <- c("1.2 ± 2.3")
    leading_symbols <- c("64.95-1.95-")

    # evaluation
    result_comparators <- fix_perc(comparators)
    result_ranges <- fix_perc(ranges)
    result_msd <- fix_perc(msd)
    result_leading_symbols <- fix_perc(leading_symbols)

    # tests
    result_comparators |> expect_equal(c(1.1, 1.1, 1.1, 1.1))
    result_ranges |> expect_equal(
      c(
        mean(c(1.2, 2.3)),
        mean(c(94.5, 98.9))
      )
    )
    result_msd |> expect_equal(1.2)
    result_leading_symbols |> expect_equal(mean(c(64.95, 1.95)))
  })

})




# Data ------------------------------------------------------------
iobed <- here("data-raw/db_long_iobed_rev.xlsx") |>
  read_xlsx() |>
  clean_names() |>
  remove_empty() |>
  remove_constant(na.rm = TRUE) |>
  mutate(acc = fix_perc(accuracy_percent) / 100)

# View(iobed)
# glimpse(iobed)
# iobed$accuracy_percent |> table(useNA = "always")
# iobed$acc |> table(useNA = "always")
# iobed$ml_type |> table(useNA = "always")



# analisi ---------------------------------------------------------
dd <- datadist(iobed)

acc_mod <- Glm(
  acc ~ ml_type + input_data_3 + output_data,
  data = iobed,
  family = "quasibinomial",
  x = TRUE, y = TRUE, model = TRUE
)
acc_mod
anova(acc_mod)
summary(acc_mod)
ggplot(Predict(acc_mod, fun = plogis))
plot(nomogram(acc_mod, fun = plogis))




iobed_collapsed <- iobed |>
  dplyr::filter(
    output_data == "in-bed pose estimation",
    input_data_3 != "other",
    acc > 0.5,
    acc < 1
  ) |>
  dplyr::mutate(
    ml_type = if_else(ml_type == "AE", "SEQ", ml_type),
    input_data_3 = if_else(
      str_detect(input_data_3, "pressure"),
      "pressure",
      input_data_3
    )
  ) |>
  remove_empty("cols") |>
  remove_constant(na.rm = TRUE)

dd <- datadist(iobed_collapsed)
acc_mod_collapsed <- Glm(
  acc ~ ml_type + input_data_3,
  data = iobed_collapsed,
  family = "quasibinomial",
  x = TRUE, y = TRUE, model = TRUE
)

acc_mod_collapsed
anova(acc_mod_collapsed)
summary(acc_mod_collapsed)
ggplot(Predict(acc_mod_collapsed, fun = plogis))
plot(nomogram(acc_mod_collapsed, fun = plogis))


iobed_collapsed |>
  ggplot(aes(ml_type, acc, fill = input_data_3, colour = input_data_3)) +
  geom_jitter() +
  geom_violin(
    aes(ml_type, acc),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  geom_violin(alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "Distribuzione accuratezze per tipo di tecnica per le analisi",
    subtitle = "Stratificazione considerata (colori) per tipologia di input utilizzati (grigio per non stratificato)\n(distribuzioni riportate solo con più di due osservzioni presenti)",
    x = "Tipologia modello",
    y = "Accuratezza",
    colour = "Tipologia di input: ",
    fill = "Tipologia di input: "
  )











iobed_pressure_nonsleep <- iobed |>
  filter(
    input_data_3 != "acceleration data",
    output_data != "sleep"
  )
iobed_pressure_nonsleep$output_data


dd <- datadist(iobed_pressure_nonsleep)
acc_mod_pns <- Glm(
  acc ~ ml_type + input_data_3 + output_data,
  data = iobed_pressure_nonsleep,
  family = "quasibinomial",
  x = TRUE, y = TRUE, model = TRUE
)
acc_mod_pns
anova(acc_mod_pns)
summary(acc_mod_pns)
ggplot(Predict(acc_mod_pns, fun = plogis))
plot(nomogram(acc_mod_pns, fun = plogis))
