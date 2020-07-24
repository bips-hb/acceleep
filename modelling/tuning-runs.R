#! /usr/bin/env Rscript

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/tfruns-overnight"))

tuning_run(
  confirm = FALSE,
  file = here::here("modelling/train_model.R"),
  flags = list(
    lstm_layers = c(1, 2),
    lstm_units = c(128, 256),
    dropout_rate = c(0, 0.2),
    batch_size = c(8, 16),
    decay = c(0, 0.001, 0.01),
    epochs = 30,
    shuffle = c(TRUE, FALSE),
    dense_units = c(0, 1) # <- well this was dumb.
))

pushoverr::pushover("Modelling done!", title = "Modelling hell")
