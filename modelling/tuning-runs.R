#! /usr/bin/env Rscript

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/downsampled-tuning"))

tuning_run(
  confirm = FALSE,
  file = here::here("modelling/train_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = 1,
    lr = 1e-6,
    decay = 0,
    batch_size = 8,
    epochs = 500,
    lstm_layers = 2,
    lstm_units = 256,
    dense_layers = 2,
    dense_units = 64,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 0
))

pushoverr::pushover("Modelling done!", title = "Workstation")
