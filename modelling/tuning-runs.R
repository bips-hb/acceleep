#! /usr/bin/env Rscript

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/downsampled-1hz-tuning"))

tuning_run(
  confirm = FALSE,
  file = here::here("modelling/train_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = c(1, 10),
    lr = c(1e-5),
    lstm_layers = c(2, 3),
    lstm_units = c(128, 256, 512),
    dense_layers = c(2, 3),
    dense_units = c(64, 128, 256),
    dropout_rate = c(0.2, 0.5),
    batch_size = c(32, 64, 128, 256),
    decay = c(0, 1e-6, 1e-7),
    epochs = 200,
    verbose = 0
))

pushoverr::pushover("Modelling done!", title = "Workstation")
