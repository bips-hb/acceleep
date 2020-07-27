library(reticulate)
use_condaenv(condaenv = "acceleep", required = TRUE)
library(keras)
library(tfruns)
library(dplyr)
library(tidyr)

# Downsampled ad hoc ----
runs_dir <- "output/runs/downsampled-ad-hoc"
options(tfruns.runs_dir = here::here(runs_dir))

# tensorboard(runs_dir)

view_run(ls_runs(latest_n = 1))
compare_runs(ls_runs(latest_n = 2))

runs <- ingest_runs(runs_dir) %>%
  filter(completed)

glimpse(runs)

# Viewing single runs -----
runs %>%
  filter(flag_res == 10, flag_dropout_rate > 0) %>%
  slice(1) %>%
  view_run()

# No dropout
runs %>%
  filter(flag_res == 10, flag_dropout_rate == 0) %>%
  slice(2) %>%
  view_run()

runs %>%
  filter(flag_res == 1, flag_dropout_rate == 0) %>%
  plot_loss_history()

# Comparing runs ----
runs %>%
  filter(flag_res == 1, flag_batch_size == 128) %>%
  slice(c(2, 3)) %>%
  compare_runs()

# 128 vs 256 LSTM layers
runs %>%
  filter(
    flag_res == 1, flag_batch_size == 128, flag_lr == 0.0001,
    flag_batch_size %in% c(128, 256)
  ) %>%
  slice(c(1, 2)) %>%
  compare_runs()


# Interesting runs
view_run("output/runs/downsampled-ad-hoc/2020-07-24T13-49-44Z")
view_run("output/runs/downsampled-ad-hoc/2020-07-24T14-26-56Z")

# Downsampled tuning for capacity / batch size ----
runs_dir <- "output/runs/downsampled-1hz-tuning/"
options(tfruns.runs_dir = here::here(runs_dir))

runs <- ingest_runs(runs_dir) %>%
  filter(completed)

glimpse(runs)

# 128 vs 256 LSTM units (but LR too high) ----
runs %>%
  filter(flag_res == 1) %>%
  slice(c(1, 2)) %>%
  compare_runs()

runs %>%
  filter(flag_batch_size == 32) %>%
  slice(c(1, 2)) %>%
  compare_runs()

runs %>%
  filter(flag_batch_size == 16) %>%
  slice(c(1, 2)) %>%
  compare_runs()

# batch size comparison at fixed units ----
runs %>%
  filter(flag_batch_size %in% c(32, 64), flag_lstm_units == 256) %>%
  slice(c(1, 2)) %>%
  compare_runs()

runs %>%
  filter(flag_batch_size %in% c(32, 64), flag_lstm_units == 128) %>%
  slice(c(1, 2)) %>%
  compare_runs()

runs %>%
  filter(flag_batch_size %in% c(16, 32), flag_lstm_units == 128) %>%
  slice(c(1, 2)) %>%
  compare_runs()

runs %>%
  filter(flag_batch_size %in% c(16, 32), flag_lstm_units == 128) %>%
  slice(c(1, 2)) %>%
  plot_loss_history()


###

ls_runs()

