# Trying to make LSTM models that work
# Main model script: "train_lstm.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/lstm-models"))

# A single ad hoc training run -----
training_run(
  file = here::here("modelling/train_lstm.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = 1,
    lr = 1e-5,
    decay = 0,
    batch_size = 32,
    batch_normalize = FALSE,
    epochs = 100,
    lstm_layers = 2,
    # lstm_units = 256,
    lstm_units_1 = 256,
    lstm_units_2 = 256,
    dense_layers = 2,
    # dense_units = 64,
    dense_units_1 = 64,
    dense_units_2 = 64,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1
))

# Now with multiple flags ----
tuning_runs <- tuning_run(
  confirm = FALSE,
  here::here("modelling/train_lstm.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = 1,
    lr = 1e-5,
    decay = 0,
    batch_size = 32,
    batch_normalize = FALSE,
    epochs = 100,
    lstm_layers = 2,
    # lstm_units = 256,
    lstm_units_1 = 256,
    lstm_units_2 = c(128, 256),
    dense_layers = 2,
    # dense_units = 64,
    dense_units_1 = c(64, 128),
    dense_units_2 = 64,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1
  ))

compare_runs(ls_runs(latest_n = 2))

view_run(ls_runs(metric_val_loss == min(metric_val_loss)))

runs <- ingest_runs()

runs %>%
  arrange(rmse) %>%
  head(1) %>%
  glimpse()
