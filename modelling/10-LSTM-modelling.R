# Starting "fresh" with lower resolution data
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
    lstm_units = 256,
    dense_layers = 2,
    dense_units = 64,
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
    batch_size = 4,
    epochs = 150,
    lstm_layers = 4,
    lstm_units = 256,
    dense_layers = 2,
    dense_units = 64,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1
  ))
pushoverr::pushover("Runs are done!", title = "Modelling Hell")

compare_runs(ls_runs(latest_n = 2))

###

View(ls_runs())

clean_runs(ls_runs(completed == FALSE))

view_run(ls_runs(metric_val_loss == min(metric_val_loss)))


runs <- ingest_runs()

