# Starting "fresh" with lower resolution data
# Main model script: "train_lstm.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/downsampled-ad-hoc"))

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
    epochs = 100,
    lstm_layers = 2,
    lstm_units = 256,
    dense_layers = 2,
    dense_units = 64,
    dropout_rate = 0.1,
    validation_split = 0.2,
    verbose = 1
))

# Now with multiple flags ----
#options(tfruns.runs_dir = here::here("output/runs/downsampled-tuning"))

tuning_runs <- tuning_run(
  confirm = FALSE,
  here::here("modelling/train_lstm.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = c(50, 10, 1),
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

view_run("2020-07-24T14-03-07Z")

runs <- ls_runs()

View(runs)

copy_run_files("2020-07-24T14-03-07Z", to = here::here("output/runs", "selected-runs"))


###

options(tfruns.runs_dir = here::here("output/runs/downsampled-1hz-tuning"))
View(ls_runs())

clean_runs(ls_runs(completed == FALSE))

view_run(ls_runs(metric_val_loss == min(metric_val_loss)))


runs <- ingest_runs()

