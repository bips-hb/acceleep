# Starting "fresh" with lower resolution data
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/downsampled-ad-hoc"))


# Default flags for easier copypasting ----
# list(
#   accel_model = "geneactiv",
#   placement = "hip_right",
#   outcome = "kJ",
#   res = 1,
#   lr = 0.001,
#   decay = 0,
#   batch_size = 32,
#   epochs = 20,
#   lstm_layers = 2,
#   lstm_units = 128,
#   dense_layers = 2,
#   dense_units = 128,
#   droput_rate = 0,
#   validation_split = 0.2,
#   verbose = 0
# )

# A single ad hoc training run -----
# options(tfruns.runs_dir = here::here("output/runs/downsampled-ad-hoc"))

training_run(
  file = here::here("modelling/train_model.R"),
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
    lstm_units = 128,
    dense_layers = 2,
    dense_units = 128,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1
))

# Now with multiple flags ----
tuning_runs <- tuning_run(
  confirm = FALSE,
  here::here("modelling/train_model.R"),
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
    lstm_units = c(128, 256, 512),
    dense_layers = 2,
    dense_units = 128,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1
  ))
# pushoverr::pushover("Runs are done!", title = "Modelling Hell")

view_run("2020-07-24T14-03-07Z")

runs <- ls_runs()

View(runs)

copy_run_files("2020-07-24T14-03-07Z", to = here::here("output/runs", "selected-runs"))


###

options(tfruns.runs_dir = here::here("output/runs/downsampled-1hz-tuning"))
View(ls_runs())

view_run(ls_runs(metric_val_loss == min(metric_val_loss)))
