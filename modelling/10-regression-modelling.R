# Starting "fresh" with lower resolution data
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/regression-model"))

# Default flags for easier copypasting ----
# list(
#   accel_model = "geneactiv",
#   placement = "hip_right",
#   outcome = "kJ",
#   lr = 0.001,
#   decay = 0,
#   batch_size = 32,
#   epochs = 20,
#   dense_layers = 2,
#   dense_units = 128,
#   droput_rate = 0,
#   validation_split = 0.2,
#   verbose = 0
# )

# A single ad hoc training run -----

training_run(
  file = here::here("modelling/train_regression_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    lr = 1e-5,
    decay = 0,
    batch_size = 32,
    epochs = 50,
    dense_layers = 2,
    dense_units = 128,
    dropout_rate = 0, # 0.2,
    validation_split = 0.2,
    verbose = 1
  ))

# Now with multiple flags ----
tuning_runs <- tuning_run(
  confirm = FALSE,
  file = here::here("modelling/train_regression_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    lr = 1e-5,
    decay = 0,
    batch_size = 32,
    epochs = 50,
    dense_layers = 2,
    dense_units = 128,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 0
  ))
pushoverr::pushover("Runs are done!", title = "Modelling Hell")
