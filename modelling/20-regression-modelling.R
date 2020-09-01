# Starting "fresh" with lower resolution data
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/regression-model"))

# A single ad hoc training run -----
training_run(
  file = here::here("modelling/train_regression_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    lr = 1e-3,
    decay = 0, # 0.01,
    batch_normalize = TRUE,
    batch_size = 32,
    epochs = 100,
    dense_layers = 4,
    dense_units_1 = 256,
    dense_units_2 = 128,
    dense_units_3 = 64,
    dense_units_4 = 32,
    # dense_units_5 = 256,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1,
    callback_reduce_lr = FALSE,
    callback_reduce_lr_patience = 5,
    callback_reduce_lr_factor = 0.5,
    callback_reduce_lr_min_delta = 0.05
  ))
pushoverr::pushover("Runs are done!", title = "Modelling Hell")

# Now with multiple flags ----
tuning_runs <- tuning_run(
  confirm = FALSE,
  file = here::here("modelling/train_regression_model.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    lr = 1e-3,
    decay = 0, # 0.01,
    batch_normalize = FALSE,
    batch_size = 32,
    epochs = 100,
    dense_layers = 3,
    dense_units_1 = 128,
    dense_units_2 = 64,
    dense_units_3 = 32,
    # dense_units_4 = 32,
    # dense_units_5 = 256,
    dropout_rate = 0.2,
    validation_split = 0.2,
    verbose = 1,
    callback_reduce_lr = FALSE,
    callback_reduce_lr_patience = 5,
    callback_reduce_lr_factor = 0.5,
    callback_reduce_lr_min_delta = 0.05
  ))
pushoverr::pushover("Runs are done!", title = "Modelling Hell")

# Quick look at runs ----
runs <- ingest_runs()

runs %>%
  slice_min(rmse, n = 1) %>%
  plot_loss_history()

compare_runs()
