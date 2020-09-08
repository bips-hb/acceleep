# Starting "fresh" with lower resolution data
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/convnet"))

# A single ad hoc training run -----
training_run(
  file = here::here("modelling/train_convnet.R"),
  flags = list(
    accel_model = "activpal",
    placement = "thigh_right",
    res = 20,
    # accel_model = "geneactiv",
    # placement = "hip_right",
    # res = 100,
    outcome = "kJ",
    lr = 1e-3,
    decay = 0,
    batch_size = 32,
    epochs = 50,
    batch_normalize = TRUE,
    conv1d_layers = 2,
    # conv1d_filters = 64,
    conv1d_filters_1 = 64,
    conv1d_filters_2 = 64,
    #conv1d_filters_3 = 32,
    # conv1d_kernel_size = 18,
    conv1d_kernel_size_1 = 50,
    conv1d_kernel_size_2 = 50,
    #conv1d_kernel_size_3 = 10,
    conv1d_pool_size = 10,
    conv1d_reduction = "maxpooling",
    dense_layers = 2,
    # dense_units = 32,
    dense_units_1 = 64,
    dense_units_2 = 32,
    dropout_rate = 0.2,
    weight_decay_conv = 0.01,
    # weight_decay_dense = 0.05,
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
  file = here::here("modelling/train_convnet.R"),
  flags = list(
    accel_model = "geneactiv",
    placement = "hip_right",
    outcome = "kJ",
    res = c(50, 100),
    lr = 1e-3,
    decay = 0, # 0.01,
    batch_size = 32,
    epochs = 50,
    batch_normalize = TRUE,
    conv1d_layers = 2,
    conv1d_filters = 64,
    conv1d_kernel_size = 18,
    conv1d_pool_size = c(2, 5, 10),
    conv1d_reduction = c("maxpooling", "flatten"),
    dense_layers = 2,
    dense_units = 64,
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

# Just the latest
view_run(ls_runs(latest_n = 1))
compare_runs(ls_runs(latest_n = 2))



runs <- ingest_runs()

runs %>%
  slice_min(rmse, n = 1) %>%
  plot_loss_history()

runs %>%
  slice_min(rmse, n = 1) %>%
  view_run()

view_run("2020-08-04T12-38-18Z")

# Cleanup
clean_runs(ls_runs(metric_val_loss > 6))
clean_runs(ls_runs(!completed))
