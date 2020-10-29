#! /usr/bin/env Rscript
# Training final model on full data
library(dplyr)
library(acceleep)
library(keras)
library(cli)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()
reticulate::dict(python = "says okay")
tick <- Sys.time()

# Declaring metadata ----
model_kind <- "CNN"
run_start <- format(tick, '%Y%m%d%H%M%S')
cli::cli_alert_info("Starting final {model_kind} fit on {run_start}")

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = ifelse(model == "activpal", 20, 100))

# Get the median number of epochs required during full-sample LOSO-CV
# Use that number of epochs (per model/placement/unit) for training here
# as there is no validation error to track
# (rough approximation but presumably better than overfitting?)

metadata <- readRDS(here::here("output", "results", "full_cv_results.rds")) %>%
  filter(model_kind == "CNN") %>%
  tidyr::unnest(data) %>%
  group_by(model, placement, outcome_unit) %>%
  summarize(
    epochs_target = round(median(epochs_completed)),
    .groups = "drop"
  ) %>%
  left_join(metadata, by = c("model" = "model", "placement" = "placement", "outcome_unit" = "outcome")) %>%
  rename(outcome = outcome_unit)


# Big loop over accelerometers, placements, outcomes
for (row in seq_len(nrow(metadata))) {

  # hold current metadata
  metaparams <- metadata[row, ]
  cli::cli_alert_info("Starting {model_kind} on {metaparams$model} ({metaparams$placement}) / {metaparams$outcome}")
  # browser()

  # Collecting data
  c(c(train_data, train_labels), c(., .)) %<-% keras_prep_lstm(
    model = metaparams$model, placement = metaparams$placement,
    outcome = metaparams$outcome, random_seed = 19283, val_split = 0,
    interval_length = 30, normalize = TRUE,
    res = metaparams$res
  )


  # Check
  unique(train_data$ID)

  # Reshaping to array form, keep train_data_full for later prediction
  train_data_array <- keras_reshape_accel(
    accel_tbl = train_data, interval_length = 30, res = metaparams$res
  )

  dim(train_data)
  dim(train_data_array)

  length(train_labels)

  # Modelling ----

  strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

  model_note <- "CF256K20-MP10-CF128K10-GMP-D64-D32-BN-E50-ES-Padding_Same"
  model_tick <- Sys.time()

  with(strategy$scope(), {
    model <- keras_model_sequential() %>%
      # Conv 1
      layer_conv_1d(
        name = "Conv1-F256K20-L2",
        filters = 256, kernel_size = 20, activation = "relu",
        kernel_regularizer = regularizer_l2(l = 0.01),
        input_shape = dim(train_data_array)[c(2, 3)],
        padding = "same"
      )  %>%
      layer_batch_normalization() %>%
      # MaxPooling 1
      layer_max_pooling_1d(name = "MaxPooling1D-10", pool_size = 10) %>%
      # Conv 2
      layer_conv_1d(
        name = "Conv2-F128K10-L2",
        filters = 128, kernel_size = 10, activation = "relu",
        kernel_regularizer = regularizer_l2(l = 0.01),
        padding = "same"
      )  %>%
      layer_batch_normalization() %>%
      # Global Max Pooling
      layer_global_max_pooling_1d(name = "GlobalMaxPooling1D") %>%
      # Dense 1
      layer_dense(name = "Dense1-64", activation = "relu", units = 64)  %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.2)  %>%
      # Dense 2
      layer_dense(name = "Dense2-32", activation = "relu", units = 32)  %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.2)  %>%
      # Output
      layer_dense(units = 1, name = "output", activation = "linear")
  })

  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 1e-3)
  )

  history <- model %>% fit(
    train_data_array,
    train_labels,
    batch_size = 16,
    epochs = metaparams$epochs_target,
    validation_split = 0,
    verbose = as.numeric(interactive())
  )

  model_tock <- Sys.time()
  model_took <- hms::hms(seconds = round(as.numeric(difftime(model_tock, model_tick, units = "secs"))))

  # Save per-device/outcome model
  out_dir_models <- here::here("output", "models")
  if (!fs::dir_exists(out_dir_models)) fs::dir_create(out_dir_models)
  filename_model <- glue::glue("final-{model_kind}-{metaparams$res}Hz-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{run_start}.hdf5")
  save_model_hdf5(model, filepath = fs::path(out_dir_models, filename_model))

}

tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
cli::cli_alert_info("Took {took}")

cuda_close_device()
