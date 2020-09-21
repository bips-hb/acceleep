#! /usr/bin/env Rscript
library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()
reticulate::dict(python = "says okay")

tick <- Sys.time()
# Declaring metadata ----
model_kind <- "CNN"
run_start <- format(tick, '%Y%m%d%H%M%S')
cliapp::cli_alert_info("Starting {model_kind} LOSO-CV on {run_start}")

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = ifelse(model == "activpal", 20, 100))

# Make a progress bar
prog <- cli_progress_bar(
  format = ":current of :total [:bar] (:percent, :elapsedfull)",
  total = nrow(metadata)
)

# Initialize empty tibble to collect results
eval_result <- tibble::tibble()

# Big loop over accelerometers, placements, outcomes
for (row in seq_len(nrow(metadata))) {
  # hold current metadata
  metaparams <- metadata[row, ]
  cliapp::cli_alert_info("Starting {model_kind} on {metaparams$model} ({metaparams$placement}) / {metaparams$outcome}")
  # browser()

  # Collecting data
  c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
    model = metaparams$model, placement = metaparams$placement,
    outcome = metaparams$outcome, random_seed = 19283, val_split = 1/3,
    interval_length = 30, normalize = TRUE,
    res = metaparams$res
  )

  # Check
  unique(train_data$ID)
  unique(test_data$ID)

  # Reshaping to array form, keep train_data_full for later prediction
  train_data_array <- keras_reshape_accel(
    accel_tbl = train_data, interval_length = 30, res = metaparams$res
  )

  test_data_array <- keras_reshape_accel(
    accel_tbl = test_data, interval_length = 30, res = metaparams$res
  )

  dim(train_data)
  dim(train_data_array)

  dim(test_data)
  dim(test_data_array)

  length(train_labels)
  length(test_labels)

  # Modelling ----

  strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

  model_note <- "CF256K20-MP10-CF128K10-GMP-D64-D32-BN-E50-ES"
  model_tick <- Sys.time()

  with(strategy$scope(), {
    model <- keras_model_sequential() %>%
      # Conv 1
      layer_conv_1d(
        name = "Conv1-F256K20-L2",
        filters = 256, kernel_size = 20, activation = "relu",
        kernel_regularizer = regularizer_l2(l = 0.01),
        input_shape = dim(train_data_array)[c(2, 3)]
      )  %>%
      layer_batch_normalization() %>%
      # MaxPooling 1
      layer_max_pooling_1d(name = "MaxPooling1D-10", pool_size = 10) %>%
      # Conv 2
      layer_conv_1d(
        name = "Conv2-F128K10-L2",
        filters = 128, kernel_size = 10, activation = "relu",
        kernel_regularizer = regularizer_l2(l = 0.01)
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
      # Dense 3
      # layer_dense(name = "Dense3-64", activation = "relu", units = 32) %>%
      # layer_batch_normalization() %>%
      # layer_dropout(rate = 0.2) %>%
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
    epochs = 50,
    validation_split = 0,
    # Uncomment the following to monitor validation error during training w/ verbose = 1
    validation_data =
      list(
        test_data_array,
        test_labels
      ),
    verbose = 0,
    callbacks =
      list(
        callback_early_stopping(
          monitor = "val_loss",
          min_delta = 0.1,
          patience = 10,
          mode = "min",
          restore_best_weights = TRUE
        )
      )
  )

  # Evaluate, save results
  eval_result <- model %>%
    evaluate(test_data_array, test_labels, verbose = 0)

  # Make predictions
  predicted_obs <- test_data %>%
    select(ID, interval, outcome = metaparams$outcome) %>%
    distinct() %>%
    mutate(predicted = as.numeric(predict(model, test_data_array)))

  # prediction rmse differs from result of evaluate() o_O
  prediction_rmse <- predicted_obs %>%
    summarize(rmse = sqrt(mean((predicted - outcome)^2))) %>%
    pull(rmse)

  model_tock <- Sys.time()
  model_took <- hms::hms(seconds = round(as.numeric(difftime(model_tock, model_tick, units = "secs"))))

  eval_result <- tibble::tibble(
    rmse = prediction_rmse,
    eval_rmse = sqrt(eval_result[["loss"]]),
    predicted_obs = list(predicted_obs),
    model_note = model_note,
    model_took = model_took,
    epochs_completed = length(history$metrics$loss)
  )

  # Save per-device model
  out_dir_models <- here::here("output", "holdout-validation", model_kind, run_start, "models")
  if (!fs::dir_exists(out_dir_models)) fs::dir_create(out_dir_models)
  filename_model <- glue::glue("holdout-eval-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.hdf5")
  save_model_hdf5(model, filepath = fs::path(out_dir_models, filename_model))


  # Save result tibble
  filename <- glue::glue("holdout-eval-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.rds")

  out_dir <- here::here("output", "holdout-validation", model_kind, run_start)
  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # Save CV RMSE results
  saveRDS(object = eval_result, file = fs::path(out_dir, filename))


  # Write model structure to plain text file
  capture.output(summary(model), file =  fs::path(out_dir, fs::path_ext_set(filename, "txt")))
}

tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
pushoverr::pushover(glue::glue("{model_kind} holdout validation is done! Took {took}"), title = "Modelling Hell", priority = 1)

cuda_close_device()
