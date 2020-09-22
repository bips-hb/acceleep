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
model_kind <- "RNN"
run_start <- format(tick, '%Y%m%d%H%M%S')
cliapp::cli_alert_info("Starting {model_kind} holdout validation on {run_start}")

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = 1)

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

  model_note <- "LSTM256-LSTM256-D128-D64-E50-LR5-ES"
  model_tick <- Sys.time()

  # with(strategy$scope(), {
  model <- keras_model_sequential() %>%
    # LSTM 1 --
    layer_lstm(
      units = 256, input_shape = dim(train_data_array)[c(2, 3)],
      activation = "tanh", recurrent_activation = "sigmoid",
      recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
      return_sequences = TRUE
    ) %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.2)  %>%
    # LSTM 2 --
    layer_lstm(
      units = 256,
      activation = "tanh", recurrent_activation = "sigmoid",
      recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
      return_sequences = FALSE
    ) %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.2)  %>%
    # Dense 1 --
    layer_dense(activation = "relu", units = 128)  %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.2)  %>%
    # Dense 2 --
    layer_dense(activation = "relu", units = 64) %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, name = "output", activation = "linear")
  # })


  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 1e-5)
  )

  history <- model %>% fit(
    x = train_data_array,
    y = train_labels,
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
