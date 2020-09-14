#! /usr/bin/env Rscript
library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()
reticulate::dict(python = "says okay")

# If this is true, only geneactiv hip_right / kJ will be CV'd for quicker iteration
MINI_RUN <- FALSE

tick <- Sys.time()
# Declaring metadata ----
model_kind <- "RNN"
run_start <- format(tick, '%Y%m%d%H%M%S')
cliapp::cli_alert_info("Starting {model_kind} LOSO-CV on {run_start}")

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = 1)

if (MINI_RUN) {
  cliapp::cli_alert_warning("Only running on GENEActiv (right hip) with kJ!")

  metadata <- metadata %>%
    filter(model == "geneactiv", placement == "hip_right", outcome == "kJ")
} else {
  cliapp::cli_alert_warning("Running on on {nrow(metadata)} accelerometer/outcome combinations!")
}


# Big loop over accelerometers, placements, outcomes
for (row in seq_len(nrow(metadata))) {
  # hold current metadata
  metaparams <- metadata[row, ]
  cliapp::cli_alert_info("Starting {model_kind} on {metaparams$model} ({metaparams$placement}) / {metaparams$outcome}")

  # browser()

  # Collecting original training data only to get it's subject IDs
  # resolution is small here because it doesn't matter, actual training data is read later
  c(c(train_data_full, train_labels_full), c(., .)) %<-% keras_prep_lstm(
    model = metaparams$model, placement = metaparams$placement,
    outcome = metaparams$outcome, random_seed = 19283, val_split = 1/3,
    interval_length = 30, res = 1
  )

  IDs_full <- train_data_full %>%
    pull(.data$ID) %>%
    unique() %>%
    sort()

  # Get the entire dataset (again), from which each LOO-CV train/validation set will be derived
  full_data <- get_combined_data(model = metaparams$model, placement = metaparams$placement, res = metaparams$res)

  # Initialize empty tibble to collect results
  cv_result <- tibble::tibble()

  prog <- cli_progress_bar(
    format = ":current of :total [:bar] (:percent, :elapsedfull)",
    total = length(IDs_full)
  )

  # Loop over subject IDs ----
  # leave out one for each model run
  for (i in IDs_full) {
    prog$tick()

    # Dataprep ----
    # Split into train / validation datasets based on subject IDs
    training_data <- full_data %>%
      filter(.data$ID %in% IDs_full, .data$ID != i)

    validation_data <- full_data %>%
      filter(.data$ID == i)

    # Check
    unique(training_data$ID)
    unique(validation_data$ID)

    # Normalize
    c(training_data, validation_data) %<-% normalize_accelerometry(training_data, validation_data)


    # Split into data and labels
    split_data <- split_data_labels(training_data, validation_data, outcome = metaparams$outcome)

    c(train_data, train_labels) %<-% split_data$training
    c(test_data, test_labels) %<-% split_data$validation

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

    # Not training on multi-gpu b/c weird "Unknown: CUDNN_STATUS_BAD_PARAM" error I don't understand
    # strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

    model_note <- "LSTM256-LSTM256-D128-D64-E50-LR5"
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
      # callbacks = list(
      #   callback_model_checkpoint(
      #     filepath = tempfile(),
      #     monitor = "val_loss",
      #     save_best_only = TRUE,
      #     mode = "min"
      #   )
      # ),
      # Uncomment the following to monitor validation error during training w/ verbose = 1
      # validation_data =
      #   list(
      #     test_data_array,
      #     test_labels
      #   ),
      verbose = 0
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

    current_result <- tibble::tibble(
      left_out = i,
      rmse = prediction_rmse,
      eval_rmse = sqrt(eval_result[["loss"]]),
      predicted_obs = list(predicted_obs),
      model_note = model_note,
      mini_run = MINI_RUN,
      model_took = model_took
    )

    cv_result <- bind_rows(cv_result, current_result)

    # Save per-subject model maybe?
    out_dir_models <- here::here("output", "cross-validation", model_kind, run_start, "models")
    if (!fs::dir_exists(out_dir_models)) fs::dir_create(out_dir_models)
    filename_model <- glue::glue("k1-cv-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-LOSO_{i}-{run_start}.hdf5")
    save_model_hdf5(model, filepath = fs::path(out_dir_models, filename_model))
  }


  # Save result tibble
  filename <- glue::glue("k1-cv-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.rds")

  out_dir <- here::here("output", "cross-validation", model_kind, run_start)
  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # Save CV RMSE results
  saveRDS(object = cv_result, file = fs::path(out_dir, filename))

  # Write model structure to plain text file
  capture.output(summary(model), file =  fs::path(out_dir, fs::path_ext_set(filename, "txt")))
}

tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
pushoverr::pushover(glue::glue("{model_kind} (1Hz) cross validation is done! Took {took}"), title = "Modelling Hell", priority = 1)

cuda_close_device()
