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
model_kind <- "DNN"
run_start <- format(tick, '%Y%m%d%H%M%S')
cliapp::cli_alert_info("Starting {model_kind} holdout eval on {run_start}")

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
  prog$tick()

  # hold current metadata
  metaparams <- metadata[row, ]

  cliapp::cli_alert_info("Starting {model_kind} on {metaparams$model} ({metaparams$placement}) / {metaparams$outcome}")

  # browser()

  # Collecting original training data only tpo get it's subject IDs
  # resolution is small here because it doesn't matter, actual training data is read later
  c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_regression(
    model = metaparams$model, placement = metaparams$placement,
    outcome = metaparams$outcome, random_seed = 19283, val_split = 1/3,
    interval_length = 30, res = metaparams$res, normalize = TRUE
  )

  # This is easier for later
  train_data$outcome <- train_labels
  train_data <- train_data %>%
    select(ID, interval, outcome, everything())

  test_data$outcome <- test_labels
  test_data <- test_data %>%
    select(ID, interval, outcome, everything())

  # Check
  unique(train_data$ID)
  unique(test_data$ID)

  dim(train_data)
  dim(test_data)

  # Modelling ----
  model_tick <- Sys.time()

  strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

  model_note <- "D512-D256-BN-E30-ES"
  model_tick <- Sys.time()

  with(strategy$scope(), {
    model <- keras_model_sequential() %>%
      # L1 --
      layer_dense(
        input_shape = 30,
        name = "Dense1",
        activation = "relu", units = 512
      )  %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.2)  %>%
      # L2 --
      layer_dense(
        name = "Dense2",
        activation = "relu", units = 256
      )  %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.2)  %>%
    # Output layer
    layer_dense(units = 1, name = "output", activation = "linear")
  })

  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 1e-3)
  )

  history <- model %>% fit(
    as.matrix(train_data[-c(1:3)]), # Make sure to exclude ID, interval + outcome columns (1, 2, 3)
    train_labels,
    batch_size = 16,
    epochs = 30,
    validation_split = 0,
    # Uncomment the following to monitor validation error during training w/ verbose = 1
    validation_data =
      list(
        as.matrix(test_data[-c(1:3)]),
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
  # Make predictions
  predicted_obs <- test_data %>%
    select(ID, interval) %>%
    # distinct() %>%
    mutate(
      outcome = test_labels,
      predicted = as.numeric(predict(model, as.matrix(test_data[-c(1:3)])))
    )

  # prediction rmse
  prediction_rmse <- predicted_obs %>%
    summarize(rmse = sqrt(mean((predicted - outcome)^2))) %>%
    pull(rmse)

  model_tock <- Sys.time()
  model_took <- hms::hms(seconds = round(as.numeric(difftime(model_tock, model_tick, units = "secs"))))

  eval_result <- tibble::tibble(
    rmse = prediction_rmse,
    predicted_obs = list(predicted_obs),
    model_took = model_took
  )

  # Save per-device model
  out_dir_models <- here::here("output", "holdout-validation", model_kind, run_start, "models")
  if (!fs::dir_exists(out_dir_models)) fs::dir_create(out_dir_models)
  filename_model <- glue::glue("holdout-eval-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.rds")
  saveRDS(model, file = fs::path(out_dir_models, filename_model))
  ##}

  # Save result tibble
  filename <- glue::glue("holdout-eval-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.rds")

  out_dir <- here::here("output", "holdout-validation", model_kind, run_start)
  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # Save RMSE results
  saveRDS(object = eval_result, file = fs::path(out_dir, filename))
}


tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
pushoverr::pushover(glue::glue("{model_kind} holdout validation is done! Took {took}"), title = "Modelling Hell", priority = 1)

