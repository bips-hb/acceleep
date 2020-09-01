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
MINI_RUN <- TRUE

tick <- Sys.time()
# Declaring metadata ----
model_kind <- "DNN"
run_start <- format(tick, '%Y%m%d%H%M%S')

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = ifelse(model == "activpal", 20, 100))

if (MINI_RUN) {
  cliapp::cli_alert_warning("Only running on GENEActiv (right hip) with kJ!")

  metadata <- metadata %>%
    filter(model == "geneactiv", placement == "hip_right", outcome == "kJ")
}

# Big loop over accelerometers, placements, outcomes
for (row in seq_len(nrow(metadata))) {
  # hold current metadata
  metaparams <- metadata[row, ]
  # browser()

  # Collecting original training data only tpo get it's subject IDs
  # resolution is small here because it doesn't matter, actual training data is read later
  c(c(train_data_full, train_labels_full), c(., .)) %<-% keras_prep_regression(
    model = metaparams$model, placement = metaparams$placement,
    outcome = metaparams$outcome, random_seed = 19283, val_split = 1/3,
    interval_length = 30, res = metaparams$res
  )

  # This is easier for later subject-splitting I assume.
  train_data_full$outcome <- train_labels_full

  IDs_full <- train_data_full %>%
    pull(.data$ID) %>%
    unique() %>%
    sort()

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
    training_data <- train_data_full %>%
      filter(.data$ID %in% IDs_full, .data$ID != i) %>%
      select(-.data$outcome)

    # Fail if for some reason left out ID is in training set
    stopifnot(!(i %in% unique(training_data$ID)))

    test_data <- train_data_full %>%
      filter(.data$ID == i) %>%
      select(-.data$outcome)

    # Fail if left out ID is _not_ in validation data where it belongs
    stopifnot(i %in% unique(test_data$ID))


    # Check
    unique(training_data$ID)
    unique(test_data$ID)

    # Normalize
    # c(training_data, test_data) %<-% normalize_accelerometry(training_data, test_data)


    # # Split into data and labels
    train_labels <- train_data_full %>%
      filter(.data$ID %in% IDs_full, .data$ID != i) %>%
      pull(.data$outcome)

    test_labels <- train_data_full %>%
      filter(.data$ID == i) %>%
      pull(.data$outcome)

    dim(training_data)
    dim(test_data)

    length(train_labels)
    length(test_labels)

    # Modelling ----

    strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

    with(strategy$scope(), {
      model <- keras_model_sequential() %>%
        # L1
        layer_dense(
          activation = "relu", units = 256
        )  %>%
        # layer_batch_normalization() %>%
        layer_dropout(rate = 0.2)  %>%
        # L2
        layer_dense(
          activation = "relu", units = 256
        )  %>%
        # layer_batch_normalization() %>%
        layer_dropout(rate = 0.2)  %>%
        # L3
        layer_dense(
          activation = "relu", units = 128
        )  %>%
        # layer_batch_normalization() %>%
        layer_dropout(rate = 0.2)  %>%
        # L4
        layer_dense(
          activation = "relu", units = 128
        ) %>%
        # layer_batch_normalization() %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 1, name = "output", activation = "linear")
    })

    model %>% compile(
      loss = "mse",
      optimizer = optimizer_adam(lr = 1e-3),
      metrics = "mae"
    )

    history <- model %>% fit(
      as.matrix(training_data[-c(1, 2)]), # Make sure to exclude ID and interval columns (1, 2)
      train_labels,
      batch_size = 32,
      epochs = 100,
      validation_split = 0,
      verbose = 0
    )

    # To check in with LOO model results
    # browser()

    # Evaluate, save results
    eval_result <- model %>%
      evaluate(as.matrix(test_data[-c(1, 2)]), test_labels, verbose = 0)

    # Make predictions
    predicted_obs <- test_data %>%
      select(ID, interval) %>%
      # distinct() %>%
      mutate(
        outcome = test_labels,
        predicted = as.numeric(predict(model, as.matrix(test_data[-c(1, 2)])))
      )

    # prediction rmse differs from result of evaluate() o_O
    prediction_rmse <- predicted_obs %>%
      summarize(rmse = sqrt(mean((predicted - outcome)^2))) %>%
      pull(rmse)

    current_result <- tibble::tibble(
      left_out = i,
      rmse = sqrt(eval_result[["loss"]]),
      prediction_rmse = prediction_rmse,
      # mse = eval_result[["loss"]],
      # mae = eval_result[["mae"]],
      predicted_obs = list(predicted_obs)
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
pushoverr::pushover(glue::glue("{model_kind} cross validation is done! Took {took}"), title = "Modelling Hell", priority = 1)

