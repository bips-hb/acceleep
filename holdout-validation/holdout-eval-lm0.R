#! /usr/bin/env Rscript
library(dplyr)
library(acceleep)
library(cli)

tick <- Sys.time()
# Declaring metadata ----
model_kind <- "LM0"
run_start <- format(tick, '%Y%m%d%H%M%S')
cli::cli_alert_info("Starting {model_kind} holdout eval on {run_start}")

metadata <- get_overview_table() %>%
  distinct(model, placement) %>%
  tidyr::expand_grid(outcome = c("kJ", "Jrel", "MET")) %>%
  mutate(res = ifelse(model == "activpal", 20, 100))

# Make a progress bar
# prog <- cli_progress_bar(
#   format = ":current of :total [:bar] (:percent, :elapsedfull)",
#   total = nrow(metadata)
# )

# Initialize empty tibble to collect results
eval_result <- tibble::tibble()

# Big loop over accelerometers, placements, outcomes
for (row in seq_len(nrow(metadata))) {
  #prog$tick()

  # hold current metadata
  metaparams <- metadata[row, ]

  cli::cli_alert_info("Starting {model_kind} on {metaparams$model} ({metaparams$placement}) / {metaparams$outcome}")

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

  # Modeling ----
  model_tick <- Sys.time()

  model <- lm(outcome ~ 1, data = train_data[-c(1:2)])

  # Evaluate, save results
  # Make predictions
  predicted_obs <- test_data %>%
    select(ID, interval, outcome) %>%
    mutate(
      predicted = as.numeric(predict(model, test_data[-c(1:3)]))
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

