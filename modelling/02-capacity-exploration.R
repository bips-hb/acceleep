# Systematic capacity exploration
library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate::dict(1)
# yields: Error in FUN(X[[i]], ...) : subscript out of bounds
# This ^ is here to ensure the subsequent functions are executed properly
# Can be "fixed" by calling reticulate:::ensure_python_initialized() first

# Close both GPU devices to free up resources just in case.
# Be careful not to close them on other running CUDA processes!
cuda_close_device(c(0, 1))


# A sample configuration list to collect metadata of current sample data for easier
# downstream management and logging
sample_meta <- list(
  model = "geneactiv", placement = "hip_right",
  res = 100, outcome = "kJ"
)

# Collecting data
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = sample_meta$model, placement = sample_meta$placement,
  outcome = sample_meta$outcome, random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = sample_meta$res
)

# Primary model training function ----
train_model <- function(
  sample_meta,
  lstm_layers = 1, dense_layers = 1,
  lstm_units = 128, dense_units = 128,
  droput_rate = 0.2, lr = 0.1, validation_split = 0.2,
  epochs = 10, batch_size = 64, verbose = 1,
  name_custom = "", run_dir = "exploration", push_notify = FALSE
  ) {

  # # Model creation is wrapped in a strategy scope
  # # to enable distribution across GPUs
  # with(strategy$scope(), {

    model <- keras_model_sequential()

    # We need at least one LSTM layer, add additional ones before the last one
    if (lstm_layers > 1) {
      for (lstm_layer in seq_len(lstm_layers - 1)) {
        model %>%
          layer_lstm(
            units = lstm_units,
            activation = "tanh", recurrent_activation = "sigmoid",
            recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
            return_sequences = TRUE
          ) %>%
          layer_dropout(rate = droput_rate)
      }
    }

    # Add the last LSTM layer with return_sequences = FALSE
    model %>%
      layer_lstm(
        units = lstm_units,
        activation = "tanh", recurrent_activation = "sigmoid",
        recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
        return_sequences = FALSE
      ) %>%
      layer_dropout(rate = droput_rate)

    # Add optional additional dense layers before the last dense layer
    if (dense_layers > 0) {
      for (dense_layer in seq_len(dense_layers)) {
        model %>%
          layer_dense(units = dense_units, activation = "relu") %>%
          layer_dropout(rate = droput_rate)
      }
    }

    # Every model with have this final dense layer for the output
    model %>%
      layer_dense(units = 1, name = "output")
  ##})

  # Compilation
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = lr),
    metrics = "mae"
  )

  # browser()
  # make a logging path
  conf_dense <- ""
  if (dense_layers > 0) {
    conf_dense <- glue::glue("U_{dense_layers}Dense{dense_units}")
  }
  conf_model <- glue::glue("{lstm_layers}LSTM{lstm_units}{conf_dense}-BS{batch_size}-LR{lr}")
  conf_sample <- glue::glue("{sample_meta$model}-{sample_meta$placement}-{sample_meta$outcome}-{sample_meta$res}Hz")
  model_id <- glue::glue("{conf_model}--{conf_sample}-{name_custom}-{format(Sys.time(),'%Y%m%d-%H%M%S')}")
  log_path <- here::here("output", "runs", run_dir, model_id)

  # Make sure directory exists, just in case
  if (!fs::dir_exists(here::here("output", "runs", run_dir))) fs::dir_create(here::here("output", "runs", run_dir))

  tick <- Sys.time()
  history <- model %>% fit(
    train_data,
    train_labels,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = validation_split,
    verbose = verbose,
    callbacks =
      list(
        callback_tensorboard(log_dir = log_path),
        callback_reduce_lr_on_plateau(patience = 1, factor = 0.5, min_lr = 0.0001),
        # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 3, mode = "min", restore_best_weights = TRUE),
        callback_terminate_on_naan()
      )
  )
  tock <- Sys.time()
  took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
  min_rms <- round(min(sqrt(history$metrics$val_loss)), 2)

  log_message <- glue::glue("Model {model_id} took {took}, min rmse = {min_rms}")

  cli_alert_info(log_message)
  if (push_notify) pushoverr::pushover(log_message, title = "Modelling hell", priority = 0)


  list(model_id = model_id, model = model, history = history)
}

# Close both GPU devices to free up resources just in case.
# Be careful not to close them on other running CUDA processes!
cuda_close_device(c(0, 1))

# Model creation is wrapped in a strategy scope
# to enable distribution across GPUs
# Define scope to use all available GPUs
strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

# Training loop over param_grid ----

# Define parameter grid to explore
param_grid <- tidyr::expand_grid(
  lstm_layers = 1:3,
  lstm_units = c(64, 128, 256, 512),
  dense_layers = c(0, 1),
  dense_units = c(128, 256),
  batch_size = c(4, 8, 16, 32)
)

for (row in seq_len(nrow(param_grid))) {

  params <- param_grid[row, ]

  with(strategy$scope(), {
    train_output <- train_model(
      sample_meta,
      lstm_layers = params$lstm_layers, dense_layers = params$dense_layers,
      lstm_units = params$lstm_units, dense_units = params$dense_units,
      droput_rate = 0.2, lr = 0.1,
      epochs = 20, batch_size = params$batch_size, push_notify = FALSE,
      run_dir = "exploration-pt3", name_custom = "",
      verbose = 0
    )
  })
}
pushoverr::pushover("Modelling done!", title = "Modelling hell", priority = 0)


# Ad hoc models ----
with(strategy$scope(), {
  train_output <- train_model(
    sample_meta,
    lstm_layers = 2, dense_layers = 0,
    lstm_units = 128, dense_units = 0,
    droput_rate = 0.2, lr = 0.01,
    epochs = 15, batch_size = 8,
    name_custom = "with-lr-scheduler",
    push_notify = FALSE, run_dir = "ad-hoc"
  )
})

# Compare predictions to training labels for reference ----
library(ggplot2)
model <- train_output$model

sample_intervals <- sample(dim(train_data)[[1]], size = 500)
sample_intervals <- 600:1200

model_comparison <- tibble::tibble(
  index = seq_len(length(sample_intervals)),
  Predicted = as.numeric(predict(model, train_data[sample_intervals,,])),
  Observed = train_labels[sample_intervals]
)

model_comparison %>%
  summarize(rmse = sqrt(mean((Predicted - Observed)^2)))

model_comparison %>%
  tidyr::pivot_longer(cols = c(Predicted, Observed)) %>%
  ggplot(aes(x = index, y = value, color = name, fill = name)) +
  geom_path() +
  geom_point(shape = 21, color = "darkgray") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  labs(
    title = "Comparison of predicted and observed labels",
    subtitle = "Using an arbitrary set of intervals from the training data",
    x = "Interval Index", y = "Energy Expenditure (kJ)", fill = "", color = "",
    caption = "Accelerometer: geneactiv, right hip"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

model_comparison %>%
  mutate(diff = Predicted - Observed) %>%
  ggplot(aes(x = index, y = diff)) +
  geom_path() +
  geom_point(shape = 21, color = "darkgray") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  labs(
    title = "Comparison of predicted and observed labels",
    subtitle = "Using an arbitrary set of intervals from the training data",
    x = "Interval Index", y = "Differenze in Prediction: Energy Expenditure (kJ)", fill = "", color = "",
    caption = "Accelerometer: activPAL, right thigh"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
