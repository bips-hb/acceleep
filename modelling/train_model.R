library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()

# Hyperparameter flags ----

FLAGS <- flags(
  flag_string("model", "geneactiv"),
  flag_string("placement", "hip_right"),
  flag_string("outcome", "kJ"),
  flag_numeric("res", 100),
  flag_numeric("lr", 0.001),
  flag_numeric("decay", 0),
  flag_numeric("batch_size", 32),
  flag_numeric("epochs", 20),
  flag_numeric("verbose", 0),
  flag_numeric("validation_split", 0.2),
  flag_boolean("shuffle", TRUE),
  flag_numeric("lstm_layers", 1),
  flag_numeric("dense_layers", 0),
  flag_numeric("lstm_units", 128),
  flag_numeric("dense_units", 128),
  flag_numeric("dropout_rate", 0.2)
)

# Data Preparation ----

# Collecting data
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = FLAGS$model, placement = FLAGS$placement,
  outcome = FLAGS$outcome, random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = FLAGS$res
)

# Model building ----
# Model creation is wrapped in a strategy scope
# to enable distribution across GPUs
# Define scope to use all available GPUs
strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

with(strategy$scope(), {
  model <- keras_model_sequential()

  # We need at least one LSTM layer, add additional ones before the last one
  if (FLAGS$lstm_layers > 1) {
    for (lstm_layer in seq_len(FLAGS$lstm_layers - 1)) {
      model %>%
        layer_lstm(
          units = FLAGS$lstm_units,
          activation = "tanh", recurrent_activation = "sigmoid",
          recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
          return_sequences = TRUE
        ) %>%
        layer_dropout(rate = FLAGS$dropout_rate)
    }
  }

  # Add the last LSTM layer with return_sequences = FALSE
  model %>%
    layer_lstm(
      units = FLAGS$lstm_units,
      activation = "tanh", recurrent_activation = "sigmoid",
      recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
      return_sequences = FALSE
    ) %>%
    layer_dropout(rate = FLAGS$dropout_rate)

  # Add optional additional dense layers before the last dense layer
  if (FLAGS$dense_layers > 0) {
    for (dense_layer in seq_len(FLAGS$dense_layers)) {
      model %>%
        layer_dense(units = FLAGS$dense_units, activation = "relu") %>%
        layer_dropout(rate = FLAGS$dropout_rate)
    }
  }

  # Every model with have this final dense layer for the output
  model %>%
    layer_dense(units = 1, name = "output")
})

# Compilation
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = FLAGS$lr, FLAGS$decay),
  metrics = "mae"
)

tick <- Sys.time()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_split = FLAGS$validation_split,
  verbose = FLAGS$verbose,
  shuffle = FLAGS$shuffle,
  callbacks =
    list(
      # callback_tensorboard(log_dir = log_path),
      # callback_reduce_lr_on_plateau(patience = 2, factor = 0.5, min_lr = 0.0001),
      # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 3, mode = "min", restore_best_weights = TRUE),
      callback_terminate_on_naan()
    )
)
tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
cat(cliapp::cli_alert_info("Took {took} -- Minimum validation RMSE: {round(min(sqrt(history$metrics$val_loss)), 2)}"))

# Compare predictions to training labels for reference ----
library(ggplot2)

# sample_intervals <- sample(dim(train_data)[[1]], size = 500)
sample_intervals <- 600:800

model_comparison <- tibble::tibble(
  index = seq_len(length(sample_intervals)),
  Predicted = as.numeric(predict(model, train_data[sample_intervals,,])),
  Observed = train_labels[sample_intervals]
)

seq_rmse <- model_comparison %>%
  summarize(rmse = sqrt(mean((Predicted - Observed)^2))) %>%
  pull(rmse) %>%
  round(2)

png(filename = "prediction-comparison.png", width = 13, height = 8, units = "in", res = 300)
p <- model_comparison %>%
  tidyr::pivot_longer(cols = c(Predicted, Observed)) %>%
  ggplot(aes(x = index, y = value, color = name, fill = name)) +
  geom_path() +
  geom_point(shape = 21, color = "darkgray") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  labs(
    title = "Comparison of predicted and observed labels",
    subtitle = glue::glue("Using an arbitrary set of {length(sample_intervals)} successive intervals from the training data\nRMSE in this interval = {seq_rmse}"),
    x = "Interval Index", y = glue::glue("Energy Expenditure ({FLAGS$outcome})"), fill = "", color = "",
    caption = glue::glue("Accelerometer: {FLAGS$model}, {FLAGS$placement}")
  ) +
  theme_minimal() +
  theme(legend.position = "top")

p
dev.off()

# Print plot also to show up in output viewer
p
# Close both GPU devices to free up resources just in case.
# Be careful not to close them on other running CUDA processes!
# cuda_close_device(c(0, 1))
