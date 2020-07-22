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
##})

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
cliapp::cli_alert_info("Took {took} -- Minimum validation RMSE: {round(min(sqrt(history$metrics$val_loss)), 2)}")
