library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()

# Hyperparameter flags ----

FLAGS <- flags(
  flag_string("accel_model", "geneactiv"),
  flag_string("placement", "hip_right"),
  flag_string("outcome", "kJ"),
  flag_numeric("res", 1),
  flag_numeric("lr", 0.001),
  flag_numeric("decay", 0),
  flag_numeric("batch_size", 32),
  flag_numeric("epochs", 20),
  flag_numeric("verbose", 1),
  flag_numeric("validation_split", 0.2),
  # flag_boolean("shuffle", TRUE),
  flag_numeric("lstm_layers", 2),
  flag_numeric("dense_layers", 2),
  flag_numeric("lstm_units", 128),
  flag_numeric("dense_units", 128),
  flag_numeric("dropout_rate", 0.2)
)

# Data Preparation ----

# Collecting data
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = FLAGS$accel_model, placement = FLAGS$placement,
  outcome = FLAGS$outcome, random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = FLAGS$res
)

dim(train_data)
dim(test_data)
length(train_labels)
length(test_labels)

# Model building ----
# Model creation is wrapped in a strategy scope
# to enable distribution across GPUs
# Define scope to use all available GPUs
strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

with(strategy$scope(), {
  model <- keras_model_sequential()

  # We need at least one LSTM layer, first one with input_shape, last one with return_sequences = FALSE
  for (lstm_layer in seq_len(FLAGS$lstm_layers)) {

    input_shape <- if (lstm_layer == 1) dim(train_data)[c(2,3)] else NULL
    return_sequences <- !(lstm_layer == FLAGS$lstm_layers)

    logmsg <- glue::glue("Making LSTM layer {lstm_layer} of {FLAGS$lstm_layers}:\n")
    cat(logmsg)
    #if (is.null(input_shape)) cat("input_shape is NULL\n")
    if (return_sequences) cat("return_sequences is TRUE\n")


    model %>%
      layer_lstm(
        units = FLAGS$lstm_units, # input_shape = input_shape,
        activation = "tanh", recurrent_activation = "sigmoid",
        recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
        return_sequences = return_sequences
      )

    if (FLAGS$dropout_rate > 0) {
      model %>%
        layer_dropout(rate = FLAGS$dropout_rate)
    }
  }

  # Add optional additional dense layers before the last dense layer
  if (FLAGS$dense_layers > 0) {
    for (dense_layer in seq_len(FLAGS$dense_layers)) {
      model %>%
        layer_dense(units = FLAGS$dense_units, activation = "relu")

      if (FLAGS$dropout_rate > 0) {
        model %>%
          layer_dropout(rate = FLAGS$dropout_rate)
      }
    }
  }

  # Every model with have this final dense layer for the output
  model %>%
    layer_dense(units = 1, name = "output")
})

# Compilation
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = FLAGS$lr, decay = FLAGS$decay),
  metrics = "mae"
)

tick <- Sys.time()
history <- model %>% fit(
  x = train_data,
  y = train_labels,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_split = FLAGS$validation_split,
  verbose = FLAGS$verbose,
  shuffle = TRUE,
  callbacks =
    list(
      # callback_tensorboard(log_dir = log_path),
      # callback_reduce_lr_on_plateau(patience = 2, factor = 0.5, min_lr = 0.0001),
      # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 3, mode = "min", restore_best_weights = TRUE),
      callback_terminate_on_naan()
    )
)

# print(summary(model))

# Compare predictions to training labels for reference ----
library(ggplot2)

min_rmse_val <- round(min(sqrt(history$metrics$val_loss)), 2)

# Predict on full training set -----
# Reassemble full training data with ID information
# (inefficient but wanted to avoid overhauling the entire pipeline)
c(training_data_full, .) %<-% assemble_train_data(
  accel_model = FLAGS$accel_model, placement = FLAGS$placement,
  outcome = FLAGS$outcome, random_seed = 19283, val_split = 1/3,
  res = FLAGS$res
)

# Predict in the training data
train_predicted <- as.numeric(predict(model, train_data))

# get the order of IDs in training data for correct matching later
ID_order <- unique(training_data_full$ID)

ID_counts <- training_data_full %>%
  select(interval, ID) %>%
  distinct() %>%
  count(ID) %>%
  mutate(ID = factor(ID, levels = ID_order)) %>%
  arrange(ID) %>%
  mutate(ID = as.character(ID))

train_ids <- ID_counts %>%
  purrr::pmap(~{
    rep(.x, times = .y)
  }) %>%
  unlist()

prediction_comparison <- tibble(
  ID = train_ids,
  predicted = train_predicted,
  observed = train_labels
) %>%
  group_by(ID) %>%
  mutate(interval = seq_along(ID))

rmse_per_subject <- prediction_comparison %>%
  group_by(ID) %>%
  summarize(rmse = sqrt(mean((predicted - observed)^2)), .groups = "drop")

png(filename = "rmse-per-child.png", width = 13, height = 8, units = "in", res = 300)
p <- ggplot(rmse_per_subject, aes(x = ID, y = rmse)) +
  geom_col() +
  labs(
    title = "RMSE of Subjects in Training Set",
    x = "Subject ID",
    y = glue::glue("RMSE ({FLAGS$outcome})")
  ) +
  theme_minimal()
p
dev.off()
p

png(filename = "prediction-comparison-all.png", width = 13, height = 8, units = "in", res = 300)
p <- prediction_comparison %>%
  left_join(rmse_per_subject, by = c("ID")) %>%
  mutate(ID = glue::glue("{ID} ({round(rmse, 2)})")) %>%
  select(-rmse) %>%
  tidyr::pivot_longer(cols = c("observed", "predicted")) %>%
  ggplot(aes(x = interval, y = value, color = name, fill = name)) +
  facet_wrap(~ID) +
  geom_path() +
  labs(
    title = glue::glue("Predicted and observed labels: {FLAGS$accel_model}, {FLAGS$placement} at {FLAGS$res}Hz"),
    subtitle = glue::glue("Minimal validation RMSE during training: {min_rmse_val}"),
    x = "Interval Index", y = glue::glue("Energy Expenditure ({FLAGS$outcome})"), fill = "", color = "",
    caption = glue::glue("Strip text: Subject ID (Subject RMSE)")
  ) +
  theme_minimal() +
  theme(legend.position = "top")
p
dev.off()
p

# ----
tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
log_final <- glue::glue("\nTook {took} -- Minimum validation RMSE: {min_rmse_val}")
cat(log_final, "\n")

# if (min_rmse_val < 3.5) pushoverr::pushover(log_final, title = "Modelling Hell")
# ---

