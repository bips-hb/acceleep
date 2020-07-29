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
  flag_numeric("res", 100), # When summarizing anyway, this seems rather pointless to set below max res
  flag_numeric("lr", 0.001),
  flag_numeric("decay", 0),
  flag_numeric("batch_size", 32),
  flag_numeric("epochs", 20),
  flag_numeric("verbose", 1),
  flag_numeric("validation_split", 0.2),
  flag_numeric("dense_layers", 2),
  flag_numeric("dense_units", 128),
  flag_numeric("dropout_rate", 0.2)
)

# Data Preparation ----

# Collecting data
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_regression(
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

  # Add additional dense layers before the last dense layer
  for (dense_layer in seq_len(FLAGS$dense_layers)) {

    if (dense_layer == 1) {
      model %>%
        layer_dense(units = FLAGS$dense_units, activation = "relu", input_shape = 30)
    } else {
      model %>%
        layer_dense(units = FLAGS$dense_units, activation = "relu")
    }

    if (FLAGS$dropout_rate > 0) {
      model %>%
        layer_dropout(rate = FLAGS$dropout_rate)
    }
  }

  # Every model will have this final dense layer for the output
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
  x = as.matrix(train_data[-c(1,2)]),
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
train_predicted <- as.numeric(predict(model, as.matrix(train_data[-c(1,2)])))

prediction_comparison <- train_data %>%
  select("ID", "interval") %>%
  mutate(observed = train_labels, predicted = train_predicted) %>%
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
    subtitle = glue::glue("Minimal validation RMSE during training: {min_rmse_val}\nMedian RMSE across subjects: {round(median(rmse_per_subject$rmse), 2)}"),
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

