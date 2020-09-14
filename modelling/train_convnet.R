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
  flag_numeric("res", 100),
  flag_numeric("lr", 0.001),
  flag_numeric("decay", 0),
  flag_numeric("batch_size", 32),
  flag_numeric("epochs", 20),
  flag_numeric("verbose", 1),
  flag_numeric("validation_split", 0.2),
  flag_numeric("conv1d_layers", 2),
  flag_numeric("dense_layers", 2),
  flag_numeric("conv1d_filters", 32),   # Unused
  flag_numeric("conv1d_filters_1", 32), # filters in layer 1
  flag_numeric("conv1d_filters_2", 32), # filters in layer 2
  flag_numeric("conv1d_filters_3", 32), # filters in layer 3
  flag_numeric("conv1d_kernel_size", 7),
  flag_numeric("conv1d_kernel_size_1", 7), # kernel size in layer 1
  flag_numeric("conv1d_kernel_size_2", 7), # ..layer 2
  flag_numeric("conv1d_kernel_size_3", 7), # ..layer 3
  flag_numeric("conv1d_pool_size", 10),
  flag_numeric("conv1d_pool_strides", 10),
  flag_numeric("dense_units", 64),   # Unused
  flag_numeric("dense_units_1", 64), # Units in dense layer 1
  flag_numeric("dense_units_2", 64), # ... in layer 2
  flag_numeric("dense_units_3", 64), # ... 3
  flag_numeric("dropout_rate", 0.2),
  flag_boolean("batch_normalize", TRUE),
  flag_numeric("weight_decay_conv", 0.01), # default value for regularize_l2()
  # flag_numeric("weight_decay_dense", 0.01), # default value for regularize_l2()
  flag_string("conv1d_reduction", "maxpooling"),
  flag_boolean("callback_reduce_lr", FALSE),
  flag_numeric("callback_reduce_lr_patience", 3),
  flag_numeric("callback_reduce_lr_factor", 0.1),
  flag_numeric("callback_reduce_lr_min_delta", 0.01),
  flag_numeric("callback_reduce_lr_min_lr", 1e-8),
  flag_string("model_kind", "CNN") # Dummy flag just in case for sorting later
)

# Data Preparation ----

# Collecting data
c(c(train_data_full, train_labels), c(test_data_full, test_labels)) %<-% keras_prep_lstm(
  model = FLAGS$accel_model, placement = FLAGS$placement,
  outcome = FLAGS$outcome, random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = FLAGS$res
)

# Reshaping to array form, keep train_data_full for later prediction
train_data <- keras_reshape_accel(
  accel_tbl = train_data_full, interval_length = 30, res = FLAGS$res
)

test_data <- keras_reshape_accel(
  accel_tbl = test_data_full, interval_length = 30, res = FLAGS$res
)

dim(train_data)
dim(train_data_full)

dim(test_data)
dim(test_data_full)


length(train_labels)
length(test_labels)

# Model building ----
# Model creation is wrapped in a strategy scope
# to enable distribution across GPUs
# Define scope to use all available GPUs
strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

with(strategy$scope(), {
  model <- keras_model_sequential()

  for (conv_layer in seq_len(FLAGS$conv1d_layers)) {

    # For some reason explicitly supplying input_shape = NULL is something different
    # than not supplying the argument at all, which is why I have to do this weird
    # dance to ensure the input_shape argument is provided, otherwise the output of
    # summary(model) will not appear in the results, which is kind of a bummer
    input_shape <- NULL
    if (conv_layer == 1) input_shape <- dim(train_data)[c(2, 3)]

    # layer_filters <- FLAGS$conv1d_filters
    # layer_kernel_size <- FLAGS$conv1d_kernel_size

    layer_filters <- FLAGS[[glue::glue("conv1d_filters_{conv_layer}")]]
    layer_kernel_size <- FLAGS[[glue::glue("conv1d_kernel_size_{conv_layer}")]]


    model %>%
      layer_conv_1d(
        filters = layer_filters,
        kernel_size = layer_kernel_size,
        kernel_regularizer = regularizer_l2(l = FLAGS$weight_decay_conv),
        activation = "relu",
        input_shape = input_shape
      )

    if (FLAGS$batch_normalize) {
      model %>%
        layer_batch_normalization()
    }

    # Every conv layer *but* the last layer gets a pooling layer
    if (conv_layer < FLAGS$conv1d_layers) {
      model %>%
        layer_max_pooling_1d(pool_size = FLAGS$conv1d_pool_size, strides = FLAGS$conv1d_pool_strides)
    }
  }

  # Flatten the conv1d layers either by max pooling or "just" flattening
  if (FLAGS$conv1d_reduction == "maxpooling") {
    model %>%
      layer_global_max_pooling_1d()
  } else if (FLAGS$conv1d_reduction == "flatten") {
    model %>%
      layer_flatten()
  }


  for (dense_layer in seq_len(FLAGS$dense_layers)) {

    # layer_dense_units <- FLAGS$dense_units
    layer_dense_units <- FLAGS[[glue::glue("dense_units_{dense_layer}")]]

    model %>%
      layer_dense(
        units = layer_dense_units,
        # kernel_regularizer = regularizer_l2(l = FLAGS$weight_decay_dense),
        activation = "relu"
        )

    if (FLAGS$batch_normalize) {
      model %>%
        layer_batch_normalization()
    }

    if (FLAGS$dropout_rate > 0) {
      model %>%
        layer_dropout(rate = FLAGS$dropout_rate)
    }
  }

  model %>%
    layer_dense(units = 1, name = "output",  activation = "linear")
})

# Compilation
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = FLAGS$lr, decay = FLAGS$decay)
)

# Assemble callbacks
callbacks <- list(
  callback_terminate_on_naan(), # This can't hurt
  callback_model_checkpoint(
    filepath = "best-model.hdf5", monitor = "val_loss",
    save_best_only = TRUE, save_freq = "epoch", save_weights_only = FALSE
  )
)

if (FLAGS$callback_reduce_lr) {
  callbacks[[3]] <- callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    mode = "min", # reduce LR only if loss stops decreasing
    verbose = FLAGS$verbose,
    patience = FLAGS$callback_reduce_lr_patience,
    factor = FLAGS$callback_reduce_lr_factor,
    min_delta = FLAGS$callback_reduce_lr_min_delta,
    min_lr = FLAGS$callback_reduce_lr_min_lr
  )
}

tick <- Sys.time()
history <- model %>% fit(
  x = train_data,
  y = train_labels,
  epochs = FLAGS$epochs,
  batch_size = FLAGS$batch_size,
  validation_split = FLAGS$validation_split,
  verbose = FLAGS$verbose,
  shuffle = TRUE,
  callbacks = callbacks
)

print(summary(model))

# Compare predictions to training labels for reference ----
library(ggplot2)

# Predict on full training set -----

min_rmse_val <- round(min(sqrt(history$metrics$val_loss)), 2)

# Predict in the training data
train_predicted <- as.numeric(predict(model, train_data))

prediction_comparison <- train_data_full %>%
  select(ID, interval, observed = FLAGS$outcome) %>%
  distinct() %>%
  mutate(predicted = train_predicted) %>%
  group_by(ID) %>%
  mutate(interval_index = seq_along(ID)) %>%
  ungroup()

# Per subject RMSEs for the plot
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
  ggplot(aes(x = interval_index, y = value, color = name, fill = name)) +
  facet_wrap(~ID) +
  geom_path() +
  labs(
    title = glue::glue("Predicted and observed outcome: {FLAGS$accel_model}, {FLAGS$placement} at {FLAGS$res}Hz"),
    subtitle = glue::glue("Minimal validation RMSE during training: {min_rmse_val}
Median RMSE across subjects in training set: {round(median(rmse_per_subject$rmse), 2)}
Mean RMSE across subjects in training set: {round(mean(rmse_per_subject$rmse), 2)}"),
    x = "Interval Index", y = glue::glue("Energy Expenditure ({FLAGS$outcome})"), fill = "", color = "",
    caption = glue::glue("Strip text: Subject ID (Subject RMSE)")
  ) +
  theme_minimal() +
  theme(legend.position = "top")
p
dev.off()
p

# sign off ----
tock <- Sys.time()
took <- hms::hms(seconds = round(as.numeric(difftime(tock, tick, units = "secs"))))
log_final <- glue::glue("\nTook {took} -- Minimum validation RMSE: {min_rmse_val}")
cat(log_final, "\n")
# ---

