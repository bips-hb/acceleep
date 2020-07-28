# Tentative experimentation
# Goel so far: Make sure the modelling works in practice (re: data structure, basic concept)
# Actual validation loss reduction is not a concern yet
source(here::here("modelling/_init.R"))

c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = "geneactiv", placement = "hip_right",
  outcome = "kJ", random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = 100
)

#
# # Split into train/validation (1/3 of subjects remain for the validation data)
# c(training_data, validation_data) %<-% make_initial_splits(
#   geneactiv_right, random_seed = 11235813, val_split = 1/3
# )
#
# # Training data and labels
# split_data <- split_data_labels(training_data, validation_data, outcome = "Jrel")
#
# c(train_data, train_labels) %<-% split_data$training
# c(test_data, test_labels) %<-% split_data$validation
#
#
# array_train <- training_data %>%
#   keras_reshape_accel(interval_length = 30, res = 20)
#
#
# # Validation data and labels
# array_validation <- validation_data %>%
#   keras_reshape_accel(interval_length = 30, res = 20)

# Doing the keras -----
dim(train_data) # c(2588, 3, 600) // geneactiv hip_right c(2568, 3000, 3)

# LSTM model: ----
# Requirements to use cuDNN: https://keras.io/api/layers/recurrent_layers/lstm/
# Still: Fail to find the dnn implementation.
# -> use layer_cudnn_lstm (different args) -> AttributeError: module 'tensorflow.keras.layers' has no attribute 'CuDNNLSTM'
# -> Not necessary according to https://stackoverflow.com/a/56772187
# -> Just uise layer_lstm() with appropriate parameters, seems to work fine after apt-get upgrade.

model <- keras_model_sequential() %>%
  layer_lstm(
    units = 128,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    return_sequences = TRUE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(
    units = 128,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    return_sequences = FALSE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(activation = "relu", units = 64) %>%
  layer_dense(units = 1, name = "output")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_sgd(lr = 0.001, momentum = 0.9),
  metrics = "mae"
)

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 20,
  validation_split = 0.2,
  verbose = 1,
  callbacks = callback_tensorboard(log_dir = "output/runs/experimental")
)

summary(model)

history

p_hist <- plot(history)
ggplot2::ggsave(filename = here::here("output", "model_history_lstm_array_input.png"))

# model %>% evaluate(array_validation, labels_validation, verbose = 0)

# some predicted vs. original values
predict(model, train_data) %>% head(20)
train_labels %>% head(20)

labels_train[1:20]

# Second / third attempt ----
model <- keras_model_sequential() %>%
  layer_lstm(
    units = 64,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    return_sequences = TRUE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(
    units = 64,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    return_sequences = FALSE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, name = "output")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = "mae"
)

# Trying with longer training w/ early stopping
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 40,
  validation_split = 0.2,
  verbose = 1,
  callbacks =
    list(
      callback_tensorboard(log_dir = "output/runs/experimental"),
      # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 3),
      callback_terminate_on_naan()
    )
)


#  GRU -----
model <- keras_model_sequential() %>%
  layer_gru(
    units = 32,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    reset_after = TRUE,
    return_sequences = TRUE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_gru(
    units = 32,
    activation = "tanh", recurrent_activation = "sigmoid",
    recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
    reset_after = TRUE,
    return_sequences = FALSE
  ) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, name = "output")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = metric_rmse
)

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 20,
  validation_split = 0.5,
  verbose = 1,
  callbacks = callback_tensorboard(log_dir = "output/runs/experimental")
)
