#
source(here::here("modelling/_init.R"))

c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = "geneactiv", placement = "hip_right",
  outcome = "kJ", random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = 1
)


# Define scope to use all available GPUs
strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

# Test model ----
with(strategy$scope(), {
  model <- keras_model_sequential(name = "2LSTM_128U_adam") %>%
    layer_lstm(
      units = 256,
      activation = "tanh", recurrent_activation = "sigmoid",
      recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
      return_sequences = TRUE
    ) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(
      units = 256,
      activation = "tanh", recurrent_activation = "sigmoid",
      recurrent_dropout = 0, unroll = FALSE, use_bias = TRUE,
      return_sequences = FALSE
    ) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, name = "output")
})


model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = 1e-5),
  metrics = "mae"
)

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 50,
  batch_size = 128,
  validation_split = 0.2,
  verbose = 1,
  callbacks =
    list(
      # callback_reduce_lr_on_plateau(),
      # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 5, mode = "min"),
      callback_terminate_on_naan()
    )
)

library(ggplot2)



c(training_data, train_labels) %<-% assemble_train_data(
  accel_model = "geneactiv", placement = "hip_right",
  outcome = "kJ", random_seed = 19283, val_split = 1/3,
  res = 1
)

train_predictions <- predict_on_training_set(model, training_data, interval_length = 30, res = 1)

train_labels %>%
  group_by(ID) %>%
  mutate(interval = seq_along(ID)) %>%
  ungroup() %>%
  left_join(train_predictions, by = c("interval", "ID")) %>%
  tidyr::pivot_longer(cols = c("kJ", "predicted")) %>%
  ggplot(aes(x = interval, y = value, color = name, fill = name)) +
  facet_wrap(~ID) +
  geom_path() +
  theme_minimal() +
  theme(legend.position = "top")

#----


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
