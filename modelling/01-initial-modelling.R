# First proper modelling attempts
library(dplyr)
library(acceleep)
library(keras)
# library(tensorflow)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate::dict(1)
# yields: Error in FUN(X[[i]], ...) : subscript out of bounds
# This ^ is here to ensure the subsequent functions are executed properly
# Can be "fixed" by calling reticulate:::ensure_python_initialized() first

# metric_rmse <- custom_metric("rmse", function(y_true, y_pred) {
#   k_sqrt(k_mean(k_square(y_true - y_pred)))
# })


# Assemble all geneactiv_right data, labels = kJ (AEE)

c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
  model = "geneactiv", placement = "hip_right",
  outcome = "kJ", random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = 100
)


# Test model with multi-GPU
# See https://keras.rstudio.com/reference/multi_gpu_model.html

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
    return_sequences = TRUE
  ) %>%
  layer_dropout(rate = 0.2) %>%
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
  layer_dense(units = 1, name = "output")


# Replicates the model on all available GPUs
# parallel_model <- multi_gpu_model(model, gpus = NULL, cpu_relocation = TRUE)
# Error in py_call_impl(callable, dots$args, dots$keywords) :
# TypeError: '<=' not supported between instances of 'NoneType' and 'int'

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = metric_rmse
)

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 40,
  validation_split = 0.2,
  verbose = 1,
  callbacks =
    list(
      callback_tensorboard(log_dir = "output/runs/initial-models", histogram_freq = 5),
      callback_reduce_lr_on_plateau(),
      # callback_early_stopping(monitor = "val_loss", min_delta = 0.0001, patience = 5, mode = "min"),
      callback_terminate_on_naan()
    )
)

# Compare predictions to training labels for reference ----
library(ggplot2)

tibble::tibble(
  index = 1:200,
  Predicted = as.numeric(predict(model, train_data[1:200,,])),
  Observed = train_labels[1:200]
) %>%
  tidyr::pivot_longer(cols = c(Predicted, Observed)) %>%
  ggplot(aes(x = index, y = value, color = name, fill = name)) +
  geom_path() +
  geom_point(shape = 21, color = "darkgray") +
  scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  labs(
    title = "Comparison of predicted and observed labels",
    x = "Interval Index", y = "Energy Expenditure (kJ)", fill = "", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")
