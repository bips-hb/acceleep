# Tentative experimentation
# Goel so far: Make sure the modelling works in practice (re: data structure, basic concept)
# Actual validation loss reduction is not a concern yet
library(dplyr)
library(acceleep)
library(keras)

# Assemble all activpal data
dat_activpal <- combine_clean_data("activpal", "thigh_right")

# Split into train/validation (1/3 of subjects remain for the validation data)
c(training_data, validation_data) %<-% make_initial_splits(dat_activpal, random_seed = 11235813, val_split = 1/3)


# Training data and labels
# The scaling is done wrong, i.e. not using mean/sd from training data
array_train <- training_data %>%
  select(X, Y, Z) %>%
  mutate_all(~as.numeric(scale(.x))) %>%
  convert_tbl_array(interval_length = 30, res = 20)

labels_train <- as.numeric(scale(extract_outcome(training_data, outcome = "MET", output_type = "numeric")))

# Validation data and labels
array_validation <- validation_data %>%
  select(X, Y, Z) %>%
  mutate_all(~as.numeric(scale(.x))) %>%
  convert_tbl_array(interval_length = 30, res = 20)

labels_validation <- extract_outcome(validation_data, outcome = "MET", output_type = "numeric", rescale = TRUE)


# Doing the keras -----
dim(array_train) # c(2588, 3, 600)

# array_train <- array_reshape(array_train, c(2588, 3, 600))
# dim(array_train)

# "Normal" regression model: Output is a matrix :(
# model <- keras_model_sequential() %>%
#   layer_dense(units = 64, activation = "relu", input_shape = c(NULL)) %>%
#   layer_dense(units = 64, activation = "relu") %>%
#   layer_dense(units = 1, activation = "linear", name = "output")

# LSTM model: Correct output shape! ----
model <- keras_model_sequential() %>%
  layer_lstm(activation = "tanh", units = 128, return_sequences = TRUE) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_lstm(activation = "tanh", units = 128) %>%
  layer_dense(units = 1, name = "output")

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

history <- model %>% fit(
  array_train,
  labels_train,
  epochs = 20,
  validation_split = 0.2,
  verbose = 1
)

summary(model)

history

p_hist <- plot(history)
ggplot2::ggsave(filename = here::here("output", "model_history_lstm_array_input.png"))

model %>% evaluate(array_validation, labels_validation, verbose = 0)

# some predicted vs. original values
predict(model, array_train)[1:20, 1]

labels_train[1:20]

# Regression model with tbl input maybe? ----
# Since regression with the array input produced wonky results, trying this just in case
# This should be somewhat wonky because we're setting the same label for each set of 600 "observations"

# Matrix with scales XYZ
train_dat_matrix <- training_data %>%
  select(X, Y, Z) %>%
  mutate_all(~as.numeric(scale(.x))) %>%
  as.matrix()

# One MET per *row*, not per interval
train_labels <- training_data$MET

# "Normal" regression model but tbl/matrix input
model_reg_mat <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = 3) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear", name = "output")

model_reg_mat %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

history_reg_mat <- model_reg_mat %>% fit(
  train_dat_matrix,
  train_labels,
  epochs = 20,
  validation_split = 0.2,
  verbose = 1
)
# Training takes approx 30 sek/epoch @ M's machine
# Validation loss does not look good (not too surprising givin the toy layers alone)

summary(model_reg_mat)

history_reg_mat

p_hist <- plot(history_reg_mat)
ggplot2::ggsave(filename = here::here("output", "model_history_regression_matrix_input.png"))
