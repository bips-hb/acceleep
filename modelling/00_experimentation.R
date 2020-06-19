# Tentative experimentation
library(dplyr)
library(acceleep)

dat_activpal <- combine_clean_data("activpal", "thigh_right")

# Extract subject ids present in data and assign them to test / validation sets
ids <- unique(dat_activpal$ID)

set.seed(11235813)
ids_validation <- sample(ids, size = floor(length(ids)/3)) # 1/3 of IDs
ids_train <- ids[!(ids %in% ids_validation)]

# Stop if somehow train + test ids are not the same as the initial ids
stopifnot(
  identical(sort(c(ids_validation, ids_train)), sort(ids))
)

# split data into train/validation sets
dat_validation <- dat_activpal %>%
  filter(ID %in% ids_validation)

dat_training <- dat_activpal %>%
  filter(ID %in% ids_train)

# Training data and labels
array_train <- dat_training %>%
  select(X, Y, Z) %>%
  mutate_all(~as.numeric(scale(.x))) %>%
  convert_tbl_array(interval_length = 30, res = 20)

labels_train <- as.numeric(scale(extract_outcome(dat_training, outcome = "MET", output_type = "numeric")))

# Validation data and labels
array_validation <- dat_validation %>%
  select(X, Y, Z) %>%
  mutate_all(~as.numeric(scale(.x))) %>%
  convert_tbl_array(interval_length = 30, res = 20)

labels_validation <- as.numeric(scale(extract_outcome(dat_validation, outcome = "MET", output_type = "numeric")))


# Doing the keras -----
dim(array_train) # c(2588, 3, 600)

# array_train <- array_reshape(array_train, c(2588, 3, 600))
# dim(array_train)

library(keras)

# "Normal" regression model: Output is a matrix :(
# model <- keras_model_sequential() %>%
#   layer_dense(units = 64, activation = "relu", input_shape = c(NULL)) %>%
#   layer_dense(units = 64, activation = "relu") %>%
#   layer_dense(units = 1, activation = "linear", name = "output")

# LSTM model: Correct output shape!
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

# plot(history)


model %>% evaluate(array_validation, labels_validation, verbose = 0)

# some predicted vs. original values
predict(model, array_train)[1:20, 1]

labels_train[1:20]
