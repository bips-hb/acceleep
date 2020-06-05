# Trying with keras ----
library(keras)
library(acceleep)

res <- 20             # Starting wit 20Hz instead of 100Hz or simplicity
interval_length <- 30 # 30 seconds seems reasonable due to MET limitation
n_children <- 50      #  50 "children"
n_chunks <- 120       # Arbitrarily chosen, setting 120 results in 1 hour of simulated data
n_chunks_total <- n_children * n_chunks

# Generate 10 consecutive chunks at 20Hz, 30s per chunk
accel_sim_tbl <- generate_ts_dataset(
  res = res,
  interval_length = interval_length,
  n_chunks = n_chunks_total
)

accel_train <- convert_tbl_array(accel_sim_tbl[-1], interval_length, res)
dim(accel_train) # 50 * n_chunks

# Define labels (METs)
train_labels <- rnorm(n_chunks_total, 0, 1) # Pretend we standardized them


# Define regression model

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1)

# Define LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(activation = "tanh", units = 128) %>%
  layer_dense(units = 1)

# Run model
model %>% compile(
  optimizer = optimizer_adam(lr = 0.1),
  loss = "mse",
  metrics = "mae"
)

history <- model %>% fit(
  accel_train,
  train_labels,
  batch_size = 16,
  validation_split = 0.2
)

