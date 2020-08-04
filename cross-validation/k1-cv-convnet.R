library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate:::ensure_python_initialized()

# Declaring metadata ----
model_kind <- "CNN"
accel_model <- "geneactiv"
placement <- "hip_right"
outcome <- "kJ"
res <- 100


# Collecting original training data only tpo get it's subject IDs
# resolution is small here because it doesn't matter, actual training data is read later
c(c(train_data_full, train_labels_full), c(., .)) %<-% keras_prep_lstm(
  model = accel_model, placement = placement,
  outcome = outcome, random_seed = 19283, val_split = 1/3,
  interval_length = 30, res = 1
)

IDs_full <- train_data_full %>%
  pull(.data$ID) %>%
  unique() %>%
  sort()

# Initialize empty tibble to collect results
cv_result <- tibble::tibble(.rows = length(IDs_full))

prog <- cli_progress_bar(
  format = ":current of :total [:bar] (:percent, :elapsedfull)",
  total = length(IDs_full)
)

# Loop over subject IDs ----
# leave out one for each model run
for (i in IDs_full) {
  prog$tick()

  # Dataprep ----
  # Get the entire dataset (again)
  full_data <- get_combined_data(model = accel_model, placement = placement, res = res)

  # Split into train / validation datasets based on subject IDs
  training_data <- full_data %>%
    filter(.data$ID %in% IDs_full, .data$ID != i)

  validation_data <- full_data %>%
    filter(.data$ID == i)

  # Check
  unique(training_data$ID)
  unique(validation_data$ID)

  # Normalize
  c(training_data, validation_data) %<-% normalize_accelerometry(training_data, validation_data)


  # Split into data and labels
  split_data <- split_data_labels(training_data, validation_data, outcome = outcome)

  c(train_data, train_labels) %<-% split_data$training
  c(test_data, test_labels) %<-% split_data$validation

  # Reshaping to array form, keep train_data_full for later prediction
  train_data_array <- keras_reshape_accel(
    accel_tbl = train_data, interval_length = 30, res = res
  )

  test_data_array <- keras_reshape_accel(
    accel_tbl = test_data, interval_length = 30, res = res
  )

  dim(train_data)
  dim(train_data_array)

  dim(test_data)
  dim(test_data_array)

  length(train_labels)
  length(test_labels)

  # Modelling ----

  strategy <- tensorflow::tf$distribute$MirroredStrategy(devices = NULL)

  with(strategy$scope(), {
    model <- keras_model_sequential() %>%
      layer_conv_1d(
        filters = 64, kernel_size = 18, activation = "relu",
        input_shape = dim(train_data_array)[c(2, 3)]
      )  %>%
      layer_batch_normalization() %>%
      layer_max_pooling_1d(pool_size = 100) %>%
      layer_conv_1d(
        filters = 64, kernel_size = 18, activation = "relu"
      )  %>%
      layer_batch_normalization() %>%
      # layer_max_pooling_1d(pool_size = 100) %>%
      layer_global_max_pooling_1d() %>%
      layer_dense(activation = "relu", units = 64)  %>%
      layer_batch_normalization() %>%
      layer_dropout(rate = 0.2)  %>%
      layer_batch_normalization() %>%
      layer_dense(activation = "relu", units = 64) %>%
      layer_dropout(rate = 0.2) %>%
      layer_dense(units = 1, name = "output")
  })

  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 1e-3),
    metrics = "mae"
  )

  history <- model %>% fit(
    train_data_array,
    train_labels,
    batch_size = 32,
    epochs = 50,
    validation_split = 0,
    verbose = 0
  )

  eval_result <- model %>%
    evaluate(test_data_array, test_labels, verbose = 0)

  current_result <- tibble::tibble(
    left_out = i,
    rmse = sqrt(eval_result[["loss"]]),
    mse = eval_result[["loss"]],
    mae = eval_result[["mae"]]
  )

  cv_result <- bind_rows(cv_result, current_result)
}


# Save result tibble
filename <- glue::glue("k1-cv-{model_kind}-{accel_model}-{placement}-{outcome}-{res}-{format(Sys.time(), '%Y%m%d%H%M%S')}.rds")

out_dir <- here::here("output", "cross-validation")
if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

# Save CV RMSE results
saveRDS(object = cv_result, file = fs::path(out_dir, filename))

# Write model structure to plain text file
writeLines(text = summary(model), fs::path_ext_set(filename, "model"))

pushoverr::pushover("Cross validation is done!", title = "Modelling Hell")

