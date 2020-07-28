library(ggplot2)
library(dplyr)
library(acceleep)

full_data <- get_combined_data(model = "geneactiv", placement = "hip_right", res = 1)

full_data %>%
  select(ID, interval, kJ) %>%
  group_by(ID, interval) %>%
  summarize(outcome = unique(kJ), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(index = seq_along(ID)) %>%
  ggplot(aes(x = index, y = outcome)) +
  facet_wrap(~ID) +
  geom_path() +
  theme_minimal()

## from keras_prep_lstm

# Split into train / validation datasets
c(training_data, validation_data) %<-% make_initial_splits(
  full_data, random_seed = 11235813, val_split = 1/3
)

# Split into data and labels
split_data <- split_data_labels(training_data, validation_data, outcome = "kJ")

c(train_data, train_labels) %<-% split_data$training
c(test_data, test_labels) %<-% split_data$validation

# Reshaping to array form
train_data <- keras_reshape_accel(
  accel_tbl = train_data, interval_length = 30, res = 1
)
test_data <- keras_reshape_accel(
  accel_tbl = test_data, interval_length = 30, res = 1
)
