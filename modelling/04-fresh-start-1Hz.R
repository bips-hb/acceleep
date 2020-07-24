# Starting "fresh" with lower resolution data
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)
options(tfruns.runs_dir = here::here("output/runs/downsampled-1Hz"))


# c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
#   model = "geneactiv", placement = "hip_right",
#   outcome = "kJ", random_seed = 19283, val_split = 1/3,
#   interval_length = 30, res = 1
# )


# Default flags:
list(
  model = "geneactiv",
  placement = "hip_right",
  outcome = "kJ",
  res = 1,
  lr = 0.001,
  decay = 0,
  batch_size = 32,
  epochs = 20,
  lstm_layers = 2,
  lstm_units = 128,
  dense_layers = 2,
  dense_units = 128,
  droput_rate = 0,
  validation_split = 0.2,
  verbose = 0,
  shuffle = TRUE
)


training_run(here::here("modelling/train_model.R"), flags = list(
  model = "geneactiv",
  placement = "hip_right",
  outcome = "kJ",
  res = 1,
  lr = 0.001,
  decay = 0,
  batch_size = 32,
  epochs = 30,
  lstm_layers = 2,
  lstm_units = 128,
  dense_layers = 2,
  dense_units = 128,
  dropout_rate = 0,
  validation_split = 0.2,
  verbose = 1
))

# Now with flags
tuning_runs <- tuning_run(
  confirm = FALSE,
  here::here("modelling/train_model.R"),
  flags = list(
    lstm_layers = c(2),
    lstm_units = c(128),
    dense_layers = c(2),
    dense_units = c(128),
    dropout_rate = c(0.2),
    batch_size = c(32),
    decay = 0,
    epochs = 30
  ))
# pushoverr::pushover("Runs are done!", title = "Modelling Hell")

