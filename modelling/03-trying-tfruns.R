# Experimenting with tfruns for better organization
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)

training_run(here::here("modelling/train_model.R"))

# Default flags:
list(
  model = "geneactiv",
  placement = "hip_right",
  outcome = "kJ",
  res = 100,
  lr = 0.001,
  decay = 0,
  batch_size = 32,
  epochs = 20,
  lstm_layers = 1,
  lstm_units = 128,
  dense_layers = 0,
  dense_units = 128,
  droput_rate = 0.2,
  validation_split = 0.2,
  verbose = 0,
  shuffle = TRUE
)

# Now with flags
tuning_runs <- tuning_run(
  confirm = FALSE,
  here::here("modelling/train_model.R"),
  flags = list(
    dropout_rate = c(0.2),
    batch_size = c(16),
    decay = 0.001,
    epochs = 30,
    shuffle = c(TRUE, FALSE)
    # dense_units = c(0, 1)
))
# pushoverr::pushover("Runs are done!", title = "Modelling Hell")

View(tuning_runs)

# Check latest run
latest_run()

# Get specific run
view_run("runs/2020-07-22T14-38-44Z/")


compare_runs()

# All the runs
ls_runs()
ls_runs() %>%
  as_tibble() %>%
  arrange(metric_val_loss)

compare_runs(ls_runs())

clean_runs(ls_runs(metric_val_loss > 10))
clean_runs(ls_runs(is.na(metric_val_loss)))

# pecking for loss
training_run(here::here("modelling/train_model.R"), flags = list(
  batch_size = 64,
  decay = 1e-3,
  epochs = 5,
  shuffle = FALSE
))

sqrt(latest_run()$metrics$val_loss)

# Looking at overnight runs -----
options(tfruns.runs_dir = here::here("output/runs/tfruns-overnight"))


