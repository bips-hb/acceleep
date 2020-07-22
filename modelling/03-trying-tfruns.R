# Experimenting with tfruns for better organization
# Main model script: "train_model.R"
# See https://tensorflow.rstudio.com/tools/tfruns/overview/

source(here::here("modelling/_init.R"))
library(tfruns)

training_run(here::here("modelling/train_model.R"))

# Now with flags
tuning_runs <- tuning_run(here::here("modelling/train_model.R"), flags = list(
  dropout_rate = c(0, 0.2),
  batch_size = c(16, 8)
))

View(tuning_runs)

# Check latest run
latest_run()

# Get specific run
view_run("runs/2020-07-22T13-22-30Z/")


compare_runs()

# All the runs
ls_runs()
ls_runs(order = metric_val_loss) %>%
  as_tibble()

compare_runs(ls_runs())

# pecking for loss
training_run(here::here("modelling/train_model.R"), flags = list(
  batch_size = 8,
  decay = 1e-3
))

latest_run()

