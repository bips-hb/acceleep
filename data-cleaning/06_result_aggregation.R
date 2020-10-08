# Aggregation of results from various validation methods.
# Collect and save results for convenient access in text
library(dplyr)
library(acceleep)

# Directory to save result data to
usethis::use_directory("output/results")

# Holdout results ----
holdout_results <- purrr::map_df(
  here::here("output/holdout-validation", c("LM", "RF", "DNN", "CNN", "RNN")),
  ~read_holdout_results(.x)
) %>%
  mutate(
    model_kind = ifelse(model_kind == "RNN", glue::glue("{model_kind} ({res}Hz)"), model_kind),
    model_kind = ifelse(model_kind %in% c("RNN (100Hz)", "RNN (20Hz)"), "RNN (100Hz/20Hz)", model_kind)
  )


saveRDS(holdout_results, file = here::here("output", "results", "holdout.rds"))

# Development CV ----

current_best <- tibble::tribble(
  ~model_kind, ~timestamp,
  "CNN",  20200921164853,
  "DNN",  20200923144304,
  "RNN (1Hz)",  20200917020316,
  "RNN (10Hz)", 20200919233628,
  "RF",  20200910115426,
  "LM",  20200903163311,
)

# Iterate over all model output folders, read in latest CV runs, bind together
cv_results <- purrr::map_df(
  here::here("output/cross-validation", c("LM", "RF", "DNN", "CNN", "RNN")),
  ~read_cv_results(.x, latest_only = FALSE)
) %>%
  mutate(
    current_best = (timestamp %in% current_best$timestamp),
    model_kind = ifelse(model_kind == "RNN", glue::glue("{model_kind} ({res}Hz)"), model_kind)
  )

saveRDS(cv_results, file = here::here("output", "results", "cv_results.rds"))


# Full CV (incomplete)

full_cv_results <- purrr::map_df(
  here::here("output/cross-validation-full", c("LM", "RF", "CNN", "DNN", "RNN")),
  ~read_cv_results(.x, latest_only = FALSE)
) %>%
  mutate(
    model_kind = ifelse(model_kind == "RNN", glue::glue("{model_kind} ({res}Hz)"), model_kind)
  )

saveRDS(full_cv_results, file = here::here("output", "results", "full_cv_results.rds"))
