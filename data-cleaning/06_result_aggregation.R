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
  here::here("output/cross-validation-full", c("LM", "LM0", "RF", "CNN", "DNN", "RNN")),
  ~read_cv_results(.x, latest_only = FALSE)
) %>%
  mutate(
    model_kind = ifelse(model_kind == "RNN", glue::glue("{model_kind} ({res}Hz)"), model_kind),
    model_kind = ifelse(model_kind %in% c("RNN (100Hz)", "RNN (20Hz)"), "RNN (100Hz/20Hz)", model_kind)
  )

saveRDS(full_cv_results, file = here::here("output", "results", "full_cv_results.rds"))

# Prediction comparison ----
library(tidyr)
library(ggplot2)

predictions_single_child <- holdout_results %>%
  unnest(data) %>%
  unnest(predicted_obs) %>%
  filter(model_kind %in% c("CNN", "RF")) %>%
  filter(ID == "003") %>%
  select(model_kind, accel, outcome_unit, interval, Observed = outcome, predicted) %>%
  pivot_wider(
    names_from = c("model_kind"),
    values_from = c("predicted")
  ) %>%
  pivot_longer(
    cols = c(Observed:CNN), names_to = "outcome_type", values_to = "outcome"
  ) %>%
  mutate(outcome_type = factor(outcome_type, levels = c("Observed", "RF", "CNN")))

predictions_single_child %>%
  ggplot(aes(x = interval, y = outcome, color = outcome_type)) +
  facet_grid(
    rows = vars(outcome_unit), cols = vars(accel), scales = "free_y",
    labeller = as_labeller(label_outcome)
  ) +
  geom_path(aes(group = outcome_type), size = 0.5, alpha = 2/3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Predicted and Observed Outcome",
    subtitle = "Randomly selected subject appearing in each holdout set\nModel based on 2/3 of full sample",
    x = "Interval Index", y = "", color = ""
  ) +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")

ggsave(
  plot = last_plot(),
  filename = glue::glue("predicted-observed_ID003_all.png"),
  path = here::here("output/results"),
  width = 14.2, height = 8
)

predictions_single_child %>%
  filter(accel == "Actigraph GT3X (left hip)", outcome_unit == "kJ") %>%
  ggplot(aes(x = interval, y = outcome, color = outcome_type)) +
  geom_path(aes(group = outcome_type), size = 1, alpha = 2/3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Predicted and Observed Outcome: Actigraph GT3X (left hip)",
    subtitle = "Randomly selected subject appearing in each holdout set\nModel based on 2/3 of full sample",
    x = "Interval Index", y = label_outcome("kJ"), color = ""
  ) +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")

ggsave(
  plot = last_plot(),
  filename = glue::glue("predicted-observed_ID003_actigraph_hip_left_kJ.png"),
  path = here::here("output/results"),
  width = 14 * 0.8, height = 8 * 0.8
)
