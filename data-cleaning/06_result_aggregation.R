# Aggregation of results from various validation methods.
# Collect and save results for convenient access in text
library(dplyr)
library(acceleep)

# Directory to save result data to
usethis::use_directory("output/results")

# Holdout results ----
holdout_results <- purrr::map_df(
  here::here("output/holdout-validation", c("LM", "LM0", "RF", "DNN", "CNN", "RNN")),
  ~read_holdout_results(.x)
) %>%
  mutate(
    model_kind = ifelse(model_kind == "LM0", "Null Model", model_kind),
    model_kind = ifelse(model_kind == "RNN", glue::glue("{model_kind} ({res}Hz)"), model_kind),
    model_kind = ifelse(model_kind %in% c("RNN (100Hz)", "RNN (20Hz)"), "RNN (100Hz/20Hz)", model_kind)
  )

# After-the-fact per-subject median RMSE
xdf <- holdout_results %>%
  tidyr::unnest(data) %>%
  tidyr::unnest(predicted_obs) %>%
  group_by(model_kind, accel, outcome_unit, ID) %>%
  summarize(
    rmse_persubj = mean(sqrt((outcome - predicted)^2)),
    .groups = "drop"
  ) %>%
  group_by(model_kind, accel, outcome_unit) %>%
  summarize(
    mean_rmse = mean(rmse_persubj),
    sd_rmse = sd(rmse_persubj),
    median_rmse = median(rmse_persubj),
    .groups = "drop"
  )

holdout_results <- full_join(
  holdout_results,
  xdf %>%
    select(model_kind, accel, outcome_unit, mean_rmse, sd_rmse, median_rmse),
  by = c("model_kind", "accel", "outcome_unit")
)


saveRDS(holdout_results, file = here::here("output", "results", "holdout.rds"))
rm(xdf)

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
    model_kind = ifelse(model_kind == "LM0", "Null Model", model_kind),
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

p_all <- predictions_single_child %>%
  ggplot(aes(x = (interval * 30)/60, y = outcome, color = outcome_type)) +
  facet_grid(
    rows = vars(outcome_unit), cols = vars(accel), scales = "free_y",
    labeller = as_labeller(label_outcome)
  ) +
  geom_path(aes(group = outcome_type), size = 0.5, alpha = 2/3) +
  scale_color_manual(values = c(Observed = "black", CNN = "#1B9E77", RF = "#D95F02")) +
  labs(
    title = "Predicted and Observed Outcome",
    subtitle = "Randomly selected subject appearing in each holdout set\nModels based on 2/3 of full sample",
    x = "Time (m)", y = "", color = ""
  ) +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")

ggsave(
  plot = p_all,
  device = cairo_pdf,
  filename = glue::glue("predicted-observed_ID003_all.pdf"),
  path = here::here("output/results"),
  width = 16, height = 8
)

p_single <- predictions_single_child %>%
  filter(accel == "Actigraph GT3X (left hip)", outcome_unit == "kJ") %>%
  ggplot(aes(x = (interval * 30)/60, y = outcome, color = outcome_type)) +
  geom_path(aes(group = outcome_type), size = 1, alpha = 2/3) +
  scale_color_manual(values = c(Observed = "black", CNN = "#1B9E77", RF = "#D95F02")) +
  labs(
    title = "Predicted and Observed Outcome: Actigraph GT3X (left hip)",
    subtitle = "Randomly selected subject appearing in each holdout set\nModel based on 2/3 of full sample",
    x = "Time (m)", y = label_outcome("kJ"), color = ""
  ) +
  tadaathemes::theme_ipsum_ss() +
  theme(legend.position = "top")

ggsave(
  plot = p_single,
  device = cairo_pdf,
  filename = glue::glue("predicted-observed_ID003_actigraph_hip_left_kJ.pdf"),
  path = here::here("output/results"),
  width = 14 * 0.8, height = 8 * 0.8
)

# Prediction bias? ----

holdout_residuals <- holdout_results %>%
  tidyr::unnest(data) %>%
  select(model_kind, model, placement, accel, outcome_unit, res, predicted_obs) %>%
  tidyr::unnest(predicted_obs) %>%
  mutate(
    residual = outcome - predicted,
    residual_rel = (residual)/outcome
  )

# Tabular residuals
holdout_residuals %>%
  group_by(model_kind, accel, outcome_unit) %>%
  summarize(
    mean_residual = mean(residual),
    mean_residual_rel = mean(residual_rel[is.finite(residual_rel)])
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(model_kind, outcome_unit, accel), names_from = model_kind, values_from = mean_residual
  ) %>%
  arrange(outcome_unit, accel)

# residual histograms?
holdout_residuals %>%
  ggplot(aes(x = residual)) +
  facet_grid(
    cols = vars(outcome_unit), rows = vars(model_kind), scales = "free_x",
    labeller = as_labeller(label_outcome)
  ) +
  geom_histogram(aes(y = stat(ndensity)), color = "gray", alpha = .5, bins = 40) +
  geom_density(aes(y = stat(scaled)), alpha = .5, size = .75) +
  geom_vline(xintercept = 0, lty = "52", size = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "LOSO-CV: Raw Residuals",
    subtitle = "Distribution of residuals from all predictions made during full-sample LOSO-CV",
    x = "Residual = Observed - Predicted", y = "Frequency (scaled)"
  ) +
  tadaathemes::theme_ipsum_ss()


# non-wear predictions?

holdout_residuals %>%
  filter(outcome == 0) %>%
  ggplot(aes(x = residual)) +
  facet_grid(
    cols = vars(outcome_unit), rows = vars(model_kind), scales = "free_x",
    labeller = as_labeller(label_outcome)
  ) +
  geom_histogram(aes(y = stat(ndensity)), color = "gray", alpha = .5, bins = 40) +
  geom_density(aes(y = stat(scaled)), alpha = .5, size = .75) +
  geom_vline(xintercept = 0, lty = "52", size = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "LOSO-CV: Raw Residuals for non-wear intervals (EE = 0)",
    subtitle = "Distribution of residuals from all predictions made during full-sample LOSO-CV",
    x = "Residual = Observed - Predicted", y = "Frequency (scaled)"
  ) +
  tadaathemes::theme_ipsum_ss()

# Predicted vs Observed scatterplot

predicted_observed_scatter <- function(holdout_residuals, model_kind = "CNN", outcome_unit = "kJ") {
  require(rlang)
  #browser()

  xdf <- holdout_residuals %>%
    filter(.data$model_kind == .env$model_kind) %>%
    filter(.data$outcome_unit == .env$outcome_unit)

  xyrange <- range(floor(xdf$outcome), ceiling(xdf$predicted) + 1)

  xdf %>%
    #filter(ID %in% c("042", "043")) %>%
    ggplot(aes(x = outcome, y = predicted)) + #, fill = ID)) +
    coord_cartesian(
      xlim = xyrange,
      ylim = xyrange
    ) +
    geom_point(shape = 21, alpha = .5, color = "black") +
    geom_abline(color = "black", size = 1) +
    labs(
      title = glue::glue("{model_kind}: Predicted vs. Observed"),
      subtitle = "Hold-out validation models. Reference line (y = x)",
      x = glue::glue("Observed ({label_outcome(outcome_unit)})"),
      y = glue::glue("Predicted ({label_outcome(outcome_unit)})")
    ) +
    tadaathemes::theme_ipsum_ss()
}

predicted_observed_scatter(holdout_residuals, "CNN", "Jrel")
predicted_observed_scatter(holdout_residuals, "DNN", "kJ")
predicted_observed_scatter(holdout_residuals, "RF", "kJ")
predicted_observed_scatter(holdout_residuals, "RNN (1Hz)", "kJ")
predicted_observed_scatter(holdout_residuals, "RNN (10Hz)", "kJ")
predicted_observed_scatter(holdout_residuals, "RNN (100Hz/20Hz)", "kJ")


tidyr::crossing(
  model_kind = c("CNN", "DNN", "RF", "RNN (1Hz)", "RNN (10Hz)", "RNN (100Hz/20Hz)"),
  outcome_unit = c("kJ", "MET", "Jrel")
) %>%
  purrr::pwalk(~{
    model_kind <- ..1
    outcome_unit <- ..2
    #browser()
    p <- predicted_observed_scatter(holdout_residuals, model_kind, outcome_unit)

    model_kind <- model_kind %>%
      stringr::str_replace_all("\\(|\\)|\\/|\\s", "")

    ggsave(
      plot = p,
      filename = glue::glue("{model_kind}_observed-predicted-{outcome_unit}.png"),
      #device = cairo_pdf,
      path = here::here("output/results/predictions"),
      width = 8, height = 6
    )
  })

# Completed epochs ----
library(ggplot2)
library(dplyr)
library(acceleep)
fullcv <- readRDS(here::here("output/results/full_cv_results.rds"))

fullcv %>%
  filter(model_kind == "CNN") %>%
  tidyr::unnest(data) %>%
  group_by(model, placement, outcome_unit) %>%
  summarize(
    min = min(epochs_completed),
    mean = mean(epochs_completed),
    median = median(epochs_completed),
    max = max(epochs_completed),
    .groups = "drop"
  )


fullcv %>%
  filter(model_kind == "CNN") %>%
  tidyr::unnest(data) %>%
  ggplot(aes(x = paste(label_accel_models(model), label_placement(placement), sep = "\n"), y = epochs_completed)) +
  facet_wrap(vars(outcome_unit), labeller = as_labeller(label_outcome)) +
  geom_boxplot() +
  labs(
    title = "Completed Epochs During LOSO-CV",
    subtitle = "Number of epochs limited by early-stopping callback",
    x = "Accelerometer", y = "Completed Epochs"
  ) +
  tadaathemes::theme_ipsum_ss()

