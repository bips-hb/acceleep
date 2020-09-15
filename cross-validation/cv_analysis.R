library(acceleep)
library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)

# Iterate over all model output folders, read in latest CV runs, bind together
cv_results <- purrr::map_df(
  here::here("output/cross-validation", c("LM", "RF", "DNN", "CNN", "RNN")),
  ~read_cv_results(.x, latest_only = TRUE)
)

cv_results_full <- purrr::map_df(
  here::here("output/cross-validation", c("LM", "RF", "DNN", "CNN", "RNN")),
  ~read_cv_results(.x, latest_only = FALSE)
)

# RF num.trees comparison
cv_results_full %>%
  filter(model_kind == "RF") %>%
  ggplot(aes(x = "", y = mean_rmse, fill = timestamp)) +
  facet_grid(rows = vars(outcome_unit), cols = vars(accel), scales = "free") +
  geom_point(size = 4, alpha = .5, shape = 21, color = "black", position = position_dodge2(width = .5)) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "RF: LOSO-CV RMSE (mean)",
    subtitle = "Blue: Model with num.trees = 500 for all units\nRed: num.trees = 50 for MET, 500 else",
    x = "", y = "Mean RMSE", fill = "Model ID"
  ) +
  hrbrthemes::theme_ipsum_ps() +
  theme(legend.position = "top")


# predict() vs evaluate() ----
cv_results %>%
  tidyr::unnest(data) %>%
  filter(!is.na(eval_rmse)) %>%
  mutate(prediction_diff = (rmse - eval_rmse)^2) %>%
  arrange(desc(prediction_diff)) %>%
  summarize(rmse_rmse = sqrt(mean(prediction_diff)))

cv_results %>%
  tidyr::unnest(data) %>%
  filter(!is.na(eval_rmse)) %>%
  mutate(prediction_diff = rmse - eval_rmse) %>%
  ggplot(aes(x = prediction_diff)) +
  facet_grid(cols = vars(outcome_unit), rows = vars(model_kind), scales = "free") +
  geom_histogram(color = "darkgray", alpha = 2/3) +
  labs(
    title = "Difference in RMSE Estimation",
    subtitle = "Difference in RMSE as calculated by keras::evaluate() vs. manual calculation with predict()",
    x = "Absolute difference in RMSE: predict() - evaluate()",
    y = "Frequency"
  ) +
  hrbrthemes::theme_ipsum_ps(grid = "Y")


# Per model per subject predictions -----

vec_model_kind <- unique(cv_results$model_kind)
vec_accel <- unique(cv_results$accel)
vec_outcome_unit <- unique(cv_results$outcome_unit)

for (current_model_kind in vec_model_kind) {
  for (current_accel in vec_accel) {
    for (current_outcome_unit in vec_outcome_unit) {

      current_result <- cv_results %>%
        filter(
          model_kind == .env$current_model_kind,
          accel == .env$current_accel,
          outcome_unit == .env$current_outcome_unit
        )

      # out_dir <- here::here("output", "cross-validation", current_model_kind, current_result$timestamp, "plots")
      out_dir <- here::here("output", "cross-validation", "plots")
      filename <- glue::glue("k1-cv-predictions-{current_model_kind}-{current_result$accel_id}-{current_result$outcome_unit}-{current_result$res}-{current_result$timestamp}.png")

      cliapp::cli_alert_info("Plotting {current_model_kind} / {current_accel} / {current_outcome_unit} to {filename}")

      current_result %>%
        unnest(data) %>%
        unnest(predicted_obs) %>%
        pivot_longer(cols = c(outcome, predicted)) %>%
        ggplot(aes(x = interval, y = value, color = name)) +
        facet_wrap(~left_out, scales = "free_x") +
        geom_path(key_glyph = "rect") +
        scale_color_brewer(palette = "Dark2", labels = c("outcome" = "Observed", "predicted" = "Predicted")) +
        labs(
          title = glue::glue("{current_model_kind} Per-Subject Predictions for {current_accel}"),
          subtitle = "Predictions based on model for which respective subject is not in training data",
          x = "Interval Index (30s)", y = current_outcome_unit, color = ""
        ) +
        hrbrthemes::theme_ipsum_pub(
          base_size = 14,
          strip_text_size = 11,
          axis_title_size = 13,
          axis_text_size = 10,
          grid = "Y"
        ) +
        theme(legend.position = "top")

      # ggsave(
      #   filename = fs::path(out_dir, fs::path_ext_set(filename, "pdf")),
      #   # device = ragg::agg_png, units = "cm", width = 40, height = 25,
      #   device = cairo_pdf, width = 20, height = 12.5,
      #   dpi = 320
      # )
      ggsave(
        filename = fs::path(out_dir, fs::path_ext_set(filename, "png")),
        device = ragg::agg_png, units = "cm", width = 40, height = 25,
        # device = cairo_pdf, width = 20, height = 12.5,
        dpi = 320
      )

    }
  }
}

#
# ggsave(filename = here::here("LOSO-CV-boxplot-20200907.pdf"), device = cairo_pdf, scale = 2, width = 10, height = 5)
#
#
# filename <- glue::glue("k1-cv-{model_kind}-{metaparams$model}-{metaparams$placement}-{metaparams$outcome}-{metaparams$res}-{run_start}.rds")

