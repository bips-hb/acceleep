# Taking residual from holdout CNN,
# modeling them based on demography variables
library(dplyr)
library(tidyr)
library(acceleep)+
library(kableExtra)

# Gather holdout results, CNN only
holdout_results <- readRDS(here::here("output", "results", "holdout.rds")) %>%
  filter(model_kind == "CNN")

cnn_results <- holdout_results %>%
  unnest(data) %>%
  unnest(predicted_obs) %>%
  select(accel, model, placement, outcome_unit, ID, interval, outcome, predicted) %>%
  filter(outcome != 0) %>%
  mutate(
    residual = outcome - predicted,
    residual_rel = (outcome - predicted) / outcome
  )

# Merge with demo data (mind the numeric ID)
cnn_results <- get_demo() %>%
  select(-date) %>%
  mutate(ID = sprintf("%03i", ID)) %>%
  inner_join(cnn_results, by = "ID")

# Group by outcome unit, make linear models using both absolute and relative residuals
residual_models <- cnn_results %>%
  mutate(outcome_unit = as.character(outcome_unit)) %>%
  group_by(outcome_unit) %>%
  group_map(~{

    xmd <- lm(residual ~ sex + height + weight + hand + age, data = .x)
    xmd_rel <- lm(residual_rel ~ sex + height + weight + hand + age, data = .x)

    bind_rows(
      broom::tidy(xmd) %>%
        mutate(response = "residual"),
      broom::tidy(xmd_rel) %>%
        mutate(response = "residual/outcome")
    ) %>%
      mutate(
        outcome_unit = .y[[1]],
        p.value = ifelse(p.value < 0.05, "< 0.05", as.character(round(p.value, 2)))
      )

  }) %>%
  bind_rows()

# Save for later
save_output_data(residual_models, "results")

# Tabularize?
residual_models %>%
  select(outcome_unit, response, everything()) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(
    estimate = glue::glue("{estimate} ({std.error}), t={statistic} (p = {p.value})")
  ) %>%
  pivot_wider(
    id_cols = c(outcome_unit, response, term),
    names_from = response,
    values_from = estimate
  ) %>%
  kable() %>%
  kable_styling() %>%
  #collapse_rows(1) %>%
  pack_rows("J/min/kg", 1, 6) %>%
  pack_rows("kJ", 7, 12) %>%
  pack_rows("MET", 13, 18)
