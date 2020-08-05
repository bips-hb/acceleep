library(acceleep)
library(ggplot2)
library(dplyr)

rf_results_paper <- tibble::tribble(
  ~outcome, ~model,      ~placement,    ~mean_rmse, ~sd_rmse,
  "kJ",     "actigraph", "hip_left",    2.6,        0.97,
  "Jrel",   "actigraph", "hip_left",    112.32,     28.40,
  "MET",    "actigraph", "hip_left",    1.52,       0.38,
  "kJ",     "actigraph", "hip_right",   2.74,       0.96,
  "Jrel",   "actigraph", "hip_right",   115.56,     27.35,
  "MET",    "actigraph", "hip_right",   1.56,       0.36,
  "kJ",     "geneactiv", "hip_right",   2.73,       1,
  "Jrel",   "geneactiv", "hip_right",   112.57,     28.83,
  "MET",    "geneactiv", "hip_right",   1.53,       0.38,
  "kJ",     "geneactiv", "wrist_left",  2.56,       0.83,
  "Jrel",   "geneactiv", "wrist_left",  108.64,     26.33,
  "MET",    "geneactiv", "wrist_left",  1.47,       0.36,
  "kJ",     "geneactiv", "wrist_right", 2.56,       0.83,
  "Jrel",   "geneactiv", "wrist_right", 109.34,     26.98,
  "MET",    "geneactiv", "wrist_right", 1.48,       0.37
)

cv_files <- fs::dir_ls(here::here("output/cross-validation"), glob = "*.rds")

results <- purrr::map_df(cv_files, ~{
  tibble::tibble(
    file = .x,
    data = list(readRDS(.x) %>% filter(!is.na(left_out)))
  )
}) %>%
  mutate(
    file = fs::path_file(file) %>% fs::path_ext_remove(),
    mean_rmse = purrr::map_dbl(data, ~mean(.x$rmse)),
    median_rmse = purrr::map_dbl(data, ~median(.x$rmse)),
    sd_rmse = purrr::map_dbl(data, ~sd(.x$rmse))
  ) %>%
  tidyr::separate(
    col = file,
    into = c("folds", "method", "model_kind", "model", "placement", "outcome", "res", "timestamp"),
    sep = "-"
  ) %>%
  filter(timestamp != "20200804163926") %>%
  mutate(
    model = label_accel_models(model),
    placement = purrr::map_chr(placement, label_placement),
    accel = glue::glue("{model} ({placement})")
  )

# A plot ----
results %>%
  tidyr::unnest(data) %>% View()
  ggplot(aes(x = accel, y = rmse)) +
  facet_grid(rows = vars(outcome), scales = "free_y") +
  geom_boxplot() +
  theme_minimal()

# A table ----
results %>%
  transmute(
    accel = accel,
    outcome = outcome,
    measure = glue::glue("{round(mean_rmse, 2)} ({round(sd_rmse, 2)})") %>% as.character()
  ) %>%
  tidyr::pivot_wider(names_from = accel, values_from = measure) %>%
  slice(c(3, 1, 2)) %>%
  knitr::kable(caption = "First LOSO-CV results (CNN), Mean RMSE (SD)") %>%
  kableExtra::kable_styling()



