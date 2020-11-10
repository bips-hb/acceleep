# Collect summaries for reporting
library(acceleep)
library(dplyr)

files_overview <- get_overview_table()
save_output_data(files_overview)

files_overview %>%
  count(model, placement)

# IDs without data, e.g. 38 (no usable spiro data)
files_overview %>%
  filter(!is.na(file_spiro)) %>%
  select(sid) %>%
  distinct() %>%
  mutate(sid = as.numeric(sid)) %>%
  anti_join(tibble(sid = 1:44), .)

# Real N == 40, consistent with paper at least.
files_overview %>%
  filter(!is.na(file_spiro)) %>%
  select(sid) %>%
  distinct() %>%
  nrow()

# Demographics ----
demo <- get_demo() %>%
  inner_join(
    # Subset demo to keep only IDs that have usable data
    files_overview %>% filter(file_clean_exists) %>% transmute(ID = as.numeric(sid)) %>% distinct()
  )


# demo_summary <- demo %>%
#   # group_by(sex) %>%
#   summarize_at(
#     vars(height, weight, BMI, hand, age),
#     list(~mean(.x, na.rm = TRUE), ~sd(.x, na.rm = TRUE), ~median(.x, na.rm = TRUE))
#   ) %>%
#   mutate(n = nrow(demo))

demo_summary <- demo %>%
  tidyr::pivot_longer(
    cols = c(sex, height, weight, BMI, hand, age)
  ) %>%
  group_by(name) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),#
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, na.rm = TRUE, prob = 0.75),
    q75 = mean(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    missing = sum(is.na(value)),
    .groups = "drop"
  )

demo_summary_bysex <- demo %>%
  tidyr::pivot_longer(
    cols = c(height, weight, BMI, hand, age)
  ) %>%
  group_by(sex, name) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),#
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, na.rm = TRUE, prob = 0.75),
    q75 = mean(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    missing = sum(is.na(value)),
    .groups = "drop"
  )

save_output_data(demo_summary)
save_output_data(demo_summary_bysex)

# Models/placements -----

files_overview %>%
  group_by(sid, model) %>%
  count(model) %>%
  tidyr::spread(model, n) %>%
  View()

files_overview %>%
  group_by(sid, model) %>%
  count(model) %>%
  group_by(model) %>%
  count(wt = n)

# Real N by accel/placement
files_overview %>%
  count(model, placement)


# Raw(ish) data example
set.seed(71)

rand_accel_files <- files_overview %>%
  select(model, file_accel) %>%
  group_by(model) %>%
  sample_n(1) %>%
  ungroup()


files_overview %>%
  sample_n(1) %>%
  pull(file_spiro) %>%
  vroom::vroom(n_max = 500) %>%
  sample_n(3) %>%
  select(Time, MET, O2, CO2)
