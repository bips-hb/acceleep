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
demo <- vroom::vroom(
  file = here::here("data", "input", "demo.csv"),
  col_types = cols(
    ID = "c", sex = "c", hand = "c"
  ),
  col_select = -date
)

demo_summary <- demo %>%
  filter(ID %in% unique(as.numeric(files_overview$sid))) %>%
  group_by(sex) %>%
  summarize_at(vars(height, weight, BMI, hand, age), list(~mean(.x, na.rm = TRUE), ~sd(.x, na.rm = TRUE), ~n()))

save_output_data(demo_summary)

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


# Raw(ish) data example
set.seed(71)

rand_accel_files <- files_overview %>%
  select(model, file_accel) %>%
  group_by(model) %>%
  sample_n(1) %>%
  ungroup()

purrr::map(rand_accel_files$file_accel, ~{
   vroom::vroom(.x, n_max = 3000) %>%
    sample_n(2)
}) %>%
  purrr::set_names(rand_accel_files$model) %>%
  saveRDS(here::here("output/accel_example_lines.rds"))

files_overview %>%
  sample_n(1) %>%
  pull(file_spiro) %>%
  vroom::vroom(n_max = 500) %>%
  sample_n(3) %>%
  select(Time, MET, O2, CO2)
