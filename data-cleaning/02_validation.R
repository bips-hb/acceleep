# Trying to calidate cleaned up data is correct
library(dplyr)
library(acceleep)

files_overview <- get_overview_table()

test_comb <- files_overview %>%
  filter(model == "geneactiv", placement == "hip_right", file_clean_exists) %>%
  pull(file_clean) %>%
  purrr::map_df(~{
    readRDS(.x)
  })

test_comb %>%
 head(1000) %>%
  mutate(
    interval_id = glue::glue("{interval}_{ID}")
  ) %>%
  group_by(interval_id) %>%
  summarize(
    n_rows = n()
  ) %>%
  pull(n_rows) %>%
  table()

# Intervals that are not of expected length (3000 = 30 * 100)
test_comb %>%
  count(ID, interval) %>%
  filter(n != 3000)

# Intervals per subject
test_comb %>%
  count(ID) %>%
  summarize(
    min = min(n), max = max(n), mean = mean(n), median = median(n), n_subj = n_distinct(ID)
  )


# Get number of intervals per model/placement
summarize_measure_counts <- function(model, placement) {
  files_overview %>%
    filter(file_clean_exists, model == !!model, placement == !!placement) %>%
    pull(file_clean) %>%
    purrr::map_df(readRDS) %>%
    count(ID) %>%
    summarize(
      min = min(n), max = max(n), mean = mean(n), median = median(n), n_subj = n_distinct(ID)
    ) %>%
    mutate(model = model, placement = placement)
}

# summarize_measure_counts("actigraph", "hip_left")

files_overview %>%
  select(model, placement) %>%
  distinct() %>%
  purrr::pmap_df(summarize_measure_counts) -> measurements_per_accel

save_output_data(measurements_per_accel)
