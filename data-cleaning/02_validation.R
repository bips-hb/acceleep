# Trying to calidate cleaned up data is correct
library(dplyr)
library(acceleep)

files_overview <- get_overview_table()

test_comb <- combine_clean_data("actigraph", "hip_right")


# Intervals that are not of expected length (3000 = 30 * 100)
test_comb %>%
  count(ID, interval) %>%
  filter(n != 3000)

# Intervals per subject
test_comb %>%
  count(ID, sort = TRUE) %>%
  summarize(
    min = min(n), max = max(n), mean = mean(n), median = median(n), n_subj = n_distinct(ID)
  )


# Get number of measurements per model/placement
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


# Investigate by subject ----
clean_file_checkup <- files_overview %>%
  filter(file_clean_exists) %>%
  select(model, placement, file_clean) %>%
  purrr::pmap_df(~{
    xdf <- readRDS(..3)

    # res <- ifelse(model == "activpal", 20, 100)

    tibble::tibble(
      file = ..3,
      model = ..1,
      placement = ..2,
      rows = nrow(xdf),
      intervals = list(count(xdf, interval)),
      intervals_n = n_distinct(xdf$interval),
      interval_first = min(xdf$interval),
      interval_last = max(xdf$interval),
      missings = list(purrr::map_int(xdf, ~sum(is.na(.x))))
    )

  })


