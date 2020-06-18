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


# Investigate by subject ----
clean_file_checkup <- files_overview %>%
  filter(file_clean_exists) %>%
  select(model, placement, file_clean) %>%
  purrr::pmap_df(~{
    xdf <- readRDS(..3)

    tibble::tibble(
      file_clean = ..3,
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

clean_file_checkup <- files_overview %>%
  full_join(clean_file_checkup, by = c("file_clean", "model", "placement"))

save_output_data(clean_file_checkup)

# Manual investigation afterwards

clean_file_checkup %>%
  arrange(rows)

clean_file_checkup %>%
  arrange(intervals_n)

clean_file_checkup %>%
  pull(missings) %>%
  bind_rows() %>%
  bind_cols(
    clean_file_checkup %>%
      select(file, rows, intervals_n)
  ) %>%
  View()

# 006 activpal: Not enough overlapping measurements beetwen spiro & accel
# -> excluded
readRDS("data/processed/activpal/006_ActivPal_readable.rds") %>% View()
file.remove("data/processed/activpal/006_ActivPal_readable.rds")

# Relatively little data but at least complete?
readRDS("data/processed/activpal/040_ActivPal_readable.rds") %>% View()

# Some missing accel data, but not a major issue
readRDS("data/processed/geneactiv/026_wrist_right_readable.rds") %>%
  count(interval) %>%
  filter(n == 1)

