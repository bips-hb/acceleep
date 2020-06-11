# Clean data for analysis use
library(acceleep)
library(dplyr)

files_overview <- get_overview_table()

# Delete files associated with ID 38 -- No usable spiro data
files_overview %>%
  filter(sid == "038") %>%
  select(file_accel, file_spiro) %>%
  tidyr::gather() %>%
  distinct() %>%
  pull(value) %>%
  fs::file_delete()

# Read, clean, and re-save all data, by accelerometer model
# Resulting data will be original-resolution accelerometry joined with spirometry aggregated over 30s intervals
# Intervals are matched using the first timestamp in acceloermetry

# Pre-run cleanup
out_files <- fs::dir_ls(here::here("data", "processed", c("actigraph", "activpal", "geneactiv")))
fs::file_delete(out_files)

# Collect files to read, ignoring missing spiro files
to_read <- files_overview %>%
  filter(!is.na(file_spiro)) %>%
  select(file_accel, file_spiro, sid)

# Adding a progress bar
prog <- cliapp::cli_progress_bar(total = nrow(to_read))

# This will take a while.
purrr::pwalk(to_read, ~{
  prog$tick()
  convert_input_data(input_file_accel = ..1, input_file_spiro = ..2, ID = ..3)
})


purrr::map_df(c("actigraph", "geneactiv", "activpal"), ~{
  tibble::tibble(
    file_clean = fs::dir_ls(here::here("data", "processed", .x)),
    model = .x
  )
})


files_overview %>%
  mutate(
    file_clean = stringr::str_replace(file_accel, "input", "processed"),
    file_clean = fs::path_ext_set(file_clean, ".rds")
  )
