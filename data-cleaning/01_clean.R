# Clean data for analysis use
library(acceleep)
library(dplyr)

# Disable progress bar fro vroom::vroom
options(readr.show_progress = FALSE)

files_overview <- get_overview_table()

# Delete files associated with ID 38 -- No usable spiro data
# files_overview %>%
#   filter(sid == "038") %>%
#   select(file_accel, file_spiro) %>%
#   tidyr::gather() %>%
#   distinct() %>%
#   pull(value) %>%
#   fs::file_delete()

# Accelerometry files without corresponding spiro data (i.e. children without spiro measurements)
# are moved to the exclusion directory of shame
# Only needs to be done once, commented out in case of accidents
# files_overview %>%
#   filter(is.na(file_spiro)) %>%
#   pull(file_accel) %>%
#   fs::file_move(here::here("data/excluded/"))

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

# Adding a progress bar (somehow buggy, maybe vroom progress is not suppressed correctly)
prog <- cliapp::cli_progress_bar(total = nrow(to_read))

# This will take a while.
purrr::pwalk(to_read, ~{
  prog$tick()
  convert_input_data(
    input_file_accel = ..1, input_file_spiro = ..2, ID = ..3, file_demo = here::here("data/input/demo.csv"),
    spiro_interval = 30, overwrite = TRUE, verbose = FALSE
  )
})


# Pe-aggregate subject data into one file per accelerometer ----
get_overview_table() %>%
  distinct(model, placement) %>%
  purrr::pwalk(~{

    out_path <- here::here("data/processed-combined", .x, paste0(.y, ".rds"))

    # print(fs::path_dir(out_path))
    if (!fs::dir_exists(fs::path_dir(out_path))) fs::dir_create(fs::path_dir(out_path))

    print(out_path)
    combine_clean_data(model = .x, placement = .y) %>%
      saveRDS(file = out_path)
  })

