# Downsampling
library(acceleep)
library(dplyr)

downsample_full_data <- function(full_data, res, new_res, interval_length = 30) {
  # new_res <- res * downsample_frac
  downsample_frac <- new_res / res
  # browser()

  full_data %>%
    group_by(.data$interval, .data$ID) %>%
    # Dummy index to create groups of rows that are combined, e.g.
    # each two rows get the same index -> halving the resolution
    # Operation is grouped by each subject and interval, so no "bleeding" between subjects/intervals
    mutate(index = rep(seq_len(interval_length * new_res), each = 1/downsample_frac)) %>%
    group_by(.data$interval, .data$ID, .data$index) %>%
    summarize(
      X = mean(X, na.rm = TRUE),
      Y = mean(Y, na.rm = TRUE),
      Z = mean(Z, na.rm = TRUE),
      MET = unique(MET),
      kJ = unique(kJ),
      Jrel = unique(Jrel)
    ) %>%
    select(-.data$index) %>%
    ungroup()
}

# Doing this parallel..ly
library(furrr)

# Run this outside RStudio via Rscript
plan(multicore)

tick <- Sys.time()
get_overview_table() %>%
  distinct(model, placement) %>%
  mutate(res = ifelse(model == "activpal", 20, 100)) %>%
  tidyr::expand_grid(new_res = c(50, 10, 1)) %>%
  # Drop cases where activPal would get "upsampled" due to low base res
  filter(new_res < res) %>%
  # purrr::pwalk(~{
  future_pmap(~{
    out_path <- here::here("data/processed-combined", ..1, paste0(..2, "-", ..4, "hz", ".rds"))

    cliapp::cli_alert_info("Downsampling {..1}-{..2} from {..3} to {..4} to {out_path}")

    if (!fs::dir_exists(fs::path_dir(out_path))) fs::dir_create(fs::path_dir(out_path))

    full_data <- get_combined_data(model = ..1, placement = ..2)
    new_data <- downsample_full_data(full_data = full_data, res = ..3, new_res = ..4)

    saveRDS(new_data, file = out_path)
  })
tock <- Sys.time()


# Create summarized dataset for regression/RF models -----

###
### Doesn't pan out because train/test data should be summarized separately but with shared params,
### pre-aggregating would also require pre-defining train/test sets :(
###

# get_overview_table() %>%
#   distinct(model, placement) %>%
#   mutate(res = ifelse(model == "activpal", 20, 100)) %>%
#   purrr::pwalk(~{
#     browser()
#
#     out_path <- here::here("data/processed-combined", ..1, paste0(..2, "-", "summarized", ".rds"))
#     cliapp::cli_alert_info("Summarizing {..1}-{..2} to {out_path}")
#
#     full_data <- get_combined_data(model = ..1, placement = ..2, res = ..3)
#
#   })
#
#
# get_combined_data()
