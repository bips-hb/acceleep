# Downsampling

downsample_full_data <- function(full_data, res, downsample_frac) {
  new_res <- res * downsample_frac

  full_data %>%
    group_by(interval, ID) %>%
    mutate(index = rep(seq_len(interval_length * new_res), each = new_res)) %>%
    group_by(interval, ID, index) %>%
    summarize(
      # interval = unique(interval),
      # ID = unique(ID),
      X = mean(X, na.rm = TRUE),
      Y = mean(Y, na.rm = TRUE),
      Z = mean(Z, na.rm = TRUE),
      MET = unique(MET),
      kJ = unique(kJ),
      Jrel = unique(Jrel)
    )
}

tick <- Sys.time()
get_overview_table() %>%
  distinct(model, placement) %>%
  mutate(res = ifelse(model == "activpal", 20, 100)) %>%
  tidyr::expand_grid(downsample_frac = c(0.5, 0.1, 0.05, 0.01)) %>%
  mutate(new_res = res * downsample_frac) %>%
  purrr::pwalk(~{
    out_path <- here::here("data/processed-downsampled", ..1, paste0(..2, "-", ..5, "hz", ".rds"))

    cliapp::cli_alert_info("{..1}-{..2} from {..3} to {..5} to {out_path}")

    if (!fs::dir_exists(fs::path_dir(out_path))) fs::dir_create(fs::path_dir(out_path))

    full_data <- get_combined_data(model = ..1, placement = ..2)
    new_data <- downsample_full_data(full_data = full_data, res = ..3, downsample_frac = ..4)

    saveRDS(new_data, file = out_path)
  })
tock <- Sys.time()

