#' Read CV results and bind them together
#'
#' @param path Path to CV results, e.g. `here::here("output/cross-validation/CNN")`.
#' @param latest_only `[TRUE]` Only return the latest run results.
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Iterate over all model output folders, read in latest CV runs, bind together
#' cv_results <- purrr::map_df(
#'  here::here("output/cross-validation", c("LM", "DNN", "CNN", "RNN")),
#'   read_cv_results
#' )
#' }
read_cv_results <- function(path, latest_only = TRUE) {
  # browser()
  # get latest run

  path <- fs::dir_ls(path)

  if (latest_only) {
    path <- rev(sort(path))[[1]]
  }

  purrr::map_df(fs::dir_ls(path, glob = "*.rds"), ~{
    tibble::tibble(
      file = .x,
      data = list(readRDS(.x) %>% dplyr::filter(!is.na(left_out)))
    )
  }) %>%
    mutate(
      file = fs::path_file(file) %>% fs::path_ext_remove(),
      mean_rmse = purrr::map_dbl(data, ~mean(.x$rmse)),
      median_rmse = purrr::map_dbl(data, ~median(.x$rmse)),
      sd_rmse = purrr::map_dbl(data, ~sd(.x$rmse))
    ) %>%
    tidyr::separate(
      col = .data$file,
      into = c("folds", "method", "model_kind", "model", "placement", "outcome", "res", "timestamp"),
      sep = "-"
    ) %>%
    dplyr::mutate(
      cv_count = purrr::map_dbl(.data$data, nrow),
      accel_id = glue::glue("{.data$model}_{.data$placement}"),
      # accel = glue::glue("{label_accel_models(.data$model)} ({ purrr::map_chr(.data$placement, label_placement)})"),
      accel = glue::glue("{label_accel_models(.data$model)} ({label_placement(.data$placement)})"),
      model_spec = purrr::map_chr(.data$data, ~unique(purrr::pluck(.x, "model_note", .default = NA))),
      model_spec = ifelse(is.na(model_spec), model_kind, model_spec),
      # mini_run = purrr::map_chr(.data$data, ~unique(purrr::pluck(.x, "mini_run", .default = NA)))
    ) %>%
    dplyr::rename(outcome_unit = .data$outcome) %>%
    dplyr::mutate(model_kind = ifelse(.data$model_kind == "RNN", glue::glue("RNN ({res}Hz)"), .data$model_kind))
}
