#' Create Table of Datasets
#'
#' @return A tibble
#' @export
#' @import fs
#' @import stringr
#' @examples
#' \dontrun{
#' get_overview_table()
#' }
get_overview_table <- function() {
  accel_models <- c("actigraph", "activpal", "geneactiv")
  input_files_accel <- dir_ls(here::here("data", "input", accel_models))
  input_files_spiro <- dir_ls(here::here("data", "input", "spiro"))
  # proc_files <- fs::dir_ls(here::here("data", "processed", accel_models))

  files_accel_overview <- tibble::tibble(
    file_accel = path_rel(input_files_accel),
    sid = str_extract(file_accel, "0\\d{2}"),
    model = dplyr::case_when(
      str_detect(file_accel, "actigraph") ~ "actigraph",
      str_detect(file_accel, "activpal") ~ "activpal",
      str_detect(file_accel, "geneactiv") ~ "geneactiv"
    ),
    placement = str_remove_all(path_file(file_accel), "(\\d{3}\\_)|(\\_readable)|(\\.csv)"),
  ) %>%
    mutate(
      placement = ifelse(model == "activpal", "thigh_right", placement),
      placement = ifelse(model == "actigraph", paste0("hip_", placement), placement)
    )

  files_spiro_overview <- tibble::tibble(
    file_spiro = path_rel(input_files_spiro),
    sid = str_extract(file_spiro, "0\\d{2}")
  )

  dplyr::left_join(files_accel_overview, files_spiro_overview, by = "sid") %>%
    dplyr::arrange(sid) %>%
    dplyr::select(sid, model, placement, dplyr::starts_with("file_"))
}


#' Save an R object to a local directory
#'
#' This is purely a convenience function for the lazy.
#'
#' @param x An R object. The name of the object will be the filename, e.g. "iris" -> `iris.rds`.
#' @param dir Optional: A subdirectory of `here::here("output")`
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' save_output_data(iris)
#' }
save_output_data <- function(x, dir = "") {
  out_dir <- here::here("output", dir)
  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir, recurse = TRUE)
  filename <- paste0(deparse(substitute(x)), ".rds")
  saveRDS(x, fs::path(out_dir, filename))
}
