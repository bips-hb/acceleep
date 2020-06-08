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
      placement = ifelse(model == "activpal", NA_character_, placement)
    )

  files_spiro_overview <- tibble::tibble(
    file_spiro = path_rel(input_files_spiro),
    sid = str_extract(file_spiro, "0\\d{2}")
  )

  dplyr::left_join(files_accel_overview, files_spiro_overview, by = "sid") %>%
    dplyr::arrange(sid)
}
