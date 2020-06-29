#' Create Table of Datasets
#'
#' @return A tibble
#' @export
#' @examples
#' \dontrun{
#' get_overview_table()
#' }
get_overview_table <- function() {
  accel_models <- c("actigraph", "activpal", "geneactiv")
  input_files_accel <- fs::dir_ls(here::here("data", "input", accel_models))
  input_files_spiro <- fs::dir_ls(here::here("data", "input", "spiro"))
  # proc_files <- fs::dir_ls(here::here("data", "processed", accel_models))

  files_accel_overview <- tibble::tibble(
    file_accel = fs::path_rel(input_files_accel),
    sid = str_extract(.data$file_accel, "0\\d{2}"),
    model = dplyr::case_when(
      str_detect(.data$file_accel, "actigraph") ~ "actigraph",
      str_detect(.data$file_accel, "activpal") ~ "activpal",
      str_detect(.data$file_accel, "geneactiv") ~ "geneactiv"
    ),
    placement = str_remove_all(fs::path_file(.data$file_accel), "(\\d{3}\\_)|(\\_readable)|(\\.csv)"),
  ) %>%
    mutate(
      placement = ifelse(.data$model == "activpal", "thigh_right", .data$placement),
      placement = ifelse(.data$model == "actigraph", paste0("hip_", .data$placement), .data$placement),
      file_clean = stringr::str_replace(.data$file_accel, "input", "processed"),
      file_clean = fs::path_ext_set(.data$file_clean, ".rds"),
      file_clean_exists = fs::file_exists(.data$file_clean)
    )

  files_spiro_overview <- tibble::tibble(
    file_spiro = fs::path_rel(input_files_spiro),
    sid = str_extract(.data$file_spiro, "0\\d{2}")
  )

  dplyr::left_join(files_accel_overview, files_spiro_overview, by = "sid") %>%
    dplyr::arrange(.data$sid) %>%
    dplyr::select(.data$sid, .data$model, .data$placement, dplyr::starts_with("file_"))
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

#' Extract the subject ID from a file path
#'
#' This is entirely overengineered.
#' @param x A file path, e.g. `"data/input/spiro/ID_001_spiro.csv"`
#' @return A character of the same length as the input
#' @export
#'
#' @examples
#' id_from_path("data/input/spiro/ID_001_spiro.csv") # -> "001"
id_from_path <- function(x) {
  fs::path_file(x) %>%
    stringr::str_extract("\\d{3}")
}
