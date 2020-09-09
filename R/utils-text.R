#' Label accelerometer models
#' @param x Name of accelerometer model in the dataset, e.g. `"actigraph"`
#'
#' @return A `character()` of the same length as the input
#' @export
#' @importFrom dplyr case_when
#' @examples
#' label_accel_models("actigraph") # -> "Actigraph GT3X"
#' label_accel_models("activpal") # -> "activPAL"
#' label_accel_models("geneactiv") # -> "GENEActiv"
label_accel_models <- function(x) {
  dplyr::case_when(
    x == "actigraph" ~ "Actigraph GT3X",
    x == "activpal" ~ "activPAL",
    x == "geneactiv" ~ "GENEActiv"
  )
}

#' Label accelerometer placements
#' @param x Placement, e.g. `"hip_right"`
#'
#' @return A `character()` of the same length as the input
#' @export
#' @importFrom stringr str_split str_c
#' @importFrom purrr map_chr
#' @examples
#' label_placement("hip_right") # -> "right hip"
#' label_placement("thigh_right") # -> "right thigh"
#' label_placement("wrist_left") # -> "left wrist"
label_placement <- function(x) {

  purrr::map_chr(x, ~{
    .x %>%
      stringr::str_split("_", simplify = TRUE) %>%
      rev() %>%
      stringr::str_c(collapse = " ")
  })

}
