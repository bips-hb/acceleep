#' Label accelerometer models for final text
#'
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
