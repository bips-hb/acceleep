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


#' Label outcome units
#'
#' Translates:
#' - "kJ" -> "kJ/min"
#' - "Jrel" -> "J/min/kg"
#' - "MET" is kept as is.
#'
#' @param x A string correspnding to a unit as found in the dataset, e.g. `"kJ"`.
#' @param with_abbr `[FALSE]` If `TRUE`, `"kJ"` and `"Jrel"` are appended "(AEE)"
#' and "(REE)" respectively.
#'
#' @return A character vector of the same length as `x`
#' @export
#'
#' @examples
#' label_outcome("kJ")
#' label_outcome("Jrel")
#' label_outcome(c("kJ", "Jrel", "MET"))
#' label_outcome(c("kJ", "Jrel", "MET"), with_abbr = TRUE)
label_outcome <- function(x, with_abbr = FALSE) {

  if (with_abbr) {
    dplyr::case_when(
      x == "kJ" ~ "kJ/min (AEE)",
      x == "Jrel" ~ "J/min/kg (REE)",
      TRUE ~ x
    )
  } else {
    dplyr::case_when(
      x == "kJ" ~ "kJ/min",
      x == "Jrel" ~ "J/min/kg",
      TRUE ~ x
    )
  }

}
