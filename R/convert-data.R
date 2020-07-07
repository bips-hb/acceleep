#' Get demographics
#'
#' Reads in the demographics data without further processing.
#' @param file_demo `[here::here("data/input/demo.csv")]`: Path to `demo.csv`.
#' @param id Optional. Specify a subject id such as `"005"` or `5` (will be coerced to numeric) to filter the demographics data
#'   by that id.
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_demo()
#'
#' get_demo(id = "003")
#' }
get_demo <- function(file_demo = here::here("data/input/demo.csv"), id = NULL) {
  demo <- vroom::vroom(
    file = file_demo,
    col_types = "dddddddd"
  )

  if (!is.null(id)) {
    demo <- demo %>%
      dplyr::filter(.data$ID == as.numeric(id))
  }

  demo
}

#' Take raw spirotmetry data and collapse it into approrpiate intervals
#' TODO: Add demographic data to include REE + AEE units
#' @note Since `t0` will vary by accelerometer model, placement, and SID,
#'   this function needs to be called for all combinations thereof,
#'   as this is the most robust way to ensure measurements align correctly.
#'
#' @param input_file_spiro File path to spirometry data.
#' @inheritParams get_demo
#' @param t0 Value of class `hms::hms()` to calibrate interval sequence to.
#' @param spiro_interval `[30]` Interval in seconds to calculate mean METs from
#'
#' @return A tibble
#' @export
#' @importFrom vroom vroom
#' @importFrom vroom cols
#' @importFrom stats median
#' @examples
#' \dontrun{
#' aggregate_spiro("data/input/spiro/ID_001_spiro.csv", t0 = hms::as_hms("09:00:00"))
#' }
aggregate_spiro <- function(
  input_file_spiro, file_demo = here::here("data/input/demo.csv"),
  t0, spiro_interval = 30
  ) {
  stopifnot(file.exists(input_file_spiro))

  spiro <- vroom::vroom(
    here::here(input_file_spiro),
    col_select = c("Time", "MET", "O2", "CO2"),
    col_types = vroom::cols(Time = "c", MET = "d", O2 = "d", CO2 = "d")
  )

  demo <- get_demo(file_demo, id = id_from_path(input_file_spiro))

  spiro %>%
    mutate(
      time = as.numeric(difftime(hms::as_hms(.data$Time), t0, units = "secs")),
      interval = floor(.data$time / .data$spiro_interval) + 1,
      kJ = (3.9 * .data$O2 + 1.1 * .data$CO2) * 4.184, # res[, kJ := (3.9 * O2 + 1.1 * CO2) * 4.184]
      Jrel = 1000 * .data$kJ / demo$weight
    ) %>%
    group_by(.data$interval) %>%
    summarise_at(vars(.data$MET, .data$kJ, .data$Jrel), median)
}

#' Read accelerometry + spiro and save to disk in analysis-friendly state
#'
#' @details
#' Accelerometer model is currently detected from the file path in `input_file_accel`, which has to contain
#' the following strings to properly detect each model:
#'
#' - `"actigraph"`: GT3X ActiGraph
#' - `"activpal"`: activPAL
#' - `"geneactiv"`: GENEActiv
#'
#' @param input_file_accel,input_file_spiro File path to accelerometry/spirometrydata CSV.
#' @param ID Subject ID as character with 3 places, e.g. `"002"`.
#' @param overwrite `[TRUE]` Overwrite existing output files or leave them untouched?
#' @param verbose `[FALSE]` If `TRUE`, the function signals input file and save location to the console.
#' @inheritParams aggregate_spiro
#' @inheritParams get_demo
#' @importFrom vroom vroom cols
#' @importFrom stats median
#' @return Invisibly + writtem data with colums `c("interval", "ID", "X", "Y", "Z", "MET", "kJ", "Jrel")`
#' @export
#' @examples
#' \dontrun{
#' # actigraph
#' input_file_accel <- "data/input/actigraph/026_left_readable.csv"
#' # activpal
#' input_file_accel <- "data/input/activpal/026_ActivPal_readable.csv"
#' # geneactiv
#' input_file_accel <- "data/input/geneactiv/026_wrist_right_readable.csv"
#' # spiro
#' input_file_spiro <- "data/input/spiro/ID_026_spiro.csv"
#' ID <- "026"
#'
#' convert_input_data(input_file_accel, input_file_spiro, ID = ID, overwrite = FALSE, verbose = TRUE)
#'
#' }
convert_input_data <- function(
  input_file_accel, input_file_spiro, file_demo = here::here("data/input/demo.csv"),
  ID, spiro_interval = 30, overwrite = TRUE, verbose = FALSE
  ) {

  if (verbose) cliapp::cli_alert_info("Reading {input_file_accel} and {input_file_spiro}")

  # Read acceleration data
  if (!file.exists(here::here(input_file_accel))) {
    warning("File ", input_file_accel, " does not exist.")
    return(NULL)
  }

  # Convert time format
  if (stringr::str_detect(input_file_accel, "actigraph")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("Timestamp", "Axis1", "Axis2", "Axis3"),
      col_types = vroom::cols("Timestamp" = "c")
    ) %>%
      dplyr::transmute(
        X = .data$Axis1,
        Y = .data$Axis2,
        Z = .data$Axis3,
        datetime = lubridate::parse_date_time(.data$Timestamp, orders = "%d.%m.%Y %H:%M:%0S"),
        time = hms::as_hms(.data$datetime) # Time in H:M:S is easier
      )

  } else if (stringr::str_detect(input_file_accel, "activpal")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("datetime", "X", "Y", "Z"),
      col_types = vroom::cols("datetime" = "d")
    ) %>%
      mutate(
        datetime = .data$datetime * 60 * 60 * 24,
        datetime = lubridate::as_datetime(.data$datetime, origin = "1899-12-30", tz = "GMT"),
        time = hms::as_hms(.data$datetime) # Time in H:M:S is easier
      )

  } else if (stringr::str_detect(input_file_accel, "geneactiv")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("datetime", "X", "Y", "Z"),
      col_types = vroom::cols("datetime" = "c")
    ) %>%
      mutate(
        datetime = stringr::str_replace(.data$datetime, ":(\\d+)$", ".\\1"), # Thanks Marvin
        datetime = lubridate::parse_date_time(.data$datetime, orders = "%Y-%m-%d %H:%M:%0S"),
        time = hms::as_hms(.data$datetime) # Time in H:M:S is easier
      )
  }

  # Time in seconds
  # Can't call min() on an hms, would return difftime obj
  t0 <- sort(accel_data$time)[1]

  accel_data <- accel_data %>%
    mutate(
      time = as.numeric(difftime(.data$time, t0, units = "secs")),
      interval = floor(.data$time / .data$spiro_interval) + 1
    )

  # Match spiro to accelerometer time
  spiro <- aggregate_spiro(
    input_file_spiro = input_file_spiro,
    file_demo = file_demo,
    t0 = t0, spiro_interval = spiro_interval
  ) %>%
    filter(
      !is.na(.data$MET), !is.na(.data$kJ), !is.na(.data$Jrel)
    ) # Drop rows for which all of these are NA

  # Inner join spiro + accel by interval, result contains only data for intervals present in both datasets!
  # Add METs, kJ (AEE), Jrel (REE)
  final_data <- dplyr::inner_join(spiro, accel_data, by = "interval") %>%
    dplyr::mutate(ID = ID) %>%
    dplyr::select(
      .data$interval, .data$ID, .data$X, .data$Y, .data$Z,
      .data$MET, .data$kJ, .data$Jrel
    )

  # Save final data
  out_file <- stringr::str_replace(input_file_accel, "input", "processed")
  out_file <- fs::path_ext_set(out_file, ".rds")

  if (!fs::file_exists(out_file) | overwrite) {
    if (verbose) cliapp::cli_alert_info("Saving accelerometry for ID {ID} to file {out_file}")
    saveRDS(final_data, file = out_file)
  }
  # Invisible return for interactive debugging
  invisible(final_data)
}
