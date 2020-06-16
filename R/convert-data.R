#' Convert Accelerometer tbl to Array
#'
#' @param accel_tbl Input tbl with columns X/Y/Z, one row per measurement.
#' @inheritParams generate_ts_chunk
#'
#' @return An array of dimensions c(chunks_per_tbl, 3, rows_per_chunk)
#' @export
#'
#' @examples
#' \dontrun{
#' convert_tbl_array(accel_tbl, interval_length = 30, res = 20)
#' }
convert_tbl_array <- function(accel_tbl, interval_length, res) {

  rows_per_chunk <- interval_length * res
  chunks_per_tbl <- nrow(accel_tbl) / (rows_per_chunk)

  # Generate the chunk ids and but drop potential overhead
  # If too many/too few ids are generated e.g. due to uneven number of measurements
  # in the last chunk, this would otherwise cause recycling issues
  # TLDR make sure it's ok if last chunk has less than exactly 30s of measurements
  # Wait no is that even sensible? should trailing measurements be dropped?
  # Check via nrow(accel_tbl) %% (res * interval_length) == 0
  index_along_rows <- seq_len(nrow(accel_tbl))
  chunk_ids <- rep(seq_len(chunks_per_tbl), each = rows_per_chunk)[index_along_rows]

  accel_tbl %>%
    as.matrix() %>%
    split(f = chunk_ids) %>%
    lapply(matrix, ncol = 3) %>%
    unlist() %>%
    array(dim = c(chunks_per_tbl, 3, rows_per_chunk))
}


#' Take raw spirotmetry data and collapse it into approrpiate intervals
#' TODO: Add demographic data to include REE + AEE units
#' @note Since `t0` will vary by accelerometer model, placement, and SID,
#'   this function needs to be called for all combinations thereof,
#'   as this is the most robust way to ensure measurements align correctly.
#'
#' @param input_file_spiro File path to spirometry data.
#' @param t0 Value of class `hms::hms()` to calibrate interval sequence to.
#' @param spiro_interval `[30]` Interval in seconds to calculate mean METs from
#'
#' @return A tibble
#' @export
#' @importFrom data.table fread
#' @examples
#' \dontrun{
#' aggregate_spiro("data/input/spiro/ID_001_spiro.csv", t0 = hms::as_hms("09:00:00"))
#' }
aggregate_spiro <- function(input_file_spiro, t0, spiro_interval = 30) {
  stopifnot(file.exists(here(input_file_spiro)))

  spiro <- data.table::fread(here::here(input_file_spiro), select = c("Time", "MET", "O2", "CO2"))

  spiro %>% as_tibble() %>%
    mutate(
      time = as.numeric(difftime(hms::as_hms(.data$Time), t0, units = "secs")),
      interval = floor(time / spiro_interval) + 1
    ) %>%
    group_by(interval) %>%
    summarise_at(vars(MET, O2, CO2), median) %>%
    filter(!is.na(MET))
}

#' (WIP) Read accelerometry + spiro and save to disk in analysis-friendly state
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
#' @inheritParams aggregate_spiro
#' @importFrom vroom vroom cols
#' @importFrom stats median
#' @return Nothing, just writes data with colums `c("interval", "ID", "X", "Y", "Z", "MET", "O2", "CO2")`
#' @export
#' @examples
#' \dontrun{
#' # actigraph
#' input_file_accel <- "data/input/actigraph/001_left_readable.csv"
#' # activpal
#' input_file_accel <- "data/input/activpal/001_ActivPal_readable.csv"
#' input_file_spiro <- "data/input/spiro/ID_001_spiro.csv"
#' ID <- "001"
#'
#' }
convert_input_data <- function(input_file_accel, input_file_spiro, ID, spiro_interval = 30, overwrite = TRUE) {

  cliapp::cli_alert_info("Reading {input_file_accel} and {input_file_spiro}")

  # Read acceleration data
  if (!file.exists(here::here(input_file_accel))) {
    warning("File ", input_file_accel, " does not exist.")
    return(NULL)
  }
  # accel_data <- data.table::fread(input_file_accel)
  #
  # accel_data2 <- vroom::vroom(input_file_accel)

  # Convert time format
  if (stringr::str_detect(input_file_accel, "actigraph")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("Timestamp", "Axis1", "Axis2", "Axis3"),
      col_types = vroom::cols("Timestamp" = "c")
    ) %>%
      dplyr::transmute(
        X = Axis1,
        Y = Axis2,
        Z = Axis3,
        datetime = lubridate::parse_date_time(Timestamp, orders = "%d.%m.%Y %H:%M:%0S"),
        time = hms::as_hms(datetime) # Time in H:M:S is easier
      )

  } else if (stringr::str_detect(input_file_accel, "activpal")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("datetime", "X", "Y", "Z"),
      col_types = vroom::cols("datetime" = "d")
    ) %>%
      mutate(
        datetime = datetime * 60 * 60 * 24,
        datetime = lubridate::as_datetime(datetime, origin = "1899-12-30", tz = "GMT"),
        time = hms::as_hms(datetime) # Time in H:M:S is easier
      )

  } else if (stringr::str_detect(input_file_accel, "geneactiv")) {

    accel_data <- vroom::vroom(
      input_file_accel,
      col_select = c("datetime", "X", "Y", "Z"),
      col_types = vroom::cols("datetime" = "c")
    ) %>%
      mutate(
        datetime = stringr::str_replace(datetime, ":(\\d+)$", ".\\1"), # Thanks Marvin
        datetime = lubridate::parse_date_time(datetime, orders = "%Y-%m-%d %H:%M:%0S"),
        time = hms::as_hms(datetime) # Time in H:M:S is easier
      )

  }

  # Time in seconds
  # Can't call min() on an hms, would return difftime obj
  t0 <- sort(accel_data$time)[1]

  accel_data <- accel_data %>%
    mutate(
      time = as.numeric(difftime(time, t0, units = "secs")),
      interval = floor(time / spiro_interval) + 1
    )

  # Match spiro to accelerometer time
  spiro <- aggregate_spiro(input_file_spiro, t0, spiro_interval)

  # Rename columns
  # colnames(dat)[1:3] <- c("X", "Y", "Z")

  # Add ID
  accel_data$ID <- ID

  # Add METs
  final_data <- dplyr::left_join(spiro, accel_data, by = "interval") %>%
    select(interval, ID, X, Y, Z, MET, O2, CO2)

  # Compute kJ and absolute and relative
  # pheno <- fread(file_pheno, select = c("ID", "weight"))
  # bodyweight <- pheno[ID == unique(res$ID), weight]
  # res[, kJ := (3.9 * O2 + 1.1 * CO2) * 4.184]
  # res[, Jrel := 1000 * kJ / bodyweight]

  # save final data data
  out_file_accel <- stringr::str_replace(input_file_accel, "input", "processed")
  out_file_accel <- fs::path_ext_set(out_file_accel, ".rds")

  if (!fs::file_exists(out_file_accel) | overwrite) {
    cliapp::cli_alert_info("Saving accelerometry for ID {ID} to file {out_file_accel}")
    saveRDS(final_data, file = out_file_accel)
  }

}

# Appease R CMD check (for now)
globalVariables(c(
  "CO2", "MET", "O2", "Timestamp", "datetime", "file_accel", "file_spiro",
  "interval", "median", "model", "placement", "sid", "time"
))


