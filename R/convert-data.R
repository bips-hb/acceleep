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
#'
#' @note Since t0 will vary ma acceloreter model, placement, and SID,
#'   this function needs to be called for all combinations thereof.
#'
#' @param input_file_spiro File path to spirometry data.
#' @param t0 Value of class `hms::hms()` to calibrate interval sequence to.
#' @param spiro_interval `[30]` Interval in seconds to calculate mean METs from
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' aggregate_spiro("data/input/spiro/ID_001_spiro.csv", t0 = hms::as_hms("09:00:00"))
#' }
aggregate_spiro <- function(input_file_spiro, t0, spiro_interval = 30) {
  stopifnot(file.exists(here(input_file_spiro)))

  spiro <- data.table::fread(here(input_file_spiro), select = c("Time", "MET", "O2", "CO2")) #%>%

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
#' @param input_file_acce,input_file_spiro File path to accelerometry/spirometrydata CSV.
#' @param ID Subject ID as character with 3 places, e.g. `"002"`.
#' @inheritParams aggregate_spiro
#' @import data.table
#' @return Nothing, btu writes data
#' @export
#'
convert_input_data <- function(input_file_accel, input_file_spiro, ID, spiro_interval = 30) {

  # Read acceleration data
  if (!file.exists(input_file_accel)) {
    warning("File ", input_file_accel, " does not exist.")
    return(NULL)
  }
  accel_data <- data.table::fread(input_file_accel)

  # Convert time format
  if (stringr::str_detect(input_file_accel, "actigraph")) {

    accel_data[, datetime := gsub(":(\\d+)$", ".\\1", Timestamp)]
    accel_data[, datetime := gsub("\\d+.\\d+.\\d+ ", "", datetime)]
    accel_data[, datetime := as.POSIXct(datetime, format = "%H:%M:%OS")]
    accel_data[, Timestamp := NULL]

  } else if (stringr::str_detect(input_file_accel, "activpal")) {

    accel_data[, datetime := as.POSIXct(as.numeric(datetime)*60*60*24, origin = "1899-12-30", tz = "GMT")]

  } else if (stringr::str_detect(input_file_accel, "geneactiv")) {

    accel_data[, datetime := gsub(":(\\d+)$", ".\\1", datetime)]
    accel_data[, datetime := gsub("\\d+-\\d+-\\d+ ", "", datetime)]
    accel_data[, datetime := as.POSIXct(datetime, format = "%H:%M:%OS")]
    accel_data[, c("datetime", "X", "Y", "Z")]

  }

  # Time in seconds
  # t0 <- min(accel_data$datetime)
  t0 <- hms::as_hms(min(accel_data$datetime))
  accel_data[, time := as.numeric(difftime(datetime, t0, units = "secs"))]
  accel_data[, interval := floor(time / spiro_interval) + 1]

  # Match spiro to accelerometer time
  spiro <- aggregate_spiro(input_file_spiro, t0, spiro_interval)

  # Rename columns
  # colnames(dat)[1:3] <- c("X", "Y", "Z")

  # Add ID
  accel_data$ID <- ID

  # Add METs
  final_data <- dplyr::left_join(spiro, accel_data, by = "interval")

  # Compute kJ and absolute and relative
  # pheno <- fread(file_pheno, select = c("ID", "weight"))
  # bodyweight <- pheno[ID == unique(res$ID), weight]
  # res[, kJ := (3.9 * O2 + 1.1 * CO2) * 4.184]
  # res[, Jrel := 1000 * kJ / bodyweight]

  # saveRDS(data, here::here("data", "proc", ))

  # save final data data
  out_file_accel <- stringr::str_replace(input_file_accel, "input", "processed")
  out_file_accel <- fs::path_ext_set(out_file_accel, ".rds")

  if (!fs::file_exists(out_file_accel)) {
    cliapp::cli_alert_info("Saving accelerometry for ID {ID} to file {out_file_accel}")
    saveRDS(final_data, file = out_file_accel)
  }

}



