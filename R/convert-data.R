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
