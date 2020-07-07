#' Generate a single interval of time-series of accelerometry data
#'
#' One chunk of length `interval_length` seconds of accelerometry data
#' measured at `res` Hz with normally-distributed X/Y/Z measurements.
#' Using `offset` (in seconds) allows this function to be called repeatedly to
#' create consecutive chunks.
#' @param res `[100]`: Resolution of accelerometry data, in Hz. 20 for activPal,
#'   100 for other models.
#' @param interval_length `[30]` Number of seconds one interval covers.
#' @param offset Second offset from previous chunk.
#' @export
#' @return a [tibble][tibble::tibble-package]
generate_ts_chunk <- function(res = 100, interval_length = 30, offset = 0) {
  tibble::tibble(
    timestamp = seq(offset, (offset + interval_length) - 1/res, by = 1/res),
    x = stats::rnorm(length(timestamp), mean = 0),
    y = stats::rnorm(length(timestamp), mean = 0),
    z = stats::rnorm(length(timestamp), mean = 0),
  )
}

# Appease R CMD check
globalVariables("timestamp")

#' Generate a full acceloremtry dataset
#'
#' Wraps [`generate_ts_chunk()`] to generate `n_chunks` consecutive chunks
#' of accelerometry data.
#'
#' @inheritParams generate_ts_chunk
#' @param n_chunks Number of consecutive chunks to generate.
#' @export
#' @return a [tibble][tibble::tibble-package] with `res * interval_length * n_chunks` rows.
generate_ts_dataset <- function(res = 20, interval_length = 30, n_chunks = 100) {
  chunk_offsets <- seq(0, by = interval_length, length.out = n_chunks)
  purrr::map_df(chunk_offsets, ~generate_ts_chunk(res, interval_length, .x))
}
