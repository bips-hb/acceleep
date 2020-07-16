#' Release GPU memory manually
#'
#' This is a wrapper for a python call via [`reticulate::py_run_string()`]
#' that calls the `numba` python module to close one or more CUDA devices.
#' This ensures that GPU memory is freed up after / before modelling.
#' @details The `numba` module has to be available but is not automatically
#' installed by e.g. [`keras::install_keras()`]. You can manually install it
#' via `reticulate::conda_install("acceleep", "numba")` in a `conda` env
#' named `"acceleep"`.
#' @note See <https://github.com/rstudio/keras/issues/739>
#' @param device `[0]`: The numeric ID of the GPU device to close. To close
#'   multiple devices, use `device = c(0, 1)`.
#' @importFrom reticulate py_run_string
#' @importFrom glue glue
#' @export
#' @return Nothing
#' @examples
#' \dontrun{
#' # Some model fitting here
#' # ...
#' # Before new models are fit, free up memory just in case:
#' cuda_close_device(0)
#'
#' # New model fitting here
#' }
cuda_close_device <- function(device = 0) {
  for (dev in device) {
    reticulate::py_run_string(
      glue::glue("from numba import cuda; cuda.select_device({dev}); cuda.close()")
    )
  }
}

# Custom metric wrapper for RMSE
#
# A wrapper for [`keras::custom_metric()`] to define an RMSE-based metric.
#@rdname metric_rmse
# @name metric_rmse
# @param ... Used internally.
# @return An object of class `"python.builtin.function" "python.builtin.object"`.
# @export
# @note See <https://keras.rstudio.com/reference/metric_binary_accuracy.html>
#
# @examples
# \dontrun{
#
# metric_rmse <- keras::custom_metric("rmse", function(y_true, y_pred) {
#   keras::k_sqrt(keras::k_mean(keras::k_square(y_true - y_pred)))
# })
#
# model %>% compile(
#   optimizer = optimizer_rmsprop(),
#   loss = "mse",
#   metrics = metric_rmse
# )
# }

