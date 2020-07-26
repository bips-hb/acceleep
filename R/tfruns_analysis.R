#' Get tfruns metadata
#'
#' @param run_dir Path to a single run's data. Can also be a single row
#' of a data.frame as returned by `tfruns::ls_runs`
#'
#' @return A list for flags, tibble for metrics
#' @export
#' @importFrom jsonlite fromJSON
#'
tfrun_get_flags <- function(run_dir) {
  if (inherits(run_dir, "data.frame")) run_dir <- run_dir$run_dir

  jsonlite::fromJSON(fs::path(run_dir, "tfruns.d/flags.json"))
}

#' @rdname tfrun_get_flags
#' @export
#' @importFrom purrr map_dfc
#' @importFrom jsonlite fromJSON
tfrun_get_metrics <- function(run_dir) {

  if (inherits(run_dir, "data.frame")) run_dir <- run_dir$run_dir

  jsonlite::fromJSON(fs::path(run_dir, "tfruns.d/metrics.json")) %>%
    tibble::as_tibble() %>%
    purrr::map_dfc(as.numeric)
}


#' Assemble tfrun metata into distinct labels
#'
#' @inheritParams tfrun_get_flags
#'
#' @return
#' @export
#' @importFrom glue glue_data
#' @examples
#' \dontrun{
#' tfrun_label_summary("path/to/run")
#' }
tfrun_label_summary <- function(run_dir) {
  if (inherits(run_dir, "data.frame")) run_dir <- run_dir$run_dir

  flags <- tfrun_get_flags(run_dir)

  accel <- glue::glue_data(
    flags,
    "Accelerometer: {label_accel_models(accel_model)} ({label_placement(placement)}) at {res}Hz – Outcome: {outcome}"
  )

  model <- glue::glue_data(
    flags,
    "{lstm_layers}xLSTM({lstm_units}), {dense_layers}xDense({dense_units}) w/ {dropout_rate * 100}% dropout"
  )

  training <- glue::glue_data(
    flags,
    "Batch size = {batch_size}, LR = {lr} (decay = {decay}), validation split = {validation_split * 100}%"
  )

  list(
    accel = as.character(accel), model = as.character(model),
    training = as.character(training)
  )
}

#' Plot a run's loss training history
#'
#' @inheritParams tfrun_get_flags
#'
#' @return A ggplot2 object
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' plot_loss_history("output/runs/downsampled-ad-hoc/2020-07-24T14-48-54Z/")
#' }
plot_loss_history <- function(run_dir) {
# browser()
  if (inherits(run_dir, "data.frame")) {
    run_dir <- run_dir$run_dir
  }

  if (length(run_dir) > 1) {
    plots <- purrr::map(run_dir, plot_loss_history)
    return(cowplot::plot_grid(plotlist = plots))
  }

  loss_hist <- tfrun_get_metrics(run_dir) %>%
    dplyr::select(training = loss, validation = val_loss) %>%
    dplyr::mutate(epoch = dplyr::row_number())

  min_val_loss <- round(sqrt(min(loss_hist$validation)), 2)
  min_val_loss_epoch <- which.min(loss_hist$validation)

  run_labels <- tfrun_label_summary(run_dir)

  loss_hist %>%
    tidyr::pivot_longer(cols = c(training, validation), names_to = "loss") %>%
    ggplot(aes(x = epoch, y = value, color = loss, fill = loss)) +
    geom_path() +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(),
      sec.axis = sec_axis(trans = ~sqrt(.x), name = "Loss (RMSE)")
    ) +
    scale_color_brewer(
      palette = "Dark2", direction = -1, aesthetics = c("color", "fill"),
      breaks = c("validation", "training")
    ) +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    annotate(
      "text", label = glue::glue("Minimal validation RMSE of {min_val_loss} at epoch {min_val_loss_epoch}/{nrow(loss_hist)}"),
      x = 0, y = 2, hjust = 0
    ) +
    labs(
      title = glue::glue("{run_labels$model}"),
      subtitle = glue::glue("{run_labels$training}"),
      x = "Epoch", y = "Loss (MSE)",
      color = "", fill = "",
      caption = run_labels$accel
    ) +
    theme_minimal() +
    theme(legend.position = "top")
}
