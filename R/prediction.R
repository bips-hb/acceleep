#' Assemble training data for prediction
#'
#' @inheritParams combine_clean_data
#' @inheritParams extract_outcome
#' @inheritParams make_initial_splits
#' @inheritParams keras_reshape_accel
#' @inheritSection combine_clean_data Available Data
#'
#' @return Two tibbles for training data and labels
#' @export
#'
assemble_train_data <- function(
  accel_model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right"),
  outcome = c("MET", "kJ", "Jrel"),
  random_seed = 11235813, val_split = 1/3,
  interval_length = 30, res = 1
) {
  # browser()

  full_data <- get_combined_data(model = accel_model, placement = placement, res = res)

  # Split into train / validation datasets
  c(training_data, validation_data) %<-% make_initial_splits(
    full_data, random_seed = random_seed, val_split = val_split
  )

  training_xyz <- training_data %>%
    dplyr::select(.data$interval, .data$ID, .data$X, .data$Y, .data$Z) %>%
    mutate(
      X = (.data$X - mean(.data$X, na.rm = TRUE)) / sd(.data$X, na.rm = TRUE),
      Y = (.data$Y - mean(.data$Y, na.rm = TRUE)) / sd(.data$Y, na.rm = TRUE),
      Z = (.data$Z - mean(.data$Z, na.rm = TRUE)) / sd(.data$Z, na.rm = TRUE)
    )

  training_outcome <- training_data %>%
    dplyr::select(.data$interval, .data$ID, .env$outcome) %>%
    dplyr::distinct()

  list(
    data = training_xyz,
    labels = training_outcome
  )

}

#' Predict outcome for all data in the given training set
#'
#' @param model A keras model to predict from.
#' @inheritParams assemble_train_data
#'
#' @return A tibble
#' @export
predict_on_training_set <- function(model, training_data, interval_length = 30, res = 1) {

  # browser()
  training_data %>%
   # filter(ID == "021", interval == 15) %>%
    # group_by(ID, interval) %>%
    group_by(ID) %>%
    group_map(~{

      # browser()
      # cliapp::cli_alert("ID = {.y$ID}, interval = {.y$interval}")

      train_array <- .x %>%
        select(.data$X, .data$Y, .data$Z) %>%
        as.matrix() %>%
        keras_reshape_accel(interval_length = interval_length, res = res)

      dim(train_array)

      predict0 <- purrr::safely(keras:::predict.keras.engine.training.Model)

      predicted <- predict0(model, train_array)

      # Weird error that might be related to cuDNN?
      # Some predictions fail on specific intervals
      if (is.null(predicted$result)) {
        return(tibble(
          ID = .y$ID,
         #  interval = .y$interval,
          interval = seq_along(predicted),
          predicted = NA_real_
        ))
      }

      tibble(
        predicted = as.numeric(predicted$result),
        ID = .y$ID,
        # interval = .y$interval
        interval = seq_along(predicted)
      )

    }, .keep = TRUE) %>%
    bind_rows()
}
