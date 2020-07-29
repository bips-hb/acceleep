#' Combine cleaned data for one accelerometer model / placement
#'
#' This function collects the datasets (split by subject) for a given model and placement, where the result is a
#' single dataset of all measurements across subjects.
#'
#' @section Available Data:
#'
#' The following combinations of `model` and `placement` are available:
#'
#' |model     |placement   |
#' |:---------|:-----------|
#' |actigraph |hip_left    |
#' |actigraph |hip_right   |
#' |activpal  |thigh_right |
#' |geneactiv |hip_right   |
#' |geneactiv |wrist_left  |
#' |geneactiv |wrist_right |
#' @param model Accelerometer model, one of `c("actigraph", "activpal", "geneactiv")`.
#' @param placement One of `c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right")`.
#'
#' @return A tibble. If combination of `model` and `placement` is invalid, an empty `data.frame`.
#' @export
#' @importFrom dplyr filter pull
#' @importFrom purrr map_df
#' @note Note that not every combination of `model` and `placement` are valid.
#' @examples
#' \dontrun{
#' combine_clean_data("actigraph", "hip_left", res = 20)
#' }
combine_clean_data <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right")
) {

  # browser()
  model <- match.arg(model)
  placement <- match.arg(placement)

  get_overview_table() %>%
    dplyr::filter(
      .data$file_clean_exists,
      .data$model == !!model, .data$placement == !!placement
    ) %>%
    dplyr::pull(.data$file_clean) %>%
    purrr::map_df(readRDS)
}

#' @rdname combine_clean_data
#' @export
get_combined_data <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right"),
  res = 100
) {
  # browser()
  model <- match.arg(model)
  placement <- match.arg(placement)

  file_path <- here::here("data/processed-combined", model, paste0(placement, "-", res, "hz", ".rds"))

  if (!fs::file_exists(file_path)) {
    stop("File ", file_path, " does not exist. Check your model/placement/resolution arguments.")
  }

  # Read and ungroup data because I forgot to ungroup when saving the downsampled data
  # and now I have to fix it, but don't want to resave the data just yet.
  readRDS(file_path) %>%
    # Reorder columns to ID, interval - should have done that earlier
    select(.data$ID, .data$interval, dplyr::everything()) %>%
    ungroup() %>%
    group_by(.data$ID, .data$interval) %>%
    # get per-interval row ids just in case
    mutate(rowid = seq_along(.data$interval)) %>%
    ungroup()
}

#' Extract the outcome variables from the working dataset
#'
#' The working dataset will have both accelerometry and energy expenditure
#' measurements, where the latter are duplicated along each measurement interval.
#' This function extracts the outcome variable for each interval and returns it
#' so that the result is a vector with the same number of elements as there are
#' intervals in the dataset. Since intervals are numbered uniquely only per
#' subject, an additional variable `ID` is expected to ensure that datasets
#' containing multiple are handled correctly.
#'
#' @param xdf A full dataset containing outcome measures and interval / subject identifiers
#' as returned by [`convert_input_data()`]  or combined by [`combine_clean_data()`]
#' @param outcome `[MET]` one of `c("MET", "kJ", "Jrel")`, referring to
#' metabolic equivalents, absolute energy expenditure and relative energy
#' expenditure respectively.
#' @param output_type `["numeric", "tbl"]` Return the outcome as a `"numeric"` vector
#' or as a [`tibble::tibble()`], where the latter additionally preserves
#' `interval` and `ID` variables for potential debugging.
#'
#' @return Either a `numeric` vector (default) or `[tibble::tibble()]`, depending on
#' `outcome_type`.
#' @export
#' @importFrom dplyr distinct select
#' @examples
#' \dontrun{
#' xdf <- combine_clean_data(model = "actigraph", placement = "hip_right")
#' mets <- extract_outcome(xdf, outcome = "MET", output_type = "numeric")
#'
#' # mets is now a vector with one value per interval across all subjects
#' }
extract_outcome <- function(
  xdf, outcome = c("MET", "kJ", "Jrel"), output_type = c("numeric", "tbl")
  ) {

  outcome <- match.arg(outcome, choices = c("MET", "kJ", "Jrel"))

  ret <- xdf %>%
    dplyr::select("ID", "interval", dplyr::all_of(outcome)) %>%
    dplyr::distinct()

  if (output_type == "numeric") {
    ret <- ret[[outcome]]
  }

  ret
}

#' Split an entire dataset into training/validation data.
#' Please note that the resulting datasets are *not* yet split into data and labels.
#'
#' @param full_data The full dataset as e.g. assempled via [`combine_clean_data()`].
#' @param random_seed `[11235813]` Seed used to ensure reproducibility via [`set.seed`].
#' @param val_split `[1/3]` Validation split, meaning e.g. 1/3 of the subjects
#'   (rounded down) will be used as validation dataset.
#'
#' @return A list of two tibbles, each containing the full dataset incl.
#'   accelerometry and calorimetry.
#' @export
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' full_data <- combine_clean_data("activpal", "thigh_right")
#' c(training_data, validation_data) %<-% make_initial_splits(
#'   full_data, random_seed = 21421, val_split = 1/3
#' )
#' }
make_initial_splits <- function(full_data, random_seed = 11235813, val_split = 1/3) {
  # Extract subject ids present in data and assign them to test / validation sets
  ids <- unique(full_data$ID)
  # browser()
  set.seed(random_seed)
  ids_validation <- sample(ids, size = floor(length(ids) * val_split)) # 1/3 of IDs
  ids_train <- ids[!(ids %in% ids_validation)]

  # Stop if somehow train + test ids are not the same as the initial ids
  stopifnot(
    identical(sort(c(ids_validation, ids_train)), sort(ids))
  )

  # split data into train/validation sets
  dat_training <- full_data %>%
    dplyr::filter(.data$ID %in% ids_train) %>%
    arrange(.data$ID, .data$interval, .data$rowid)

  dat_validation <- full_data %>%
    dplyr::filter(.data$ID %in% ids_validation) %>%
    arrange(.data$ID, .data$interval, .data$rowid)

  list(
    training = dat_training,
    validation = dat_validation
  )
}

#' Normalize accelerometry data
#'
#' Standard normalization by subtracting the mean and dividing by standard
#' deviation. Mean and SD are calculated from the entire training data (across
#' all subjects/intervals) and is used to standardize both training- and test-
#' sets with the same parameters.
#' @inheritParams split_data_labels
#'
#' @return A list of two tibbles, ordered same as input arguments.
#' @export
#'
#' @examples
#' \dontrun{
#' normalize_accelerometry(training_data, validation_data)
#' }
normalize_accelerometry <- function(training_data, validation_data) {
  # This is sloppy but at least it gets the job done
  # I can agonize over this later
  training_meansd <- training_data %>%
    dplyr::ungroup() %>% # ungrouping just for safety
    dplyr::summarize_at(
      dplyr::vars(.data$X,.data$Y, .data$Z),
      list(mean = mean, sd = sd), na.rm = TRUE
    )

  # One mean and SD per axis per the entire training set
  training_data <- training_data %>%
    dplyr::mutate(
      X = (.data$X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (.data$Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (.data$Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

  # Validation data and labels
  # Scaling XYZ and labels with same mean/sd as training data!
  validation_data <- validation_data %>%
    dplyr::mutate(
      X = (.data$X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (.data$Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (.data$Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

  list(
    training = training_data,
    validation = validation_data
  )
}

#' Summarize accelerometry data
#'
#' Statistics calculated:
#'
#' - mean
#' - SD
#' - lag-1 autocorrelation
#' - min, max
#' - quantiles at 10, 25, 50, 75 and 90
#'
#' @inheritParams normalize_accelerometry
#'
#' @return A list of tibbles
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_accelerometry(training_data, validation_data)
#' }
summarize_accelerometry <- function(training_data, validation_data = NULL) {

  training_data_summarized <- training_data %>%
    group_by(.data$ID, .data$interval) %>%
    summarize(across(c(.data$X, .data$Y, .data$Z), list(
      mean = mean,
      sd = sd,
      min = min,
      max = max,
      q10 = ~quantile(.x, probs = 0.1),
      q25 = ~quantile(.x, probs = 0.25),
      q50 = ~quantile(.x, probs = 0.5),
      q75 = ~quantile(.x, probs = 0.75),
      q90 = ~quantile(.x, probs = 0.9),
      acf = ~acf(.x, plot = FALSE, lag.max = 1)[["acf"]][2,,]
    ), .names = "{col}_{fn}"), .groups = "drop")

  # Paste EE measures back on the summarized accelerometry
  training_data <- training_data_summarized %>%
    left_join(
      training_data %>%
        select(-any_of(c("X", "Y", "Z", "rowid"))) %>%
        distinct(),
      by = c("ID", "interval")
    )

  if (!is.null(validation_data)) {
    # Do the same for the validation data
    # Just in case later standardization needs to use distinct parameters for train/test set
    validation_data_summarized <- validation_data %>%
      group_by(.data$ID, .data$interval) %>%
      summarize(across(c(.data$X, .data$Y, .data$Z), list(
        mean = mean,
        sd = sd,
        min = min,
        max = max,
        q10 = ~quantile(.x, probs = 0.1),
        q25 = ~quantile(.x, probs = 0.25),
        q50 = ~quantile(.x, probs = 0.5),
        q75 = ~quantile(.x, probs = 0.75),
        q90 = ~quantile(.x, probs = 0.9),
        acf = ~acf(.x, plot = FALSE, lag.max = 1)[["acf"]][2,,]
      ), .names = "{col}_{fn}"), .groups = "drop")

    validation_data <- validation_data_summarized %>%
      left_join(
        validation_data %>%
          select(-any_of(c("X", "Y", "Z", "rowid"))) %>%
          distinct(),
        by = c("ID", "interval")
      )
  }

  list(
    training = training_data,
    validation = validation_data
  )
}

#' Split training/validation datasets into separate data and labels
#'
#' This function only splits accelerometry from energy expenditure.
#' @note As of now, this function only ever returns one label per interval of accelerometry data.
#' @param training_data,validation_data Datasets as created by e.g. `[make_initial_splits()]`.
#' @param outcome `["MET"]`: Outcome variable to be extracted, passed to `[extract_outcome()]`.
#'
#' @return A nested list of the form
#' ```
#' list(
#'   training = list(
#'     data = train_data,
#'     labels = train_labels
#'   ),
#'   validation = list(
#'     data = test_data,
#'     labels = test_labels
#'   )
#' )
#' ```
#' @export
#' @importFrom dplyr select mutate summarize_at vars
#' @importFrom stats sd
#' @examples
#' \dontrun{
#' full_data <- combine_clean_data("activpal", "thigh_right")
#' c(training_data, validation_data) %<-% make_initial_splits(
#'   full_data, random_seed = 21421, val_split = 1/3
#' )
#' split_data <- split_data_labels(training_data, validation_data, outcome = "kJ")
#'
#' c(train_data, train_labels) %<-% split_data$training
#' c(test_data, test_labels) %<-% split_data$validation
#' }
split_data_labels <- function(
  training_data, validation_data,
  outcome = c("MET", "kJ", "Jrel")
  ) {

  outcome <- match.arg(outcome)
  # browser()

  training_xyz <- training_data %>%
    dplyr::select(.data$X, .data$Y, .data$Z)

  training_labels <- extract_outcome(training_data, outcome = outcome, output_type = "numeric")

  # Validation data and labels
  validation_xyz <- validation_data %>%
    dplyr::select(.data$X, .data$Y, .data$Z)

  validation_labels <- extract_outcome(validation_data, outcome = outcome, output_type = "numeric")

  list(
    training = list(
      data = training_xyz,
      labels = training_labels
    ),
    validation = list(
      data = validation_xyz,
      labels = validation_labels
    )
  )
}

#' Convert a tbl of accelerometry to a Keras- / LSTM-friendly-array
#'
#' As this function uses [`keras::array_reshape()`], its lack of sophistication
#' barely justifies its existence and one might as well just use
#' [`keras::array_reshape()`].
#' @note
#' This function uses [`keras::array_reshape()`], which adds a rather daunting
#' dependency on Python, but at least it has saved the author from hours
#' of unsuccessful experimentation trying to re-create the correct output
#' structure using [`array()`].
#' @param accel_tbl Input tbl with *only* columns X/Y/Z, one row per measurement.
#' @inheritParams generate_ts_chunk
#'
#' @return An [`array()`] of dimensions `c(samples, res * interval_length, 3)`,
#' wherte `samples` is calculated as `nrow(accel_tbl) / (interval_length * res)`.
#' @export
#' @importFrom keras array_reshape
#' @examples
#' \dontrun{
#' # Aggregating subject data for model/placement
#' full_data <- combine_clean_data("activpal", "thigh_right")
#'
#' # Split into train / validation datasets
#' c(training_data, validation_data) %<-% make_initial_splits(
#'   full_data, random_seed = 21421, val_split = 1/3
#' )
#'
#' # Split into data and labels
#' split_data <- split_data_labels(training_data, validation_data, outcome = "kJ")
#'
#' c(train_data, train_labels) %<-% split_data$training
#' c(test_data, test_labels) %<-% split_data$validation
#'
#' # Reshaping to array form
#' train_data <- keras_reshape_accel(train_data, 30, 20)
#' test_data <- keras_reshape_accel(test_data, 30, 20)
#' }
keras_reshape_accel <- function(accel_tbl, interval_length = 30, res = 100) {
  # rows per chunk = interval_length * res
  chunks_per_tbl <- nrow(accel_tbl) / (interval_length * res)

  keras::array_reshape(
    as.matrix(accel_tbl), c(chunks_per_tbl, res * interval_length, 3)
  )
}

#' Do the entire preparation dance
#'
#' This function wraps up all steps required to prepare the sample data
#' for `keras` modelling using RNNs (LSTM/GRU).
#'
#' @details
#' This is a wrapper around the following steps:
#'
#' 1. [`combine_clean_data()`]: Aggregate per-subject datasets for a given
#' accelerometer model and placement.
#' 2. [`make_initial_splits()`]: Split the data into a training and validation
#' dataset.
#' 3. [`split_data_labels()`]: Split the data into accelerometry and calorimetry
#' data.
#' 4. [`keras_reshape_accel()`]: Reshape the accelerometry data into an array
#' (tensor) of shape (number_of_intervals, `res * interval_length`, 3).
#'
#' @inheritParams combine_clean_data
#' @inheritParams extract_outcome
#' @inheritParams make_initial_splits
#' @inheritParams keras_reshape_accel
#' @inheritSection combine_clean_data Available Data
#'
#' @return A nested list of the form
#' ```
#' list(
#'   training = list(
#'     data = train_data,
#'     labels = train_labels
#'   ),
#'   validation = list(
#'     data = test_data,
#'     labels = test_labels
#'   )
#' )
#' ```
#'
#' Where `train_data` and `test_data` are already converted to a tensor with 3
#' axes and `train_labels` and `test_labels` are numeric vectors of the same
#' length as the first axis of the respective `_data` tensor.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' c(c(train_data, train_labels), c(test_data, test_labels)) %<-% keras_prep_lstm(
#'   model = "geneactiv", placement = "hip_right",
#'   outcome = "kJ", random_seed = 19283, val_split = 1/2,
#'   interval_length = 30, res = 1
#' )
#' }
keras_prep_lstm <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right"),
  outcome = c("MET", "kJ", "Jrel"),
  random_seed = 11235813, val_split = 1/3,
  interval_length = 30, res = 100, normalize = TRUE
) {
  # browser()
  # Aggregating subject data for model/placement
  full_data <- get_combined_data(model = model, placement = placement, res = res)

  # Split into train / validation datasets
  c(training_data, validation_data) %<-% make_initial_splits(
    full_data, random_seed = random_seed, val_split = val_split
  )

  if (normalize) {
    c(training_data, validation_data) %<-% normalize_accelerometry(training_data, validation_data)
  }

  # Split into data and labels
  split_data <- split_data_labels(training_data, validation_data, outcome = outcome)

  c(train_data, train_labels) %<-% split_data$training
  c(test_data, test_labels) %<-% split_data$validation

  # Reshaping to array form
  train_data <- keras_reshape_accel(
    accel_tbl = train_data, interval_length = interval_length, res = res
  )
  test_data <- keras_reshape_accel(
    accel_tbl = test_data, interval_length = interval_length, res = res
  )

  list(
    training = list(
      data = train_data,
      labels = train_labels
    ),
    validation = list(
      data = test_data,
      labels = test_labels
    )
  )
}


#'
#' @rdname keras_prep_lstm
#' @inheritParams keras_prep_lstm
#'
#' @return
#' @export
#'
keras_prep_regression <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right"),
  outcome = c("MET", "kJ", "Jrel"),
  random_seed = 11235813, val_split = 1/3,
  interval_length = 30, res = 100, normalize = TRUE
) {

  # browser()
  # Aggregating subject data for model/placement
  full_data <- get_combined_data(model = model, placement = placement, res = res)

  # Split into train / validation datasets
  c(training_data, validation_data) %<-% make_initial_splits(
    full_data, random_seed = random_seed, val_split = val_split
  )

  # Summarization
  c(training_data, validation_data) %<-% summarize_accelerometry(training_data, validation_data)

  # Split into data and labels
  # Keep ID and interval for easier predictions afterwards
  # Drop first two columns when feeding to keras: as.matrix(train_data[-c(1,2)])
  train_data <- training_data %>%
    select("ID", "interval", dplyr::starts_with("X"), dplyr::starts_with("Y"), dplyr::starts_with("Z"))

  test_data <- validation_data %>%
    select("ID", "interval", dplyr::starts_with("X"), dplyr::starts_with("Y"), dplyr::starts_with("Z"))

  train_labels <- extract_outcome(training_data, outcome = outcome, output_type = "numeric")
  test_labels <-  extract_outcome(validation_data, outcome = outcome, output_type = "numeric")


  list(
    training = list(
      data = train_data,
      labels = train_labels
    ),
    validation = list(
      data = test_data,
      labels = test_labels
    )
  )
}

globalVariables(c("training_data", "validation_data", "train_labels", "test_labels"))
