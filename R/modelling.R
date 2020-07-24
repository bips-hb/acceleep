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
  model <- match.arg(model)
  placement <- match.arg(placement)

  file_path <- here::here("data/processed-combined", model, paste0(placement, "-", res, "hz", ".rds"))

  if (!fs::file_exists(file_path)) {
    stop("File ", file_path, " does not exist. Check your model/placement/resolution arguments.")
  }

  # Read and ungroup data because I forgot to ungroup when saving the downsampled data
  # and now I have to fix it, but don't want to resave the data just yet.
  readRDS(file_path) %>%
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
    dplyr::select("interval", "ID", outcome) %>%
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

  set.seed(random_seed)
  ids_validation <- sample(ids, size = floor(length(ids) * val_split)) # 1/3 of IDs
  ids_train <- ids[!(ids %in% ids_validation)]

  # Stop if somehow train + test ids are not the same as the initial ids
  stopifnot(
    identical(sort(c(ids_validation, ids_train)), sort(ids))
  )

  # split data into train/validation sets
  dat_validation <- full_data %>%
    dplyr::filter(.data$ID %in% ids_validation)

  dat_training <- full_data %>%
    dplyr::filter(.data$ID %in% ids_train)

  list(
    training = dat_training,
    validation = dat_validation
  )
}

#' Split training/validation datasets into separate data and labels
#'
#' This function also normalizes XYZ and desired outcome, where the validation data is
#' scaled using the same mean and standard deviation as the training data.
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
  # This is sloppy but at least it gets the job done
  # I can agonize over this later
  training_meansd <- training_data %>%
    dplyr::summarize_at(
      dplyr::vars(.data$X,.data$Y, .data$Z),
      list(mean = mean, sd = sd), na.rm = TRUE
    )

  training_xyz <- training_data %>%
    dplyr::select(.data$X, .data$Y, .data$Z) %>%
    dplyr::mutate(
      X = (.data$X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (.data$Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (.data$Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

  training_labels <- extract_outcome(training_data, outcome = outcome, output_type = "numeric")

  # Validation data and labels
  # Scaling XYZ and labels with same mean/sd as training data!
  validation_xyz <- validation_data %>%
    dplyr::select(.data$X, .data$Y, .data$Z) %>%
    dplyr::mutate(
      X = (.data$X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (.data$Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (.data$Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

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

#' Defunct because silly: Convert Accelerometer tbl to Array
#'
#' @param accel_tbl Input tbl with columns X/Y/Z, one row per measurement.
#' @inheritParams generate_ts_chunk
#'
#' @return An array of dimensions c(chunks_per_tbl, 3, rows_per_chunk)
#' @export
#' @importFrom purrr map
#' @examples
#' \dontrun{
#' convert_tbl_array(accel_tbl, interval_length = 30, res = 20)
#' }
convert_tbl_array <- function(accel_tbl, interval_length, res) {

  .Deprecated("keras_reshape_accel")

  rows_per_chunk <- interval_length * res
  chunks_per_tbl <- nrow(accel_tbl) / (rows_per_chunk)
  #browser()
  # Generate the chunk ids and but drop potential overhead
  # If too many/too few ids are generated e.g. due to uneven number of measurements
  # in the last chunk, this would otherwise cause recycling issues
  # TLDR make sure it's ok if last chunk has less than exactly 30s of measurements
  # Wait no is that even sensible? should trailing measurements be dropped?
  # Check via nrow(accel_tbl) %% (res * interval_length) == 0
  index_along_rows <- seq_len(nrow(accel_tbl))
  chunk_ids <- rep(seq_len(chunks_per_tbl), each = rows_per_chunk)[index_along_rows]

  # accel_tbl %>%
  #   as.matrix() %>%
  #   split(f = chunk_ids) %>%
  #   lapply(matrix, nrow = 3) %>%
  #   # lapply(matrix, nrow = 3, byrow = TRUE) %>%
  #   unlist() %>%
  #   array(dim = c(chunks_per_tbl, 3, rows_per_chunk))
  #   # array(dim = c(chunks_per_tbl, rows_per_chunk, 3))

  accel_tbl %>%
    split(f = chunk_ids) %>%
    purrr::map(~{
      .x %>%
        as.matrix(ncol = 3, nrow = rows_per_chunk) %>%
        t() %>%
        as.numeric()
    }) %>%
    unlist() %>%
    array(dim = c(chunks_per_tbl, rows_per_chunk, 3))
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
#'   interval_length = 30, res = 100
#' )
#' }
keras_prep_lstm <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right"),
  outcome = c("MET", "kJ", "Jrel"),
  random_seed = 11235813, val_split = 1/3,
  interval_length = 30, res = 100
) {
  # browser()
  # Aggregating subject data for model/placement
  full_data <- get_combined_data(model = model, placement = placement, res = res)

  # Split into train / validation datasets
  c(training_data, validation_data) %<-% make_initial_splits(
    full_data, random_seed = random_seed, val_split = val_split
  )

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

globalVariables(c("training_data", "validation_data", "train_labels", "test_labels"))
