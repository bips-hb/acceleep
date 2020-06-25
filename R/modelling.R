#' Combine cleaned data for one accelerometer model / placement
#'
#' This function collects the datasets (split by subject) for a given model and placement, where the result is a
#' single dataset of all measurements across subjects.
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
#' combine_clean_data("actigraph", "hip_left")
#' }
combine_clean_data <- function(
  model = c("actigraph", "activpal", "geneactiv"),
  placement = c("hip_left", "hip_right", "thigh_right", "wrist_left", "wrist_right")
) {
  get_overview_table() %>%
    dplyr::filter(file_clean_exists, model == !!model, placement == !!placement) %>%
    dplyr::pull(file_clean) %>%
    purrr::map_df(readRDS)
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
#' @param rescale `[FALSE]` If `TRUE`, outcome variable will be rescaled using [`scale()`].
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
  xdf, outcome = c("MET", "kJ", "Jrel"), output_type = c("numeric", "tbl"),
  rescale = FALSE
  ) {

  outcome <- match.arg(outcome, choices = c("MET", "kJ", "Jrel"))

  ret <- xdf %>%
    dplyr::select("interval", "ID", outcome) %>%
    dplyr::distinct()

  if (rescale) ret[[outcome]] <- as.numeric(scale(ret[[outcome]]))

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
    dplyr::filter(ID %in% ids_validation)

  dat_training <- full_data %>%
    dplyr::filter(ID %in% ids_train)

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
#'     train_data = training_xyz,
#'     train_labels = training_labels
#'   ),
#'   validation = list(
#'     val_data = validation_xyz,
#'     cal_labels = validation_labels
#'   )
#' )
#' ```
#' @export
#' @importFrom dplyr select mutate summarize_at vars
#' @importFrom stats sd
#' @examples
#' \dontrun{
#' full_data <- combine_clean_data("activpal", "thigh_right")
#' c(training_data, validation_data) %<-% make_initial_splits(full_data, random_seed = 21421, val_split = 1/3)
#' split_data <- split_data_labels(training_data, validation_data, outcome = "kJ")
#'
#' c(train_data, train_labels) %<-% split_data$training
#' c(test_data, test_labels) %<-%  split_data$validation
#' }
split_data_labels <- function(training_data, validation_data, outcome = c("MET", "kJ", "Jrel")) {

  # This is sloppy but at least it gets the job done
  # I can agonize over this later
  training_meansd <- training_data %>%
    dplyr::summarize_at(dplyr::vars(X, Y, Z), list(mean = mean, sd = sd), na.rm = TRUE)

  training_xyz <- training_data %>%
    dplyr::select(X, Y, Z) %>%
    dplyr::mutate(
      X = (X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

  training_labels <- extract_outcome(training_data, outcome = outcome, output_type = "numeric")
  training_labels_mean <- mean(training_labels, na.rm = TRUE)
  training_labels_sd <- sd(training_labels, na.rm = TRUE)

  training_labels <- (training_labels - training_labels_mean) / training_labels_sd

  # Validation data and labels
  # Scaling XYZ and labels with same mean/sd as training data!
  validation_xyz <- validation_data %>%
    dplyr::select(X, Y, Z) %>%
    dplyr::mutate(
      X = (X - training_meansd$X_mean) / training_meansd$X_sd,
      Y = (Y - training_meansd$Y_mean) / training_meansd$Y_sd,
      Z = (Z - training_meansd$Z_mean) / training_meansd$Z_sd
    )

  validation_labels <- extract_outcome(validation_data, outcome = outcome, output_type = "numeric")
  validation_labels <- (validation_labels - training_labels_mean) / training_labels_sd

  list(
    training = list(
      train_data = training_xyz,
      train_labels = training_labels
    ),
    validation = list(
      val_data = validation_xyz,
      val_labels = validation_labels
    )
  )
}
