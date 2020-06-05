# Basic array experiments b/c I don't understand things that are not rectangular

# tensor dimensions c(1, 2, 3)
# c(anzahl der intervalle, 3 (XYZ achsen), messungen pro intervall = res * interval_length)
# anzahl der intervalle = n_kinder *
# dim 1 passend zu MET messung

# Setting interval_length = 10, res = 1, static XYZ measurements (to keep track of)
# this is a set of 5 chunks a 10 measurements
xdat <- tibble::tibble(
  timestamp = seq(1, 50, 1),
  x = rep(1, times = 50),
  y = rep(2, times = 50),
  z = rep(3, times = 50)
)

# XYZ matrix without timestamp
accel_mat <- as.matrix(xdat[-1])

# Correct shape (I think), but XYZ are not "filled" correctly,
# i.e. in the matrices the first matrix is filled with x values, then y, etc.
array(accel_mat, dim = c(10, 3, 5))

# Would result in mixup
array(as.numeric(accel_mat), dim = c(10, 3, 5))


# This works but can not possibly be a good solution:
accel_array <- split(accel_mat,  (xdat$timestamp - 1) %/% 10) %>%
  lapply(matrix, ncol = 3) %>%
  unlist() %>%
  array(dim = c(10, 3, 5)) # intervals, # accel axes, 5 intervals (?)

dim(accel_array)


# Combining multiple datasets, i.e. per child
abind::abind(accel_array, accel_array)
