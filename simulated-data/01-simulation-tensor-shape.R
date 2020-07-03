# Objectives: ----
# - Generating data close to the real thing
# - Feed this data into keras to determine correct tensor shape
library(acceleep)

# Simpler approach using keras ----
res <- 10             # Starting wit 20Hz instead of 100Hz or simplicity
interval_length <- 30 # 30 seconds seems reasonable due to MET limitation
n_chunks <- 10        # Arbitrarily chosen, setting 120 results in 1 hour of simulated data

# Generate 10 consecutive chunks at 20Hz, 30s per chunk
accel_sim_tbl <- generate_ts_dataset(
  res = n_chunks,
  interval_length = interval_length,
  n_chunks = n_chunks
)

accel_mat <- unname(as.matrix(accel_sim_tbl[-1]))
accel_array <- keras::array_reshape(accel_mat, c(n_chunks, res * interval_length, 3))

dim(accel_array) # c(10, 300, 3)

accel_array[1, 1:2, ]
head(accel_mat, 2)

all.equal(accel_array[1, 1:2, ], head(accel_mat, 2))

accel_array[2, 1:2, ]
accel_mat[res * interval_length + 1:2, ]
all.equal(accel_array[2, 1:2, ], )

# base array() ? not as easy
accel_array_base <- array(as.numeric(accel_mat), c(n_chunks, res * interval_length, 3))

# identical
accel_mat[1, ]
accel_array[1, 1, ]
accel_array_base[1, 1, ]

# Not identical
accel_mat[2, ]
accel_array[1, 2, ]
accel_array_base[1, 2, ]
