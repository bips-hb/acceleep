# As per Marvin ----
n <- 30 # Number of seconds per interval
p <- 3 # No. of accelerometry axes
t <- 100 # seconds
res <- 20 # measures/sec

# Original data
x <- array(rnorm(n * p * t), dim = c(n, p, t))
y <- matrix(rnorm(n * t), nrow = n)

dim(x)
dim(y)

# Training data
xx <- array(rnorm(n * p * t), dim = c(n * res, p, t / res))
yy <- rnorm(n * t/res) # Labels

dim(xx)
dim(yy) # TIL vectors don't have a dim()


# Objectives: ----
# - Generating data close to the real thing
# - Feed this data into keras to determine correct tensor shape
library(acceleep)

res <- 10             # Starting wit 20Hz instead of 100Hz or simplicity
interval_length <- 30 # 30 seconds seems reasonable due to MET limitation
n_chunks <- 10       # Arbitrarily chosen, 120 results in 1 hour of simulated data

# Generate 10 consecutive chunks at 20Hz, 30s per chunk
accel_sim_tbl <- generate_ts_dataset(
  res = n_chunks,
  interval_length = interval_length,
  n_chunks = n_chunks
)

# Ignoring sequence information:
# One matrix (res * interval_length * n_chunks) x 3
accel_sim_mat <- as.matrix(accel_sim_tbl[-1])

xx <- array(
  as.numeric(accel_sim_mat),
  dim = c(res * interval_length * n_chunks, res * interval_length, 3)
)

dim(xx)

# Now how to convert to appropriate array?


# Associated METs, one measurement per chunk
yy <- rnorm(10, 30, 15)
