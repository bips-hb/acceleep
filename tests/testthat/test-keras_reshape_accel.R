test_that("keras_reshape_accel() reshapes correctly", {
  # Sample data
  res <- 10             # Starting wit 20Hz instead of 100Hz or simplicity
  interval_length <- 30 # 30 seconds seems reasonable due to MET limitation
  n_chunks <- 10        # Arbitrarily chosen, setting 120 results in 1 hour of simulated data

  # Generate 10 consecutive chunks at 20Hz, 30s per chunk
  accel_sim_tbl <- generate_ts_dataset(
    res = n_chunks,
    interval_length = interval_length,
    n_chunks = n_chunks
  )

  # unname for easier comparison as array won't have xyz names
  accel_sim_mat <- unname(as.matrix(accel_sim_tbl[-1]))

  test_array <- keras_reshape_accel(
    accel_sim_mat, interval_length = interval_length, res = res
  )

  # Check correct dimensions (the easy bit)
  expect_equal(
    dim(test_array),
    c(n_chunks, interval_length * res, 3)
  )

  # Check values are in the correct order (the hard bit)

  # Check if all samples are identical, sample by sample
  # (.x - 1) * (res * interval_length) calculates an offset, .x = sample id
  # for .x == 1 this would correspond to the second sample, hence (.x - 1)
  purrr::walk(seq_len(n_chunks), ~{
    expect_identical(
      test_array[.x, seq_len(interval_length * res), ],
      accel_sim_mat[(.x - 1) * (res * interval_length) + seq_len(interval_length * res), ]
    )
  })
})
