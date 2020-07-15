# Installing keras et al
# This file is only here in case there's important tweaks to the keras setup I
# need to keep track of down the road.

# Okay then, attempt 12039: I don't even know anymore. // But it seems to work fine!
# Trying to make a conda env maybe
library(reticulate)
conda_binary()

# Create a new, project specific env
conda_create("acceleep")

# required = TRUE -> Use *this* env and no other
use_condaenv(condaenv = "acceleep", required = TRUE)

# Should show a bin/python in an "acceleep" env
conda_python(envname = "acceleep")


# keras ----
install.packages("keras")

library(keras)
install_keras(method = "conda", tensorflow = "gpu")

# The above includes the tensorflow installation
# tensorflow::install_tensorflow(envname = "acceleep")
# Not sure if the below is needed, does not look like it.
# reticulate::conda_install(envname = "acceleep", packages = "cudatoolkit")

# To release GPU memory manually (see https://github.com/rstudio/keras/issues/739)
reticulate::conda_install("acceleep", "numba")

cuda_close_device <- function(device = 0) {
  for (dev in device) {
    reticulate::py_run_string(
      glue::glue("from numba import cuda; cuda.select_device({device}); cuda.close()")
    )
  }
}

cuda_close_device(0)
