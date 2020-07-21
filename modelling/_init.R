library(dplyr)
library(acceleep)
library(keras)
library(cliapp)
reticulate::use_condaenv(condaenv = "acceleep", required = TRUE)

# The first python-based action after session restart always fails:
reticulate::dict(1)
# yields: Error in FUN(X[[i]], ...) : subscript out of bounds
# This ^ is here to ensure the subsequent functions are executed properly
# Can be "fixed" by calling reticulate:::ensure_python_initialized() first

# Close both GPU devices to free up resources just in case.
# Be careful not to close them on other running CUDA processes!
cuda_close_device(c(0, 1))
