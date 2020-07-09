# Installing keras et al
# This file is only here in case there's important tweaks to the keras setup I
# need to keep track of down the road.

# Okay then, attemp 12039: I don't even know anymore.

reticulate::use_virtualenv(virtualenv = "acceleep", required = TRUE)
reticulate::virtualenv_list()
reticulate::py_available()
reticulate:::ensure_python_initialized()
reticulate::py_available()

# keras ----
install.packages("keras")

library(keras)
install_keras(envname = "acceleep")

tensorflow::install_tensorflow(envname = "acceleep")

#reticulate::conda_install(envname = "acceleep", packages = "cudatoolkit")
