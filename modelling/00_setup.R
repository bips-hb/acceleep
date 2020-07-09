# Installing keras et al
# This file is only here in case there's important tweaks to the keras setup I
# need to keep track of down the road.
# Special consideration: using renv with this via renv::use_python()
# Needs python < 3.8 for tensorflow afair

# Installs miniconda for the system (_not_ renv specific)
reticulate::install_miniconda(path = reticulate::miniconda_path())

# Uses a specific conda end and records it in lockfile
renv::use_python(
  type = "conda", name = "acceleep"
)

# Some debug checkups
# reticulate::use_condaenv()
reticulate::conda_list()
reticulate::conda_binary()
reticulate::conda_python()

# To install r and python packages from lockfile, use renv::restore() as usual

# keras ----
install.packages("keras")

library(keras)
install_keras(method = "conda", tensorflow = "gpu")

