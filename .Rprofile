if (interactive()) {
  suppressMessages(require(usethis))
}


# Setting up python stuff
Sys.setenv("PATH" = paste0("/usr/local/anaconda3/bin:", Sys.getenv("PATH")))

# use_condaenv("r-reticulate")
# Giving up, just use defaults.
# install_keras(method = "conda")
# tensorflow::install_tensorflow(method = "conda")
# reticulate::virtualenv_python()
# reticulate::conda_binary()

reticulate::use_condaenv("r-reticulate", required = TRUE)
