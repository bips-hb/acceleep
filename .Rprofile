if (interactive()) {
  suppressMessages(require(usethis))
}


# Setting up python stuff
Sys.setenv("PATH" = paste0("/usr/local/anaconda3/bin:", Sys.getenv("PATH")))

# install_keras(method = "conda")
# tensorflow::install_tensorflow(method = "conda")
# reticulate::conda_list()
# reticulate::conda_version()
# reticulate::conda_binary()
# required = TRUE seems to help.
reticulate::use_condaenv("r-reticulate", required = TRUE)
