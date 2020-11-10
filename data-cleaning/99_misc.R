# Session info
library(keras)
library(tidyverse)
library(lubridate)
library(hms)
library(vroom)
library(reticulate)
library(tfruns)
library(rmarkdown)
library(fs)

sess <- sessioninfo::session_info()
saveRDS(sess, file = here::here("output/sessioninfo.rds"))
