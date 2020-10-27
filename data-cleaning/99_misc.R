# Session info

sess <- sessioninfo::session_info()
saveRDS(sess, file = here::here("output/sessioninfo.rds"))
