library(hutils)

JTW_2016_SA2 <-
  fread("~/ABS-data/2016-SA2-POW-UR/2016-SA2-POW-UR.csv", na.strings = "", header = FALSE) %>%
  setnames(1:3, c("D_SA2_NAME16", "O_SA2_NAME16", "workers")) %>%
  .[, D_SA2_NAME16 := zoo::na.locf(D_SA2_NAME16, na.rm = FALSE)] %>%
  .[workers > 0] %>%
  drop_empty_cols() %>%
  set_cols_first("O_SA2_NAME16") %>%
  .[D_SA2_NAME16 != "POW not applicable"] %>%
  .[D_SA2_NAME16 != "Total"] %>%
  .[O_SA2_NAME16 != "Total"] %T>%
  fwrite("~/ABS-data/O_SA2_NAME16-D_SA2_NAME16-workers.csv") %>%
  .[]

devtools::use_data(JTW_2016_SA2, overwrite = TRUE)


