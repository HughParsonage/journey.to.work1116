library(data.table)
library(hutils)
library(magrittr)

JTW_2011_SA2 <-
  fread("~/ABS-Data/2011-HomeSA2-WorkSA2/2011-HomeSA2-WorkSA2.csv", header = FALSE, na.strings = "") %>%
  drop_empty_cols() %>%
  setnames(1:3, c("D_SA2_NAME11", "O_SA2_NAME11", "workers")) %>%
  .[, D_SA2_NAME11 := zoo::na.locf(D_SA2_NAME11)] %>%
  .[D_SA2_NAME11 != "Total"] %>%
  .[O_SA2_NAME11 != "Total"] %>%
  set_cols_first("O_SA2_NAME11") %>%
  .[workers > 0] %T>%
  fwrite("~/ABS-Data/O_SA2_NAME11-D_SA2_NAME11-workers.csv") %>%
  .[]

