library(data.table)
library(hutils)
library(magrittr)

JTW_2006_SLA <-
  fread("~/ABS-data/O_SLA_NAME06-D_SLA_NAME06-workers.csv") %>%
  .[workers > 0] %T>%
  fwrite("~/ABS-data/O_SLA_NAME06-D_SLA_NAME06-workers.csv") %>%
  .[]

devtools::use_data(JTW_2006_SLA)
