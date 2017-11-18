library(readxl)
library(tidyxl)
library(data.table)
library(magrittr)
library(hutils)



H_WRKPLC_1991 <-
  # convert the originals to xlsx
  tidy_xlsx("data-raw/bts_1991_jtw_summary_destination/W_HOME.xlsx", sheets = 2) %>%
  use_series("data") %>%
  lapply(as.data.table) %>%
  rbindlist

H_WRKPLC_1991_formats <-
  tidy_xlsx("data-raw/bts_1991_jtw_summary_destination/W_HOME.xlsx", sheets = 2) %>%
  use_series("formats") %>%
  lapply(as.data.table) %>%
  rbindlist

bold_indices <-
  tidy_xlsx("data-raw/bts_1991_jtw_summary_destination/W_HOME.xlsx", sheets = 2) %>%
  use_series("formats") %>%
  use_series("local") %>%
  use_series("font") %>%
  use_series("bold") %>%
  which

TOP3 <- H_WRKPLC_1991[row <= 3]

headers <- character(max(TOP3$col))
for (co in seq_len(max(TOP3$col))) {
  if (co > 1) {
     column <- TOP3[col == co]
     switch (sum(column[["data_type"]] == "character"),
             {
               headers[co] <- column[data_type == "character"][["character"]]
             },

             {
               headers[co] <- paste0(column[data_type == "character"][["character"]], collapse = "_")
             })
  }
}

headers <- headers

WorkplaceSLA_HomeSLA <- CJ(row = seq_len(max(H_WRKPLC_1991$row)),
                           col = seq_len(max(H_WRKPLC_1991$col)))

i <- 0
for (row in 1:79) {
  i <- i + 1
  for (col in 1:75) {
    Entry <- H_WRKPLC_1991[i]
    Address <- Entry$address
    Row <- Entry$row
    # Don't include subtotals
    if (AND(Row %notin% c(10, 14, 19, 22, 25, 29,
                          35, 39, 43, 46, 53, 56,
                          59, 62, 63, 70, 76, 79),
           gsub("[0-9]", "", Address, perl = TRUE) %notin% c("H", "L", "Q", "T", "W",
                                                             "AA", "AG", "AK", "AO", "AR",
                                                             "AY", "BB", "BE", "BH", "BI", "BP",
                                                             "BV", "BW"))) {

      Col <- Entry$col
      Home_SLA <-
        if (is.na(H_WRKPLC_1991[col == Col & row == 2L]$character)) {
          H_WRKPLC_1991[col == Col & row == 3L]$character
        } else {
          paste0(H_WRKPLC_1991[col == Col & row %in% c(2, 3)]$character, collapse = "_")
        }

      Home_SLA_ <- gsub("[^A-Za-z]+", "_", Home_SLA, perl = TRUE)
      Work_SLA_ <- H_WRKPLC_1991[col == 1 & row == Row]$character

      WorkplaceSLA_HomeSLA[i, workers := Entry$numeric]
      WorkplaceSLA_HomeSLA[i, "Work_SLA" := Work_SLA_]
      WorkplaceSLA_HomeSLA[i, "Home_SLA" := Home_SLA_]
    }
    i <- i + 1
  }
}

JTW_1991_SYD <-
  WorkplaceSLA_HomeSLA %>%
  .[, .(Home_SLA, Work_SLA, workers)] %>%
  .[complete.cases(.)]
JTW_1991_SYD %>%
  fwrite("data-raw/bts_1991_jtw_summary_destination/1991-HOME_SLA-Work_SLA-workers.csv")

devtools::use_data(JTW_1991_SYD)



