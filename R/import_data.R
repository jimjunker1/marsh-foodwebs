##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

import_data <- function(data_files) {
  rm(data_files)
  ###+++ Helper functions +++###
  read_excel_allsheets <- function(filename, tibble = TRUE) {
    if(grepl(".csv", filename)){
      x <- read.csv(filename, header = TRUE)
    } else{
    require(readxl)
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    }
    x
  }
  
  ###+++ End Helper functions +++###

  all_files = list.files(path = "./data/raw-data","*", recursive = TRUE, full.names = TRUE)
  zip_files = all_files[grepl("*.zip", all_files)]
  map(zip_files, ~.x %>% unzip(., exdir = "./data/raw-data", junkpaths = TRUE, overwrite = TRUE))
  
  all_files = list.files(path = "./data/raw-data","*", recursive = TRUE, full.names = TRUE)
  xml_files = all_files[grepl("*.xml",all_files)]
  excel_files = all_files[grepl("*.xls*|*.csv", all_files)]
  xls_names = excel_files %>% strsplit(.,"/") %>% map(~.x %>% pluck(length(.))) %>% str_remove(".xls.{0,1}|.csv")
  xls_sheet_lists = lapply(excel_files, read_excel_allsheets) %>% setNames(., nm = xls_names)

  saveRDS(xls_sheet_lists, file = "./data/raw-data/data_lists.rds")
}
