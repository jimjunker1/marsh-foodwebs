here::i_am("packages.R")
## library() calls go here
library(here)
library(dotenv)
library(drake)
library(XML)
library(methods)
library(tidyverse)
library(xml2)
library(readxl)
library(rglobi)
library(rmarkdown)
library(fuzzySim)
library(taxize)
library(rfishbase)
library(igraph)
library(vegan)
library(textclean)

remotes::install_github("ropensci/rmangal")
remotes::install_github("FMestre1/fw_package")

'%ni%' <- Negate('%in%')
# monitor raw-data folder for changes
source("./R/foodweb_functions.R")
source("./R/import_data.R")
import_data()
if(!all(grepl(".*_df_traits|.*_dt_metadata.csv|.*_df_merged_metadata_traits.csv", 
              list.files("./data/raw-data/DATABASES/TOFF/")))){
  
  source("./R/R-Script_TOFF_Data_Release_2021.R")
  message("TOFF database updated.")
  TOFF_import()
  
} else{print("TOFF database is up to date.")}

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

data_list <<- readRDS("./data/raw-data/data_lists.rds")

