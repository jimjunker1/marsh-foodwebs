## library() calls go here
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
data_files = length(list.files("./data/raw-data/", "*.*"))
source("./R/foodweb_functions.R")
if(!all(grepl(".*_df_traits|.*_dt_metadata.csv|.*_df_merged_metadata_traits.csv", 
              list.files("./data/raw-data/DATABASES/TOFF/")))){
  
  source("./R/R-Script_TOFF_Data_Release_2021.R")
  TOFF_import()
  
}

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
