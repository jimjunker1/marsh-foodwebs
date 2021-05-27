##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_list represents the full species list from food web data 
fill_taxonomy <- function(spp_list = foodweb_data[["spp_list"]]) {
  #!++++    Helper functions    ++++!

  '%ni%' = Negate('%in%')
  #!++++   End Helper functions    ++++!
  spp_taxonomy_list = readRDS(file = "./data/derived-data/spp_taxonomy_list.rds")
  na_list = readRDS(file = "./data/derived-data/bad_names.rds")
  
  new_spp_names = spp_list[spp_list %ni% names(spp_taxonomy_list) & spp_list %ni% na_list]
  
  new_spp_search = taxize::classification(new_spp_names, db = 'itis')
  
  nas = new_spp_search[is.na(new_spp_search)]
  short_tax = 
  new_spp = new_spp_list[!is.na(new_spp_list)]
  
  spp_taxonomy_list = rlist::list.merge()
  if(file.exist(path = "./data/derived-data/spp_taxonomy_list.rds") && length(new_spp_files) == 0){
    cat("Species list up to date. The list of groups with NAs are:",na_list)
  } else{
    
    full_spp_list = readRDS(file = "./data/derived-data/full_spp_list.rds")
    full_spp_list = full_spp_list[!is.na(full_spp_list)]
    full_names = names(full_spp_list)
  
    short_spp_file_list = list.files(path = "./data/raw-data/", pattern = "*spp_list.rds", full.names = TRUE)
    short_spp_files= lapply(short_spp_file_list, readRDS)
    short_nas = short_spp_files %>% map(~.x %>% .[is.na(.)]) %>% .[lapply(.,length) >0] %>% flatten
    short_spp_files = short_spp_files %>% map(~.x %>% .[!is.na(.)]) %>% flatten
    new_names = trimws(names(short_spp_files))
  
    new_spp = short_spp_files[new_names %ni% full_names]
    new_full_spp_list = rlist::list.merge(full_spp_list, new_spp)
  
    nas = short_nas[trimws(names(short_nas)) %ni% names(new_full_spp_list)]
  
    nas = nas[trimws(names(nas)) %ni% na_list]
    
  if(length(nas) >0){
    na_search = taxize::classification(trimws(names(nas)), db = "itis")
    new_nas = na_search[is.na(na_search)]
    filled_nas = na_search[!is.na(na_search)]
    new_full_spp_list = rlist::list.merge(new_full_spp_list, filled_nas)
    nas = c(na_list, names(new_nas))
  }
 saveRDS(new_full_spp_list, "./data/derived-data/spp_taxonomy_list.rds")
 saveRDS(names(nas), "./data/derived-data/bad_names.rds")
  }
}
