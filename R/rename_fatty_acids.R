##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

rename_fatty_acids <- function() {

  FA_data_list= data_list[grepl("(FA).*", unlist(names(data_list)), ignore.case = TRUE)] %>%
    flatten 
  
  FA_information = FA_data_list[grepl('.*(Information|concentrations).*', names(FA_data_list), ignore.case = TRUE)] 
    
  

}
