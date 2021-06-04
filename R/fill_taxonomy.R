##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_list represents the full species list from food web data 
fill_taxonomy <- function(spp_list = foodweb_data[["spp_list"]]) {
  #!++++    Helper functions    ++++!

  '%ni%' = Negate('%in%')
  
  sci2comm_concat <- function(x){
    temp <- taxize::sci2comm(x)
    if(length(temp) == 0) NA else paste0(temp[[1]], collapse = ", ")
  }
  #!++++   End Helper functions    ++++!
  spp_list = gsub(" sp.| spp.","", spp_list)
  spp_taxonomy_list = readRDS(file = "./data/derived-data/spp_taxonomy_list.rds")
  na_list = readRDS(file = "./data/derived-data/bad_names.rds")
  full_names = c(names(spp_taxonomy_list),na_list) %>% gsub(" sp.","",.)
  
  if(!all(spp_list %in% full_names)){
    new_spp = spp_list[spp_list %ni% full_names]
    new_class_search = taxize::classification(new_spp, db = "itis")
    new_class = new_class_search[!is.na(new_class_search)]
    new_full_tax = c(spp_taxonomy_list, new_class) %>% map(as_tibble)
    saveRDS(new_full_tax, file = "./data/derived-data/spp_taxonomy_list.rds")
    new_nas = new_class_search[is.na(new_class_search)]
    new_na_list = c(na_list, names(new_nas))
    saveRDS(new_na_list, file = "./data/derived-data/bad_names.rds")
  } else{ print("Species up to date.")}
  
  spp_taxonomy_df = spp_taxonomy_list %>%
    map(~.x %>% dplyr::select(name, rank) %>%
          pivot_wider(names_from = 'rank', values_from = 'name', values_fn = list)) %>%
    bind_rows(.id = 'species_name') %>%
    dplyr::select(species_name, kingdom, phylum, class, order, family, tribe, genus, species) %>%
    dplyr::mutate(across(everything(),~na_if(.x, "NULL"))) %>%
    unnest(everything()) %>% 
    # slice_head(n = 5) %>%
    rowwise %>%
    dplyr::mutate(common_name = sci2comm_concat(species_name)) %>%
    as_tibble
  
  x = names(spp_taxonomy_list)[names(spp_taxonomy_list) %ni% spp_taxonomy_df$species_name] %>%
    taxize::classification(., db = 'worms') #%>%
  
  y = x[!is.na(x)] %>%
    map(~.x %>% dplyr::select(name, rank) %>%
          pivot_wider(names_from = 'rank', values_from = 'name', values_fn = list)) %>%
    bind_rows(.id = 'species_name') %>%
    dplyr::rename_with(tolower) %>%
    dplyr::select(species_name, kingdom, phylum, class, order, family, genus, species) %>%
    dplyr::mutate(across(everything(),~na_if(.x, "NULL"))) %>%
    unnest(everything()) %>% 
    # slice_head(n = 5) %>%
    rowwise %>%
    dplyr::mutate(common_name = sci2comm_concat(species_name)) %>%
    as_tibble
  
  spp_taxonomy_df = bind_rows(spp_taxonomy_df, y)
  saveRDS(spp_taxonomy_df, file = "./data/derived-data/spp_taxonomy_df.rds")
  
  return(spp_taxonomy_df = spp_taxonomy_df)
}
