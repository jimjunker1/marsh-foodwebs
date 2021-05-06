##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param nameme1
fill_traits <- function(spp_taxonomy_list =
                        readRDS(file_in("./data/derived-data/spp_taxonomy_list.rds"))) {

  
  spp_taxonomy = spp_taxonomy_list %>% map(~.x %>% dplyr::filter(rank %in% c("Species")) %>% select(name)) %>% unlist
  gen_taxonomy = spp_taxonomy_list %>% map(~.x %>% dplyr::filter(rank %in% c("Genus")) %>% select(name)) %>% unlist %>% unique

  taxa_update = readRDS(file_in("./data/derived-data/taxa_update.rds"))
  
  if((Sys.Date() - taxa_update) > 100){
    #validate names
  fish_valid_taxonomy = rfishbase::validate_names(spp_taxonomy, server = 'fishbase') 
  sealife_valid_taxonomy = rfishbase::validate_names(spp_taxonomy, server = 'sealifebase') 
  # get metadata of valid names
  fish_valid_meta = rfishbase::species(fish_valid_taxonomy, server = 'fishbase') %>% 
    dplyr::mutate(server = 'rfishbase')
  sealife_valid_meta = rfishbase::species(sealife_valid_taxonomy, server = 'sealifebase') %>% 
    dplyr::mutate(server = 'sealifebase')
  taxonomy_valid_meta = bind_rows(fish_valid_meta, sealife_valid_meta)
  saveRDS(taxonomy_valid_meta, file = "./data/derived-data/taxonomy/taxonomy_valid_meta.rds")
  # pull Length-mass regression from valid names
  fishLM = rfishbase::length_weight(fish_valid_taxonomy, server = 'rfishbase') %>%
    dplyr::mutate(server = 'rfishbase')
  sealifeLM = rfishbase::length_weight(sealife_valid_taxonomy, server = 'sealifebase') %>%
    dplyr::mutate(server = 'sealifebase')
  taxonomyLM = bind_rows(fishLM, sealifeLM)
  saveRDS(taxonomyLM, "./data/derived-data/taxonomy/taxonomyLM.rds")
  # pull length-length regressions for valid names
  fishLL = rfishbase::length_length(fish_valid_taxonomy, server = 'rfishbase') %>%
    dplyr::mutate(server = 'rfishbase')
  sealifeLL = rfishbase::length_length(sealife_valid_taxonomy, server = 'sealifebase') %>%
    dplyr::mutate(server = 'sealifebase')
  taxonomyLL = bind_rows(fishLL, sealifeLL)
  saveRDS(taxonomyLL, "./data/derived-data/taxonomy/taxonomyLL.rds")
  
  # save date code to run in future only ~100 days
  saveRDS(Sys.Date(), "./data/derived-data/taxa_update.rds")
  }
  taxonomy_valid_meta
  # common_valid_taxonomy = rfishbase::sci_to_common(spp_valid_taxonomy)
  
  
}
