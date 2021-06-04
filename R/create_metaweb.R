##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_nodes
##' @param agg_matrix
##' @param GoMexSI_df
create_metaweb <- function(spp_nodes = foodweb_data[["spp_nodes"]], 
                           agg_matrix = foodweb_data[['agg_matrix']],
                           GoMexSI_df = readRDS(file_in("./data/derived-data/foodwebs/GoMexSI_df.rds"))) {

  #!++++    HELPER FUNCTIONS    ++++!#
  square_matrix = function(x,...){
    if(dim(x)[2] == 2){
      x$pres = 1
    } else{
      x = x[,1:3]
    }
    col_names = unlist(unique(x[,1]));col_name = as.character(names(x)[1])
    row_names = unlist(unique(x[,2]));row_name = as.character(names(x)[2])
    full_names = unique(c(col_names, row_names))
    full_mat = expand.grid(col_name = full_names, row_name = full_names) %>%
      setNames(., nm = c(col_name, row_name))
    pres_matrix = full_join(full_mat, x,  by = c('predator_filled', 'prey_filled'),
                            keep = FALSE) %>% distinct(.keep_all = TRUE) %>%
      pivot_wider(names_from = 'predator_filled', values_from = 'pres') %>%
      column_to_rownames('prey_filled')
    pres_matrix[is.na(pres_matrix)] <- 0 
    pres_matrix
  }
  
  #!++++   END HELPER FUNCTION    ++++!#
  
  # create long df of consumer-resource interactions from agg_matrix and expand.grid based on spp_nodes
  
  mccann_expanded_feeding_links = agg_matrix %>% data.frame %>% 
    rownames_to_column('prey_group') %>% 
    pivot_longer(-prey_group, names_to = "predator_group", values_to = "pres") %>%
    dplyr::filter(pres == 1) %>%
    dplyr::mutate(merge_group = make.names(predator_group)) %>%
    full_join(spp_nodes %>% dplyr::mutate(merge_group = make.names(group)) %>%
                select(merge_group, group, code, generic, specific_name, lifestage), by = "merge_group") %>%
    dplyr::mutate(interaction = 'eats') %>% select(-merge_group) %>% dplyr::filter(!is.na(group)) %>%
    dplyr::mutate(merge_group = make.names(prey_group)) %>%
    full_join(spp_nodes %>% dplyr::mutate(merge_group = make.names(group)) %>%
                select(merge_group, code, generic, specific_name, lifestage), by = "merge_group") %>%
    select(predator_group = "group", predator_code = "code.x", predator_generic = "generic.x",
           predator_specific = "specific_name.x", predator_lifestage = "lifestage.x", `interaction`,
           prey_group = "merge_group", prey_code = "code.y", prey_generic = "generic.y", prey_specific = "specific_name.y",
           prey_lifestage = "lifestage.y") %>%
    dplyr::filter(!is.na(predator_group) & prey_group != "NA.")

  
  # debugonce(square_matrix)
  interaction_list = mccann_expanded_feeding_links %>%
    dplyr::mutate(predator_filled = ifelse(is.na(predator_specific), predator_generic, predator_specific),
                  prey_filled = ifelse(is.na(prey_specific), ifelse(is.na(prey_generic), prey_group, prey_generic), prey_specific)) %>%
    select(predator_filled, prey_filled) %>%
    bind_rows(GoMexSI_df %>% select(predator_filled = "source_taxon_name", prey_filled = 'target_taxon_name'))
  
  full_matrix =  interaction_list %>%
    unique %>% dplyr::mutate(pres = 1) %>% 
    square_matrix %>% as.matrix
  
  trophic_sort = full_matrix %>% apply(.,2, sum) %>% sort
  
  full_matrix = full_matrix[rownames(full_matrix) %in% names(trophic_sort), colnames(full_matrix) %in% names(trophic_sort)]
  
  return(list(interaction_list = full_matrix))
}
