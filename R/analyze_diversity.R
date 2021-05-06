##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param saltmarsh_data
analyze_diversity <- function(saltmarsh_data) {

  community_matrix = spp_abundance %>% select(Location, site, sp_codes, n) %>% 
    dplyr::mutate(loc_site = interaction(Location, site)) %>%
    group_by(loc_site) %>%
    pivot_wider(names_from = "sp_codes", values_from = "n", values_fn = sum, values_fill = 0) %>%
    ungroup %>%
    dplyr::filter(if_any(where(is.numeric), zeros))
  
  site_nmds = vegan::metaMDS(community_matrix %>% select(-Location:-loc_site), na.rm = TRUE, trymax = 100, maxit = 500, parallel = 4)
  plot(site_nmds)
  
  site_nmds.scrs = community_matrix %>% select(Location:loc_site) %>%
    bind_cols(as.data.frame(vegan::scores(site_nmds), display = 'sites'))
  
  hulls = site_nmds.scrs %>% 
    group_by(site) %>%
    nest %>%
    dplyr::mutate(hull = map(data, ~find_hull(.x))) %>%
    dplyr::select(-data) %>% unnest(cols = c(hull)) %>%
    dplyr::select(site, NMDS1, NMDS2)
  
  hulls %>%
    ggplot(aes(x = NMDS1, y = NMDS2)) +
    geom_polygon(aes(color = site, fill = site), alpha = 0.3) +
    geom_point(aes(fill = site), shape = 21, color = 'black', size = 2)+
    geom_point(data = site_nmds.scrs, aes(fill = site), color = 'black', shape = 21, size = 1.3)+
    theme_minimal()
  
  community_rank = spp_abundance %>% select(site, sp_codes, n) %>% 
    group_by(site) %>%
    dplyr::mutate(rel_n = n/sum(n),
                  n_rank = row_number(desc(n))) 
  community_rank %>%
    ggplot(aes(x = n_rank, y = log(rel_n)))+
    geom_line(aes(color = site), size = 1)+
    geom_point(aes(color = site), size = 3)+
    theme_minimal()
  

}
