##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_list
##' @param taxonomy_df
##' @param trait_df
##' @param interaction_list
clean_marsh_data <- function(data_list, taxonomy_df
                             # , trait_df, interaction_list = metaweb[['interaction_list']]
                             ) {

  #!++++    HELPER FUNCTIONS    ++++!#
  
  '%ni%' = Negate('%in%')
  
  find_hull <- function(df) df[chull(df$NMDS1, df$NMDS2),]# function to create hull of each stream
  
  zeros <- function(x){
    x != 0
  }
  
  fix_latlong = function(x){
    x_lat = sapply(x, function(x) gsub("N", c(""), x))
    x_latlong = sapply(x_lat, function(x) gsub("W","-",x))
    x_spec_trim = sapply(x_latlong, function(x) sub("º|°","",x))
    unname(x_spec_trim)
  }
  
  #!++++   END HELPER FUNCTION    ++++!#
  
  
  #### COMPOSITION DATA ####
  
  community_objects = data_list[grepl("comp.*|abun.*",unlist(names(data_list)), ignore.case = TRUE)]
  
  spp_abundance = community_objects %>% flatten %>% .[grepl("abundance", names(.), ignore.case = TRUE)] %>% flatten_df %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(species = gsub(" spp.", " sp.", species),
                  species = trimws(species),
                  species = if_else(species == 'na', NA_character_, species),
                  `common name` = gsub("\\(?\\)","", `common name`),
                  `common name` = trimws(`common name`),
                  `common name` = if_else(`common name` == 'na', NA_character_, `common name`))

  # clean the species and common names of all taxa.
  spp_abundance$clean_spp_names = lapply(spp_abundance$species, function(a){
    if(is.na(a)){
      NA_character_
      } else{
        agrep(a,x = spp_taxonomy_df$species_name, max.distance = 0.02)
        }
    }) %>%
    map(~.x %>% pluck(1) %>% spp_taxonomy_df$species_name[.] %>% coalesce(.,NA_character_))
  
  spp_abundance$clean_common_names = lapply(spp_abundance$species, function(a){
    if(is.na(a)){
      NA_character_
      } else{
        agrep(a, x = spp_taxonomy_df$species_name, max.distance = 0.02)
        }
    }) %>%
    map(~.x %>% pluck(1) %>% spp_taxonomy_df$common_name[.] %>%coalesce(.,NA_character_))
  
  spp_abundance$common2sci = lapply(spp_abundance$`common name`, function(a){
    if(is.na(a)){
      NA_character_
    } else{
      agrep(a,x = spp_taxonomy_df$common_name, max.distance = 0.02)
    }
  }) %>%
    map(~.x %>% pluck(1) %>% spp_taxonomy_df$species_name[.] %>% coalesce(.,NA_character_)) 
  
spp_abundance = spp_abundance %>%
  unnest(everything()) %>%
  dplyr::mutate(across(c(clean_spp_names, common2sci, clean_common_names), ~recode(.x, "character(0)" = NA_character_))) %>%
  dplyr::mutate(species = case_when(stringdist::stringdist(species, clean_spp_names) <= 3 ~ clean_spp_names,
                                    (is.na(species) & !is.na(`common name`)) ~ common2sci, 
                                    TRUE ~ species),
                `common name` = case_when(stringdist::stringdist(`common name`, clean_common_names) <= 3 ~ clean_common_names,
                                          TRUE ~ `common name`)) %>%
  dplyr::select(-clean_spp_names, -clean_common_names, -common2sci)


spp_length.mass = community_objects %>% flatten %>% .[grepl("lengths", names(.), ignore.case = TRUE)] %>% flatten_df %>%
  # convert columns to numeric where appropriate
  dplyr::mutate(across(matches(')'), as.numeric)) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(across(c(`common name`, species), trimws))

  # clean the species and common names of all taxa.
  spp_length.mass$clean_spp_names = lapply(spp_length.mass$species, function(a){
    if(is.na(a)){
      NA_character_
    } else{
      agrep(a,x = spp_taxonomy_df$species_name, max.distance = 0.02)
    }
  }) %>%
  map(~.x %>% pluck(1) %>% spp_taxonomy_df$species_name[.] %>% coalesce(.,NA_character_))

  spp_length.mass$clean_common_names = lapply(spp_length.mass$species, function(a){
    if(is.na(a)){
      NA_character_
    } else{
      agrep(a, x = spp_taxonomy_df$species_name, max.distance = 0.02)
    }
  }) %>%
    map(~.x %>% pluck(1) %>% spp_taxonomy_df$common_name[.] %>%coalesce(.,NA_character_))
  
  spp_length.mass$common2sci = lapply(spp_length.mass$`common name`, function(a){
    if(is.na(a)){
      NA_character_
    } else{
      agrep(a,x = spp_taxonomy_df$common_name, max.distance = 0.02)
    }
  }) %>%
    map(~.x %>% pluck(1) %>% spp_taxonomy_df$species_name[.] %>% coalesce(.,NA_character_))  
  
  spp_length.mass = spp_length.mass %>%
    unnest(everything()) %>%
    dplyr::mutate(across(c(clean_spp_names, common2sci, clean_common_names), ~recode(.x, "character(0)" = NA_character_))) %>%
    dplyr::mutate(species = case_when(stringdist::stringdist(species, clean_spp_names) <= 3 ~ clean_spp_names,
                                      (is.na(species) & !is.na(`common name`)) ~ common2sci, 
                                      TRUE ~ species),
                  `Common Name` = case_when(stringdist::stringdist(`common name`, clean_common_names) <= 3 ~ clean_common_names,
                                            TRUE ~ `common name`)) %>%
    dplyr::select(-clean_spp_names, -clean_common_names, -common2sci, -contains('mesh'))
  
  spp_length.mass_sum = spp_length.mass %>%
    group_by(`collection date`, location, site, gear, `common name`, species) %>%
    dplyr::summarise(across(matches(')'), ~ sum(!is.na(.x)), .names = "{.col}_n"))
  
  spp_length_n_sum = spp_abundance %>% dplyr::select(-`time start`) %>%
    group_by(`collection date`, location, site, gear, `common name`, species) %>%
    dplyr::summarise(n = sum(n, na.rm = TRUE)) %>%
    left_join(spp_length.mass_sum)
  
  
  # test df for length-abundance merging
  
  test_spp_n = spp_abundance %>%
    slice_sample(n = 15)
  
  #'
  #'
  merge_length_n = function(n_df = NULL, len_df = NULL, groupings = NULL,...){
    require(dplyr);require(purrr)
    # initial parameter tests
    if(is.null(n_df) | is.null(len_df))stop("Abundance dataframe (n_df) or lengths dataframe (len_df) are NULL/missing. Must be provided.")
    if(is.null(groupings))warning("Groupings parameter is NULL. Groupings to merge abundance and lengths is inferred by `left_join`")
    
    # force to dataframe/tibble n_df and len_df if not currently
    if(!is.data.frame(n_df) | !is_tibble(n_df)) n_df = as_tibble(n_df)
    if(!is.data.frame(len_df) | !is_tibble(len_df)) len_df = as_tibble(len_df)
    
    #split into list by rows 
    n_list = split(n_df, seq(nrow(n_df))) 
    
    # merge lengths_df into list
    n_merge_list = purrr::map(n_list, function(a) a %>% 
                          dplyr::mutate(len_df = left_join(a,len_df) %>% 
                                          select(matches('tl')) %>%
                                          nest %>% flatten %>% flatten)) %>%
      bind_rows %>%
      dplyr::mutate(length_n = lengths(len_df),
                    can_merge = if_else(!is.na(len_df) & n == length_n , TRUE, FALSE),
                    sub_lengths = case_when(is.na(len_df) | n >1 & length_n == 1 ~ FALSE,
                                            can_merge == TRUE ~ FALSE,
                                            !is.na(len_df) & n > length_n ~ TRUE,
                                            TRUE ~ FALSE)) %>%
      rowwise %>%
      dplyr::mutate(tl_mm = ifelse(can_merge, list(len_df) , ifelse(sub_lengths, purrr::map2(len_df, n, sample, replace = TRUE), list(NA_real_)))) %>%
      dplyr::select(-len_df, -can_merge, -sub_lengths)
  }
  
  
  spp_length_abundance = merge_length_n(n_df = spp_abundance, len_df = spp_length.mass)
    
  
  pond_abundance = spp_abundance %>%
    dplyr::filter(grepl("pond", Location) & !grepl('na',`common name`)) %>%
    dplyr::mutate(clean_taxa = )
  
  pond_foodweb = pond_abundance %>% select(species) %>% 
    left_join(interaction_list %>% dplyr::rename(species = 'predator_filled')) %>% unique
    left_join(interaction_list %>% dplyr::rename(species = 'prey_filled'))
    

  
  
  
  spp_length.mass %>%
    dplyr::filter(Species == "Anchoa mitchilli") #%>%
  ggplot(aes(x = 'TL (mm)'))+
    geom_density() +
    theme_minimal()
  
  #### TRACER DATA ####
  foodweb_objects = data_list[grepl("food*",unlist(names(data_list)), ignore.case = TRUE)]
  
  # cwc_samples = foodweb_objects[[1]] %>% flatten %>% bind_rows
  # cwc_d13C_samples = foodweb_objects[[2]] %>% flatten %>% bind_rows
  # 
  # clean the tracer data
  isotope_objects_list = data_list[grepl("(SIA|stable|isotope).*", unlist(names(data_list)), ignore.case = TRUE)] %>%
    flatten
  # read in isotope data and combine
  # isotope_objects = lapply(isotope_objects_list, read_excel_allsheets) %>% flatten
  # remove lists with dimensions of zero
  remove_list = vector('logical', length(isotope_objects_list))
  for(i in 1:length(isotope_objects_list)){
    remove_list[i] = any(dim(isotope_objects_list[[i]]) != 0)
  }
  isotope_objects = isotope_objects_list[remove_list]
  
  
  
  
  FA_objects_list = data_list[grepl("(FA).*", unlist(names(data_list)), ignore.case = TRUE)] %>%
    flatten
  
  
  # add in CWC_ID variable
  CWC_add_list = vector('logical', length(isotope_objects))
  for(i in 1:length(isotope_objects)){
    CWC_add_list[i] = all(!grepl("^CWC.*|DATABASE ID",names(isotope_objects[[i]]), ignore.case = TRUE))
  }
  # fix lat and long to standard format
  # debugonce(fix_latlong)
  isotope_objects[CWC_add_list] = lapply(isotope_objects[CWC_add_list], function(x){
    x %>% dplyr::mutate(CWC_ID = NA) 
  })
  # standardize column names
  full_names_list = lapply(isotope_objects, names) %>% unlist %>% unique
  old_names = list(full_names_list[grepl("^CWC.*|DATABASE ID", full_names_list)],
                   full_names_list[grepl("laboratory", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("individual", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("date", full_names_list, ignore.case= TRUE)],
                   full_names_list[grepl("habitat|location", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("scientific|species", full_names_list, ignore.case= TRUE)],
                   full_names_list[grepl("common", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("node.*group", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("node.*code", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("tissue", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("C:N|CN", full_names_list, ignore.case = TRUE)],
                   full_names_list[grepl("notes", full_names_list, ignore.case = TRUE)])
  
  
  new_names = list("cwcID",
                   "labID",
                   "indID",
                   "date",
                   "habitat",
                   "sci_name",
                   "common_name",
                   "node_group",
                   "node_code",
                   "material",
                   "cn_mass",
                   "notes")
  namekey = map2(old_names,new_names, function(x,y) setNames(rep(y,length(x)), nm = x)) %>% flatten %>% unlist
  
  isotope_objects = lapply(isotope_objects, function(x){
    x_fix = x %>% 
      dplyr::mutate(across(any_of(c("Latitude","Longitude")), .fns= ~fix_latlong(.x))) %>%
      dplyr::mutate(across(any_of(c("Latitude","Longitude")), .fns = ~as.numeric(.x))) %>%
      dplyr::mutate(across(matches("Date"), .fns = ~as.Date(.x, format = c("%B %Y","%Y-%m-%d")))) 
    
    names(x_fix) = recode(names(x_fix), !!!namekey)
    x_fix
  })
  
  tracer_df = isotope_objects %>% bind_rows %>%
    select(cwcID, labID, indID, Other_ID, Other_ID_1, Other_ID_2, `Other ID Number`,
           Latitude,Longitude, date, Month, Year, Season,
           Site, Area, Gear,habitat, sci_name, 
           common_name, TL_mm = "Total Length (mm)",
           FL_mm = "Fork Length (mm)", carapace_width_mm = "Carapace width (mm)",
           mass_g = "Weight (grams)", node_group, node_code,
           material, Sex,
           C_perc = "%C", N_perc = "%N", S_perc = '%S',
           cn_mass,d13C, d15N, d34S, d13C_Val, d13C_ILE, d13C_LEU,
           d13C_Thr, d13C_PHE, n, notes, everything())
  
  
  
  saveRDS(tracer_df, file = "./data/derived-data/tracers/tracers_full.rds")

  

  
  
  
  

  }
