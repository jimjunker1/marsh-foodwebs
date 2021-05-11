##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_list
clean_marsh_data <- function(data_list, trait_df) {

  #!++++    HELPER FUNCTIONS    ++++!#
  
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

  #### COMPOSITION DATA ####

  community_objects = data_list[grepl("comp.*|abun.*",unlist(names(data_list)), ignore.case = TRUE)]
  
  spp_abundance = community_objects %>% flatten %>% .[grepl("abundance", names(.), ignore.case = TRUE)] %>% flatten_df #%>%
    # dplyr::mutate(sp_codes = fuzzySim::spCodes(species, nchar.gen = 2, nchar.sp = 2))
  
  spp_length.mass = community_objects %>% flatten %>% .[grepl("lengths", names(.), ignore.case = TRUE)] %>% flatten_df %>%
    # convert columns to numeric where appropriate
    dplyr::mutate(across(matches(')'), as.numeric))
  
  
  
  spp_length.mass %>%
   dplyr::filter(Species == "Anchoa mitchilli") #%>%
    ggplot(aes(x = 'TL (mm)'))+
    geom_density() +
    theme_minimal()
  

  
  
  
  

  }
