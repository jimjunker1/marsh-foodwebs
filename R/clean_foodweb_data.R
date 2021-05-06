##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_list
clean_foodwebs <- function(data_list) {
  
  #!++++    HELPER FUNCTIONS    ++++!#
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
  
  fix_latlong = function(x){
    x_lat = sapply(x, function(x) gsub("N", c(""), x))
    x_latlong = sapply(x_lat, function(x) gsub("W","-",x))
    x_spec_trim = sapply(x_latlong, function(x) sub("º|°","",x))
    unname(x_spec_trim)
  }
  
  #!++++   END HELPER FUNCTION    ++++!#

    mccann_data = data_list[grepl("links|nodes|diet", unlist(names(data_list)), ignore.case = TRUE)]
    if(!file.exists("./data/derived-data/foodwebs/mccann_agg_mat.rds")){
    agg_matrix = mccann_data[[1]] %>% column_to_rownames('X')
    saveRDS(agg_matrix, "./data/derived-data/foodwebs/mccann_agg_mat.rds")
    } else{agg_matrix = readRDS(file = "./data/derived-data/foodwebs/mccann_agg_mat.rds")}
    feeding_links = mccann_data[[2]]
    
    if(!file.exists("./data/derived-data/mccann_spp_taxonomy.rds")){
    
    # some manual cleaning based on data inspection to make code below run more cleanly
    # specifically, it puts sci names within parentheses (i.e., "( )"),
    # lifestages within double quotes (i.e., " "), and adds a few sci names to generics
    # multiple common names or species names are separated by a slash (i.e., /)
    
    bad_names = c("Rangia cuneata",
                  "Clibanarius vittatus",
                  "Pagurus longicarpus",
                  "Pagurus pollicaris",
                  "Armases cinereum / Sesarma cinereum",
                  "Panopeus herbstii",
                  '“Detritus”',
                  '“Sediment”',
                  "Fundulus xenica or Adinia xenica",
                  "Scentless plant bugs (true bugs)",
                  '"Algal mats"',
                  '“Filamentous algae”',
                  "Studies that reported “algae” in diet",
                  "Ulva",
                  '“Meiofauna”',
                  "Harpacticoid copepods",
                  "Nematodes",
                  "Ostracods",
                  '“Benthic diatoms”',
                  '“Epiphytes”',
                  '“Microalgae”',
                  "Distichlis spicata",
                  "Juncus roemerianus",
                  "“plants” and any plant wetland plant species specified e.g., Iva frutescens",
                  "Spartina alterniflora",
                  "Spartina patens",
                  "Capitellids",
                  "Nereids",
                  "White shrimp (Penaues setiferus)/(Litopenaeus setiferus)",
                  "Littoraria irrorata",
                  "Jumping spider, Salticid spiders",
                  "Sac spider (Clubionid spiders)",
                  '"Submerged aquatic vegetation", "SAV"',
                  "Calanoid copepods",
                  "Leafhopper (Leafhopper sp.)",
                  "Silver perch (Bairdiella chrsoura)",
                  "Marsh fly (Haplodictya sp.)",
                  "Sailfin molly (Poecilia latapinna)",
                  "Yellow-crowned Night-Heron (Nycticorax iolaceus)",
                  "Sheepshead (Archosarus probatocephalus)",
                  "Katydid (Conocepahlus spartinae)",
                  "Leafhopper (Megamealus lobatus)",
                  "Leafhopper (Megamealus sp.)",
                  "Longlegged fly (Dolichopodidiae)",
                  "Picture-wingedfly (Chaetopsis aenia)",
                  "Wasp (Pentelicus sp.)",
                  "Marsh Rice Rat (Orysemys palustris)",
                  "Grass shrimp (Palaemonetes anternnarius)",
                  "White shrimp (Penaues setiferus / Litopenaeus setiferus)",
                  "Orb-weaver spider (Hyposinga variabilis)",
                  "Web-weaver spider (Grammonota trivitatta)",
                  "Web-weaver spider (Linyphiid spiders)",
                  "Wolf spider (Lycosid spiders)",
                  "Wolf spider (Pirata marxii)")
    
    good_names = list("Wedge clam (Rangia cuneata)",
                      "Thin strip hermit crab (Clibanarius vittatus)",
                      "Long wristed hermit crab (Pagurus longicarpus)",
                      "Gray hermit crab (Pagurus pollicaris)",
                      "Wharf crab (Armases cinereum)",
                      "Black fingered mud crab (Panopeus herbstii)",
                      "Detritus",
                      "Sediment",
                      "Diamond killifish (Adinia xenica)",
                      "Scentless plant true bugs",
                      "Algal mats",
                      "Filamentous algae",
                      "Algae",
                      "Ulva (Ulva sp.)",
                      "Meiofauna",
                      "Harpacticoid copepods (Harpacticoida)",
                      "Nematodes (Nematoda)",
                      "Ostracods (Ostracoda)",
                      "Benthic diatoms",
                      "Epiphytes",
                      "Microalgae",
                      "Seashore saltgrass (Distichlis spicata)",
                      "Black rush (Juncus roemerianus)",
                      "General plants",
                      "Saltmarsh cordgrass (Spartina alterniflora)",
                      "Salt hay (Spartina patens)",
                      "Capitellids (Capitellidae)",
                      "Nereids (Nereididae)",
                      "White shrimp (Penaues setiferus / Litopenaeus setiferus)",
                      "Marsh periwinkle (Littoraria irrorata)",
                      "Jumping spider (Salticidae)",
                      "Sac spider (Clubionidae)",
                      "Submerged aquatic vegetation",
                      "Calanoid copepods (Calanoida)",
                      "Leafhopper (Cicadellidae)",
                      "Silver perch (Bairdiella chrysoura)",
                      "Marsh fly (Hoplodictya sp.)",
                      "Sailfin molly (Poecilia latipinna)",
                      "Yellow-crowned Night-Heron (Nycticorax violaceus)",
                      "Sheepshead (Archosargus probatocephalus)",
                      "Katydid (Conocephalus spartinae)",
                      "Leafhopper (Megamelus lobatus)",
                      "Leafhopper (Megamelus sp.)",
                      "Longlegged fly (Dolichopodidae)",
                      "Picture-wingedfly (Chaetopsis aenea)",
                      "Wasp (Pelecinus sp.)",
                      "Marsh Rice Rat (Oryzomys palustris)",
                      "Grass shrimp (Palaemonetes antennarius)",
                      "White shrimp (Penaeus setiferus / Litopenaeus setiferus)",
                      "Orb-weaver spider (Hypsosinga variabilis)",
                      "Web-weaver spider (Grammonota trivittata)",
                      "Web-weaver spider (Linyphiidae)",
                      "Wolf spider (Lycosidae)",
                      "Wolf spider (Pirata sp.)")
    
    keyval = setNames(good_names, nm = unlist(bad_names))
    
    spp_nodes <- mccann_data[[3]][[1]] %>%
      #rename taxa_or_description based on above keyvalue to ease below
      dplyr::mutate(taxa_or_description = recode(taxa_or_description, !!!keyval)) %>%
      # split taxa_or_description into generic and scientific
      dplyr::mutate(generic = case_when(grepl('(\\(.*(\\"|\\“.*)$)', taxa_or_description) ~ gsub('(.*[^(])\\(.*$', "\\1", taxa_or_description),#gsub('.*"(.*)".*', "\\1", taxa_or_description),#
                                        grepl("(\\()",taxa_or_description) ~ gsub("\\([^()]*\\)", "", taxa_or_description) %>% stringr::str_remove_all("\\s\\("),#stringr::str_extract_all(taxa_or_description, "^(.*?\\s\\()") %>% stringr::str_remove_all("\\s\\("),
                                        grepl('(\\“)', taxa_or_description) ~ gsub('“|”', "", taxa_or_description),
                                        TRUE ~ taxa_or_description)) %>%
                                        # TRUE ~ NA_character_)) %>%#, c("generic","spp_name"), sep= "(", remove = FALSE) #%>%
      #clean spp_name
      dplyr::mutate(specific_name = case_when(grepl("(\\()", taxa_or_description) ~ gsub("\\(([^()]*)\\)|.", "\\1", taxa_or_description, perl=T),
                                         !grepl('(\\(|\\")', taxa_or_description) ~ NA_character_)) %>% #,
                                         # TRUE ~ NA_character_)) %>%
      dplyr::mutate(lifestage = case_when(grepl('\\"', taxa_or_description) ~ gsub('\\"([^()]*)\\"|.', "\\1", taxa_or_description, perl=T),
                                          TRUE ~ NA_character_))
  
    saveRDS(spp_nodes, "./data/derived-data/mccann_spp_taxonomy.rds")
} else{spp_nodes = readRDS(file = "./data/derived-data/mccann_spp_taxonomy.rds")}
    
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
    
    debugonce(square_matrix)
    full_matrix = mccann_expanded_feeding_links %>%
      dplyr::mutate(predator_filled = ifelse(is.na(predator_specific), predator_generic, predator_specific),
                    prey_filled = ifelse(is.na(prey_specific), ifelse(is.na(prey_generic), prey_group, prey_generic), prey_specific)) %>%
       select(predator_filled, prey_filled) %>%
      dplyr::mutate(pres = 1) %>% 
      square_matrix

    # clean GoMexSI species interaction database
    # this data was downloaded manually via genera names from McCann data
    # In the future, automate this to update based on species lists
    # and from rglobi to extract 
    
    GoMexSI_files = list.files("./data/raw-data/DATABASES/GoMexSI/", pattern = ".csv", full.names = TRUE)
    GoMexSI_list = lapply(GoMexSI_files, read.csv)
    GoMexSI_df = map(GoMexSI_list, ~.x %>%
                             select(-contains("collection"), -contains('tmp'), -altitude) %>%
                             dplyr::mutate(across(!matches("total|frequency"), as.character),
                                           across(matches("total|freqency", as.numeric)))) %>% 
      bind_rows
    
    saveRDS(GoMexSI_df, file = "./data/derived-data/foodwebs/GoMexSI_df.rds")
    
    # get full spp list
    spp_list = GoMexSI_df %>% select(matches('taxon_name')) %>% flatten %>% unlist %>% unique
    

    return(list(agg_matrix= agg_matrix, feeding_links = feeding_links, spp_nodes = spp_nodes, spp_list = spp_list ))

}
