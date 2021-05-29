##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_list
clean_foodweb_data <- function(data_list) {
  
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
                  "Fundulus xenica or Adinia xenica",
                  "Scentless plant bugs (true bugs)",
                  '"Algal mats"',
                  '"Filamentous algae"',
                  'Studies that reported "algae" in diet',
                  "Ulva",
                  '“Meiofauna”',
                  "Harpacticoid copepods",
                  "Nematodes",
                  "Ostracods",
                  '"Benthic diatoms"',
                  '"Epiphytes"',
                  '"Microalgae"',
                  '"Detritus"',
                  '"Meiofauna"',
                  '"Sediment"',
                  "Distichlis spicata",
                  "Juncus roemerianus",
                  '"plants" and any plant wetland plant species specified e.g., Iva frutescens',
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
                  "Wolf spider (Pirata marxii)",
                  "Chironomid larvae", 
                  "Thrips",
                  "Ischnodemus sp.",
                  "Moths",
                  "Other Spiders",
                  "Tanaids",
                  "Isopods",
                  "Weevil",
                  "Moth (Doryodes grandipennis)",
                  "Immature leafhoppers",
                  "Ants",
                  "Other Planthoppers",
                  "Cape Sable Seaside Sparrows (Ammonidrimus maritimus mirabilis)",
                  "Ant (Crematogaster clara)")
    
    good_names = list("Wedge clam (Rangia cuneata)",
                      "Thin strip hermit crab (Clibanarius vittatus)",
                      "Long wristed hermit crab (Pagurus longicarpus)",
                      "Gray hermit crab (Pagurus pollicaris)",
                      "Wharf crab (Armases cinereum)",
                      "Black fingered mud crab (Panopeus herbstii)",
                      "Diamond killifish (Adinia xenica)",
                      "Scentless plant true bugs (Rhopalidae)",
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
                      "Detritus",
                      "Meiofauna",
                      "Sediment",
                      "Seashore saltgrass (Distichlis spicata)",
                      "Black rush (Juncus roemerianus)",
                      "General plants",
                      "Saltmarsh cordgrass (Spartina alterniflora)",
                      "Salt hay (Spartina patens)",
                      "Capitellids (Capitellidae)",
                      "Nereids (Nereididae)",
                      "White shrimp (Litopenaeus setiferus)",
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
                      "Orb-weaver spider (Hypsosinga pygmaea)",
                      "Web-weaver spider (Grammonota trivittata)",
                      "Web-weaver spider (Linyphiidae)",
                      "Wolf spider (Lycosidae)",
                      "Wolf spider (Pirata sp.)",
                      "Chironomid larvae (Chironomidae)",
                      "Thrips (Thysanoptera)",
                      "Chinch bug (Ischnodemus sp.)",
                      "Moths (Lepidoptera)",
                      "Other Spiders (Araneae)",
                      "Tanaids (Tanaidacea)",
                      "Isopods (Isopoda)",
                      "Weevil (Curculionoidea)",
                      "Dull Doryodes Moth (Doryodes grandipennis)",
                      'Leafhopper (Cicadellidae) "immature"',
                      "Ant (Formicidae)",
                      "Planthopper (Fulgoromorpha)",
                      "Cape Sable Seaside Sparrows (Ammodramus maritimus mirabilis)",
                      "Ant (Crematogaster laeviuscula)")
    
    keyval = setNames(good_names, nm = unlist(bad_names))
    
    spp_nodes <- mccann_data[[3]][[1]] %>%
      # replace the damn curly quotes
      dplyr::mutate(taxa_or_description = textclean::replace_curly_quote(taxa_or_description)) %>%
      #rename taxa_or_description based on above keyvalue to ease below
      dplyr::mutate(taxa_or_description = recode(taxa_or_description, !!!keyval)) %>%
      # split taxa_or_description into generic and scientific
      dplyr::mutate(generic = case_when(grepl('(\\(.*(\\"|\\“.*)$)', taxa_or_description) ~ gsub('(.*[^(])\\(.*$', "\\1", taxa_or_description),#gsub('.*"(.*)".*', "\\1", taxa_or_description),#
                                        grepl("(\\()",taxa_or_description) ~ gsub("\\([^()]*\\)", "", taxa_or_description) %>% stringr::str_remove_all("\\s\\("),#stringr::str_extract_all(taxa_or_description, "^(.*?\\s\\()") %>% stringr::str_remove_all("\\s\\("),
                                        grepl('(\\")', taxa_or_description) ~ gsub('"|"', "", taxa_or_description),
                                        TRUE ~ taxa_or_description)) %>%
                                        # TRUE ~ NA_character_)) %>%#, c("generic","spp_name"), sep= "(", remove = FALSE) #%>%
      #clean spp_name
      dplyr::mutate(specific_name = case_when(grepl("(\\()", taxa_or_description) ~ gsub("\\(([^()]*)\\)|.", "\\1", taxa_or_description, perl=T),
                                         !grepl('(\\(|\\")', taxa_or_description) ~ NA_character_)) %>% #,
                                         # TRUE ~ NA_character_)) %>%
      dplyr::mutate(lifestage = case_when(grepl('^.{2,}\\"|^.{2,}\\“', taxa_or_description) ~ gsub('\\"([^()]*)\\"|.', "\\1", taxa_or_description, perl=T),
                                          TRUE ~ NA_character_))
  
    saveRDS(spp_nodes, "./data/derived-data/mccann_spp_taxonomy.rds")
} else{spp_nodes = readRDS(file = "./data/derived-data/mccann_spp_taxonomy.rds")}
    
   
   mccann_spp_list = spp_nodes %>% select(contains("specific")) %>% unlist %>%na.omit %>% unique
    
    # clean GoMexSI species interaction database
    # this data was downloaded manually via genera names from McCann data
    # In the future, automate this to update based on species lists
    # and from rglobi to extract 
    if(!file.exists("./data/derived-data/foodwebs/GoMexSI_df.rds")){
    GoMexSI_files = list.files("./data/raw-data/DATABASES/GoMexSI/", pattern = ".csv", full.names = TRUE)
    GoMexSI_list = lapply(GoMexSI_files, read.csv)
    GoMexSI_df = map(GoMexSI_list, ~.x %>%
                             select(-contains("collection"), -contains('tmp'), -altitude) %>%
                             dplyr::mutate(across(!matches("total|frequency"), as.character),
                                           across(matches("total|freqency", as.numeric)))) %>% 
      bind_rows
    
    saveRDS(GoMexSI_df, file = "./data/derived-data/foodwebs/GoMexSI_df.rds")
    } else{GoMexSI_df = readRDS(file = "./data/derived-data/foodwebs/GoMexSI_df.rds")}
    
    GoMexSI_spp_list = GoMexSI_df %>% select(matches('taxon_name')) %>% flatten %>% unlist %>% trimws %>% unique
    
    # get full spp list
    spp_list = c(mccann_spp_list, GoMexSI_spp_list) %>% unique

    return(list(agg_matrix= agg_matrix, feeding_links = feeding_links, spp_nodes = spp_nodes, spp_list = spp_list))

}
