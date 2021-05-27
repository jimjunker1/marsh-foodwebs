the_plan <-
  drake_plan(

  import_data(data_files),
  
  data_list = readRDS(file_in("./data/raw-data/data_lists.rds")),
  
  # fatty_acid_names = rename_fatty_acids(),
  
  ### Building metaweb structure and metadata ###
  
  foodweb_data = clean_foodweb_data(data_list),
  
  taxonomy_df = fill_taxonomy(foodweb_data),
  
  metaweb = plot_metaweb(readRDS(file_in("./data/derived-data/foodwebs/mccann_agg_mat.rds"))),
  
  trait_df = fill_traits(readRDS(file_in("./data/derived-data/spp_taxonomy_list.rds"))),
  
  
  
  ### Working with site-specific data  ###
  
  saltmarsh_data = clean_marsh_data(data_list, trait_df),
  
  pondweb = clean_pond_data(saltmarsh_data, taxonomy_df),
  
  foodweb_plots = plot_foodwebs(foodweb_data),
  
  saltmarsh_diversity = analyze_diversity(saltmarsh_data),
  
  Saltmarsh_summary = target(
    command = {
      rmarkdown::render(knitr_in("doc/Saltmarsh_overview.Rmd"))
      file_out("doc/Saltmarsh_overview.html")
    }
  )
  
  

)
