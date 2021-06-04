the_plan <-
  drake_plan(

  data_list = readRDS(file_in("./data/raw-data/data_lists.rds")),
  
  # fatty_acid_names = rename_fatty_acids(),
  
  ### Building metaweb structure and metadata ###
  
  foodweb_data = clean_foodweb_data(data_list),
  # 
  taxonomy_df = fill_taxonomy(foodweb_data[['spp_list']]),
  # 
  # trait_df = fill_traits(readRDS(file_in("./data/derived-data/spp_taxonomy_list.rds"))),
  #  
  # metaweb = create_metaweb(foodweb_data[["spp_nodes"]], foodweb_data[['agg_matrix']], readRDS(file_in("./data/derived-data/foodwebs/GoMexSI_df.rds"))),
  # #
  # ### Working with site-specific data  ###
  # 
  # saltmarsh_data = clean_marsh_data(data_list, taxonomy_df),#, trait_df, metaweb[['interaction_list']]),
  # 
  # pondweb = clean_pond_data(saltmarsh_data, taxonomy_df),
  # 
  # foodweb_plots = plot_foodwebs(foodweb_data),
  # 
  # saltmarsh_diversity = analyze_diversity(saltmarsh_data),
  # 
  # Saltmarsh_summary = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/Saltmarsh_overview.Rmd"))
  #     file_out("doc/Saltmarsh_overview.html")
  #   }
  # )
  
  

)
