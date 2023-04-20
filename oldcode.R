
  
  # Map of fishing areas by commodity production
  
  # data_fa_map <- reactive(
  #   prod_raw_ISSCAAP %>%
  #     filter(conc_isscaap_group == input$species_country,
  #            year == input$year_country,
  #            production_source_name %in% input$source_country) %>%
  #     mutate(conc_fishing_area = paste(fishing_area_code, "-", fishing_area_name), 
  #            fishing_area_code = as.integer(fishing_area_code)) %>%
  #     group_by(conc_isscaap_group, fishing_area_code, conc_fishing_area, year) %>%
  #     summarise(value = sum(value)) %>%
  #     group_by() %>%
  #     mutate(total = sum(value)) %>%
  #     ungroup() %>%
  #     mutate(value_formatted = addUnits(value)) %>%
  #     mutate(share = sprintf("%0.1f%%", value/total*100))
  #   )
  # 
  # output$famap <- renderHighchart(
  #   highchart() %>%
  #     hc_title(text = "Production by FAO fishing area") %>%
  #     hc_subtitle(text = "In tonnes - live weight") %>%
  #     hc_add_series_map(
  #       map = fishing_areas,
  #       df = data_fa_map(), 
  #       joinBy = c("F_CODE","fishing_area_code"),
  #       name = "Production by FAO fishing area",
  #       value = "value",
  #       tooltip = list(pointFormat = "FAO Fishing Area: {point.conc_fishing_area}<br>Production (tonnes): {point.value_formatted}<br>Year: {point.year}") %>%
  #         hc_caption(text = "Data only includes aquatic animals.")
  #     )
  # )

  