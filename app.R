library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)
library(shinycssloaders)

source("helpers.R")

ui <- function(request){
      fluidPage(
        conditionalPanel( # REGULAR UI
          condition = "output.show == null && output.show != 'map' && output.show != 'chart' && output.show != 'table'", 
          style = "display: none;", # Prevent flashing of the hidden UI elements when only showing the map/chart/table (somehow not currently working)
          
          navbarPage(title = a(href = "https://www.fao.org/fishery/en/collection/global_production?lang=en", target = "_blank", style = "text-decoration:none;color:inherit", div(img(src = "fao-logo-blue-3lines-en.svg", id = "logo", height = "35px", style = "border-right: 1px solid grey; padding: 0 0.5rem; position: relative; margin:-15px 0px; display:right-align; "), "FishStat Global Production")),
                     tabPanel("Data Explorer",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput('species_choice', 'Species classification', choices = c('Yearbook/SOFIA Selection', 'ISSCAAP Division', 'ISSCAAP Group')),
                                  conditionalPanel(
                                    condition = "input.species_choice == 'Yearbook/SOFIA Selection'",
                                    selectInput('yearbook_selection','Species group', choices = unique(data_yearbook$species_group), selected = "Aquatic animals", multiple = FALSE),
                                    # helpText("This classification allows you to filter species by three main groups. 'Fish, crustaceans and molluscs, etc.' refers to species fully destined to human consumption. 'Aquatic plants' refer to algae species. 'Other aq. animals & products' refers to marine mammals, crocodiles, corals, pearls, mother-of-pearl, shells, and sponges species not destined to human consumption.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.species_choice == 'ISSCAAP Division'",
                                    selectInput('isscaap_division','Species group', choices = unique(sort(data_division$species_group)), multiple = FALSE),
                                    helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.species_choice == 'ISSCAAP Group'",
                                    selectInput('isscaap_group','Species group', choices = unique(sort(data_group$species_group)), multiple = FALSE),
                                    helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
                                  ),
                                  conditionalPanel( # this hard-coded condition isn't the best solution: better to check if the dataset has more than one unit based on the other filters, then display conditional panel
                                    condition = "input.yearbook_selection == 'Other aq. animals & products' || input.isscaap_division == '7 - Miscellaneous aquatic animals'",
                                    selectInput('unit','Unit', choices = unique(data_yearbook$unit), multiple = FALSE)
                                  ),
                                  selectInput('year','Year', choices = sort(unique(data_yearbook$year), decreasing = TRUE), selected = max(unique(data_yearbook$year))),
                                  checkboxGroupInput('source','Production source', choices = c("Aquaculture production", "Capture production")),
                                  hr(),
                                  fullscreen_button("full_screen", label = "Fullscreen On/Off", icon = shiny::icon("expand", lib = "font-awesome"), target = NULL),
                                  br(),
                                  br(),
                                  bookmarkButton(label = "Share this view", icon = shiny::icon("share-alt", lib = "font-awesome")),
                                  width = 2
                                ),
                                mainPanel(
                                  tabsetPanel(
                                    tabPanel(
                                      "Map", 
                                      highchartOutput("countrymap", height = "850px") %>% withSpinner()
                                    ),
                                    tabPanel(
                                      "Chart", 
                                      highchartOutput("chart", height = "850px") %>% withSpinner()
                                    ),
                                    tabPanel(
                                      "Table", 
                                      DT::dataTableOutput("data_table", height = "850px") %>% withSpinner()
                                    ),
                                  )
                                )
                              ),
                              
                              hr(),
                              div(
                                class = "footer",
                                includeHTML("./footer.html")
                              ),
                              
                     ),
                     tabPanel("Instructions",
                              fluidPage(
                                mainPanel(
                                  h1("How to use this tool"),
                                  p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_production?lang=en", "Global Production", target="_blank"), "dataset."),
                                  p("We hope you enjoy this application. Click ", a(href="https://www.fao.org/fishery/en/fishstat", "here", target="_blank"), "to learn more about FAO's Fisheries and Aquaculture statistics."),
                                  p("We encourage users to provide their feedback or ask their questions about this tool at", a(href="mailto:Fish-Statistics-Inquiries@fao.org", "Fish-Statistics-Inquiries@fao.org", target="_blank", .noWS = c('after')), "."),
                                  h2("The Data Explorer"),
                                  p("Under", em("Data Explorer,"), "you can visualize the world's fisheries and aquaculture production by the selected species group and year of your choice."),
                                  h3("Side panel"),
                                  p("The", em("side panel"), "located on the left of the user interface allows you to select the species group to visualize on the right side of the interface. Three species classifications are available:" ),
                                  tags$ul(
                                    tags$li("The ", em("Yearbook/SOFIA Selection"), " classification includes the following three species groups: (i) ", em("Aquatic animals", .noWS = c('after')), ", which cover fish, crustaceans, molluscs and other aquatic animals, excluding aquatic mammals and crocodiles; (ii) ", em("Other aquatic animals and products", .noWS = c('after')), ", which include aquatic mammals, crocodiles and alligators, corals, pearls, shells and sponges; (iii) ", em("Algae", .noWS = c('after')), ", which consist of seaweeds."), 
                                    tags$li("The ", em("ISSCAAP Division"), " classification includes broad species groups that correspond to the one-digit groups of the ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "ISSCAAP classification", target="_blank", .noWS = c('after')), "."), 
                                    tags$li("The ", em("ISSCAAP Division"), " classification consists of more specific species groups that correspond to the two-digits groups of the ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "ISSCAAP classification", target="_blank", .noWS = c('after')), ".")
                                  ),
                                  p("Please note that a few species groups (e.g. Yearbook/SOFIA Selection's", em("Other aquatic animals and products", .noWS = c('after')), ") comprise both species that are accounted for in tonnes - live weight and species that are accounted for in number of specimen produced. In order to avoid the aggregation of production in different units, a", em("Unit") , "filter will be displayed when selecting these species groups."),
                                  p("The user can also use the filter in this panel to select the year of the data and the production source (aquaculture and/or capture). Finally, the two buttons on the bottom of the side panels allow the user to display the application in fullscreen and to share the current view with somebody else."), 
                                  tags$img(src = "side_panel.png"),
                                  h3("Map"),
                                  p("The data is first represented on a map under the", em("Map"), "tab. Each bubble represents a country or territory's capture and/or aquaculture production for the species group and year selected in the side panel. Placing your cursor on individual bubbles will give you more information on a given country or territory's production. You can zoom in or out of the map using the + and - buttons on the top left of the map. This is particularly useful to better explore data in areas of the world with a high density of countries or territories. Finally, you can export the map as an image by clicking on the three lines on the top right of the map."), 
                                  tags$img(src = "map_illustration.png"),
                                  h3("Chart"),
                                  p("If you want to focus on the main producers for the species group you selected, you can display them as a bar chart ordered by their shares of world production under the", em("Chart"), "tab. Placing your cursor on individual bars will give you more information on a given country or territory's production. You can export the visual as an image by clicking on the three lines on the top right of the chart."), 
                                  tags$img(src = "chart_illustration.png"),
                                  h3("Table"),
                                  p("Finally, you can display a table listing the producers of the species group of your choice in the", em("Table"), "tab. The data can be exported by clicking on any of the buttons on the top left of the table."),
                                  tags$img(src = "table_illustration.png")
                                  )
                                )
                              )
                     )
          ),
        conditionalPanel( # MAP ONLY
          condition = "output.show == 'map'", 
          highchartOutput("countrymap_solo", height = "700px") %>% withSpinner()
          ),
        conditionalPanel( # CHART ONLY
          condition = "output.show == 'chart'", 
          highchartOutput("chart_solo", height = "700px") %>% withSpinner()
          ),
        conditionalPanel( # TABLE ONLY
          condition = "output.show == 'table'", 
          DT::dataTableOutput("data_table_solo", height = "700px") %>% withSpinner()
          )
      )
}

server <- function(input, output, session) {
  
  ### REGULAR SERVER CODE
  
  # Make the choices for production source conditional on the other inputs
  
  observeEvent(list(input$species_choice, input$yearbook_selection, input$isscaap_division, input$isscaap_group, input$unit, input$year), ignoreInit = FALSE,{
    
    if (input$species_choice == 'Yearbook/SOFIA Selection') {req(input$yearbook_selection)}
    else if (input$species_choice == 'ISSCAAP Division') {req(input$isscaap_division)}
    else if (input$species_choice == 'ISSCAAP Group') {req(input$isscaap_group)}
    
    if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') {req(input$unit)}
    
    freezeReactiveValue(input, "source")
    updateCheckboxGroupInput(
      inputId = "source", 
      choices = 
        if (input$species_choice == 'Yearbook/SOFIA Selection' & input$yearbook_selection != 'Other aq. animals & products') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'Yearbook/SOFIA Selection' & input$yearbook_selection == 'Other aq. animals & products') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$unit == input$unit & data_yearbook$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Division') {sort(unique(data_division[data_division$species_group == input$isscaap_division & data_division$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Group') {sort(unique(data_group[data_group$species_group == input$isscaap_group & data_group$year == input$year,]$production_source_name))},
      selected = 
        if (input$species_choice == 'Yearbook/SOFIA Selection' & input$yearbook_selection != 'Other aq. animals & products') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'Yearbook/SOFIA Selection' & input$yearbook_selection == 'Other aq. animals & products') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$unit == input$unit & data_yearbook$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Division') {sort(unique(data_division[data_division$species_group == input$isscaap_division & data_division$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Group') {sort(unique(data_group[data_group$species_group == input$isscaap_group & data_group$year == input$year,]$production_source_name))}
    )
  })
  
  # Map of production by country
  
  data_map <- eventReactive(input$source, { # eventReactive is there to make sure the code doesn't execute before the production source input has updated (it is dependent on the other inputs)
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      filter(value > 0) %>%
      rename(z = value) %>%
      group_by(country, species_group, unit, year, lat, lon) %>%
      summarise(z = sum(z)) %>%
      group_by() %>%
      mutate(total = sum(z)) %>%
      ungroup() %>%
      mutate(value_formatted = addUnits(z)) %>%
      mutate(share = sprintf("%0.1f%%", z/total*100))
    
  })
  
  data_total <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      summarise(value = sum(value)) %>%
      pull() %>%
      addUnits()
    
  })
  
  data_capture_share <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      group_by(production_source_name) %>%
      summarise(value = sum(value)) %>%
      mutate(share = round(value/sum(value)*100, 0)) %>% 
      filter(production_source_name == "Capture production") %>%
      pull(share)
    
  })
  
  data_aquaculture_share <- eventReactive(input$source, {
    
    if (length(input$source) > 1) {
      
      switch(input$species_choice,
             'Yearbook/SOFIA Selection' = data_yearbook, 
             'ISSCAAP Division' = data_division, 
             'ISSCAAP Group' = data_group) %>%
        filter(year == input$year) %>%
        filter(production_source_name %in% input$source) %>%
        {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
          else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
          else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
        {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
          else .} %>%
        group_by(production_source_name) %>%
        summarise(value = sum(value)) %>%
        mutate(share = round(value/sum(value)*100, 0)) %>% 
        filter(production_source_name == "Aquaculture production") %>%
        pull(share)
    }
  })
  
  data_n <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      group_by(country, species_group, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      summarise(n = n()) %>%
      pull()
    
  })
  
  data_unit <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      summarise(unit = first(unit)) %>%
      pull()
    
  })
  
  title <- eventReactive(input$source, {
    if (input$species_choice == 'Yearbook/SOFIA Selection') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_yearbook[data_yearbook$species_group == input$yearbook_selection,]$species_group[[1]]), ", ", input$year)} 
    else if (input$species_choice == 'ISSCAAP Division') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_division[data_division$species_group == input$isscaap_division,]$isscaap_division_en[[1]]), ", ", input$year)} 
    else if (input$species_choice == 'ISSCAAP Group') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_group[data_group$species_group == input$isscaap_group,]$isscaap_group_en[[1]]), ", ", input$year)}
  })
  
  subtitle = eventReactive(input$source, {
    paste0('Total ', ifelse(length(input$source) > 1, "production", tolower(input$source)), " (", tolower(data_unit()) ,"): ", data_total(), ifelse(length(input$source) > 1, paste0(" (", data_capture_share(), "% capture, ", data_aquaculture_share(), "% aquaculture)"), ""), ", number of producing countries/areas: ", data_n())
  })
  
  source = paste0("Source: FAO ", format(Sys.Date(), "%Y"), ". Global Production. In: Fisheries and Aquaculture. Rome. [Cited ", format(Sys.time(), "%A, %B %d %Y"), "]. 
                                https://www.fao.org/fishery/en/collection/global_production?lang=en")
  
  countrymap <- renderHighchart(
    
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_map(), 
                    showInLegend = F,
                    type = "mapbubble", 
                    name = "Producing country or area",
                    color = ifelse(all(input$source == "Capture production"),  '#377eb8', 
                                   ifelse(all(input$source == "Aquaculture production"), '#4daf4a', 
                                          ifelse(length(input$source) > 1, '#984ea3', '#984ea3'))),
                    tooltip = list(pointFormat = "Country or area: {point.country}<br>Species group: {point.species_group}<br>Year: {point.year}<br>Production: {point.value_formatted}<br> Unit: {point.unit}<br>Share of world production: {point.share}")) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = subtitle()) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom") %>%
      hc_caption(text = source) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(backgroundColor = "#FFFFFF"),
                     legend = list(bubbleLegend = list(enabled = TRUE))
                   ),
                   sourceWidth = 1920,
                   sourceHeight = 1080,
                   filename = paste0("(Map) ", title())
      )
  )
  
  output$countrymap <- countrymap
  output$countrymap_solo <- countrymap # can't refer to the same output twice in the UI
  
  # Bar chart of production share by country
  
  data_chart <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      group_by(species_group, country, year) %>%
      summarise(value = sum(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      mutate(country = ifelse(share < 1, "Others", country)) %>%
      mutate(bottom = ifelse(country == "Others", 1, 0)) %>%
      group_by(country, species_group, year, bottom) %>%
      summarize(value = sum(value), share = sum(share)) %>%
      ungroup() %>%
      arrange(bottom, desc(share)) %>%
      mutate(value_formatted = addUnits(value), share_pretty = sprintf("%0.1f%%", share))
    
  }
  )
  
  chart <- renderHighchart({
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = country, y = share), 
           name = "Share of production",
           color = ifelse(all(input$source == "Capture production"),  '#377eb8', 
                          ifelse(all(input$source == "Aquaculture production"), '#4daf4a', 
                                 ifelse(length(input$source) > 1, '#984ea3', '#984ea3'))),
           tooltip = list(pointFormat = "Country or area: {point.country}<br>Species group: {point.species_group}<br>Year: {point.year}<br>Production (tonnes): {point.value_formatted}<br>Share: {point.share_pretty}")) %>%
      hc_xAxis(title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Share of world production"),
               labels = list(format = "{value}%"),
               ceiling = 100) %>%
      hc_title(text = title()) %>%
      hc_subtitle(text = subtitle()) %>%
      hc_caption(text = paste("Note: the 'Others' category groups all countries/areas with a share of world production lower than 1%.<br>", source)) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   ),
                   chartOptions = list(
                     chart = list(
                       backgroundColor = "#FFFFFF"
                     )
                   ),
                   # sourceWidth = 1920,
                   # sourceHeight = 1080,
                   filename = paste0("(Chart) ", title())
      )
  })
  
  output$chart <- chart
  output$chart_solo <- chart # can't refer to the same output twice in the UI
  
  # Table of country production
  
  data_table <- eventReactive(input$source, {
    
    switch(input$species_choice,
           'Yearbook/SOFIA Selection' = data_yearbook, 
           'ISSCAAP Division' = data_division, 
           'ISSCAAP Group' = data_group) %>%
      filter(year == input$year) %>%
      filter(production_source_name %in% input$source) %>%
      {if (input$species_choice == 'Yearbook/SOFIA Selection') filter(., species_group %in% input$yearbook_selection) 
        else if (input$species_choice == 'ISSCAAP Division') filter(., species_group %in% input$isscaap_division) 
        else if (input$species_choice == 'ISSCAAP Group') filter(., species_group %in% input$isscaap_group)} %>%
      {if (input$yearbook_selection == 'Other aq. animals & products' | input$isscaap_division == '7 - Miscellaneous aquatic animals') filter(., unit %in% input$unit)
        else .} %>%
      group_by(country, species_group, year, unit) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(country, species_group, year, value, unit, share) %>%
      add_row(country = source) # add citation in the last row
    
  }
  )
  
  interactive_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    datatable(data_table(),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = list('copy', list(
                  extend = 'collection',
                  buttons = list(
                    list(extend = 'csv', 
                         filename = paste0("(Table) ", title())),
                    list(extend = 'excel', 
                         filename = paste0("(Table) ", title())),
                    list(extend = 'pdf', 
                         filename = paste0("(Table) ", title()))),
                  text = 'Download'
                )),
                pageLength = 15, 
                lengthMenu = c(10,50,100)
              ),
              rownames = FALSE,
              class = "display",
              caption = title(), 
              colnames = c("Country or area", "Species group", "Year", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 0)
  })
  
  output$data_table <- interactive_table
  output$data_table_solo <- interactive_table # can't refer to the same output twice in the UI
  
  ### BOOKMARKING-RELATED CODE
  
  # Parse query string to identify which elements should be shown (either everything or just one of the interactive visuals)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['show']])) {
      output$show <- renderText({
        query[['show']]
      })
      outputOptions(output, "show", suspendWhenHidden = FALSE)
    }
  })
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$prodsource <- input$source
  })
  
  # Read values from state$values when we restore
  onRestored(function(state) {
    
    updateCheckboxGroupInput(
      inputId = "source",
      choices = 
        if (input$species_choice == 'Yearbook/SOFIA Selection') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Division') {sort(unique(data_division[data_division$species_group == input$isscaap_division & data_division$year == input$year,]$production_source_name))}
      else if (input$species_choice == 'ISSCAAP Group') {sort(unique(data_group[data_group$species_group == input$isscaap_group & data_group$year == input$year,]$production_source_name))},
      selected = state$values$prodsource
    )

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")