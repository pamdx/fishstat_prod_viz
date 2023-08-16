library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)
library(shinycssloaders)

# library(rsconnect)
# deployApp()

source("helpers.R")

ui <- function(request){navbarPage("FishStat Production Data",
        tabPanel("Data Explorer",
          sidebarLayout(
            sidebarPanel(
              selectInput('species_choice', 'Species classification', choices = c('Yearbook/SOFIA Selection', 'ISSCAAP Division', 'ISSCAAP Group')),
              conditionalPanel(
                condition = "input.species_choice == 'Yearbook/SOFIA Selection'",
                uiOutput('yearbook_selection'),
                # helpText("This classification allows you to filter species by three main groups. 'Fish, crustaceans and molluscs, etc.' refers to species fully destined to human consumption. 'Aquatic plants' refer to algae species. 'Other aq. animals & products' refers to marine mammals, crocodiles, corals, pearls, mother-of-pearl, shells, and sponges species not destined to human consumption.")
              ),
              conditionalPanel(
                condition = "input.species_choice == 'ISSCAAP Division'",
                uiOutput('isscaap_division'),
                helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
              ),
              conditionalPanel(
                condition = "input.species_choice == 'ISSCAAP Group'",
                uiOutput('isscaap_group'),
                helpText("Click ", a(href="https://www.fao.org/fishery/static/ASFIS/ISSCAAP.pdf", "here", target="_blank"), " for more information about the ISSCAAP classification.")
              ),
              selectInput('year','Year', choices = sort(unique(data_yearbook$year), decreasing = TRUE), selected = max(unique(data_yearbook$year))),
              checkboxGroupInput('source','Production source', choices = c("Aquaculture production", "Capture production")),
              hr(),
              fullscreen_button("full_screen", label = "Fullscreen On/Off", icon = shiny::icon("expand", lib = "font-awesome"), target = NULL),
              br(),
              br(),
              bookmarkButton(label = "Share this view", icon = shiny::icon("share-alt", lib = "font-awesome")),
              br(),
              br(),
              img(src="fao-logo-en.svg", width = "100%"),
              width = 2
            ),
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Map", 
                  highchartOutput("countrymap", height = "700px") %>% withSpinner()
                  ),
                tabPanel(
                  "Chart", 
                  highchartOutput("chart", height = "700px") %>% withSpinner()
                  ),
                tabPanel(
                  "Table", 
                  DT::dataTableOutput("data_table", height = "700px") %>% withSpinner()
                  ),
              )
            )
          )
        ),
        tabPanel("Readme",
                 fluidPage(
                   mainPanel(
                     h1("How to use this tool"),
                     p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_production?lang=en", "Global Production", target="_blank"), "dataset."),
                     p("We hope you enjoy this application. Click ", a(href="https://www.fao.org/fishery/en/fishstat", "here", target="_blank"), "to learn more about FAO's Fisheries and Aquaculture statistics."),
                     p("We encourage users to provide their feedback or ask their questions about this tool at", a(href="mailto:Fish-Statistics-Inquiries@fao.org", "Fish-Statistics-Inquiries@fao.org", target="_blank")),
                     h2("The Data Explorer"),
                     p("Under", em("Data Explorer,"), "you can visualize the world's fisheries and aquaculture production by the species group and for the year of your choice."),
                     h3("Side panel"),
                     p("The", em("side panel"), "located on the left of the user interface allows you to select the species group to visualize on the right side of the interface (three species classifications are available). The user can also use the filter in this panel to select the year of the data and the production source (aquaculture and/or capture). Finally, the two buttons on the bottom of the side panels allow the user to display the application in fullscreen and to share the current view with somebody else."), 
                     tags$img(src = "side_panel.png"),
                     h3("Map"),
                     p("The data is first represented on a map under the", em("Map"), "tab. Each bubble represents a country's capture and/or aquaculture production for the species group and year selected in the side panel. Placing your cursor on individual bubbles will give you more information on a given country's production. You can zoom in or out of the map using the + and - buttons on the top left of the map. This is particularly useful to better explore data in areas of the world with a high density of countries. Finally, you can export the map as an image by clicking on the three lines on the top right of the map."), 
                     tags$img(src = "map_illustration.png"),
                     h3("Chart"),
                     p("If you want to focus on the main producers for the species group you selected, you can display them as a bar chart ordered by their shares of world production under the", em("Chart"), "tab. Placing your cursor on individual bars will give you more information on a given country's production. You can export the visual as an image by clicking on the three lines on the top right of the chart."), 
                     tags$img(src = "chart_illustration.png"),
                     h3("Table"),
                     p("Finally, you can display a table listing the producers of the species group of your choice in the", em("Table"), "tab. The data can be exported by clicking on any of the buttons on the top left of the table."),
                     tags$img(src = "table_illustration.png")
                   )
                 )
        )
      )
}
server <- function(input, output, session) {
  
  # Initialize conditional species filters
  
  output$yearbook_selection <- renderUI({
    selectInput('yearbook_selection','Species group', choices = unique(data_yearbook$species_group), selected = "Aquatic animals", multiple = FALSE)
  })
  
  output$isscaap_division <- renderUI({
    selectInput('isscaap_division','Species group', choices = unique(sort(data_division$species_group)), multiple = FALSE)
  })
  
  output$isscaap_group <- renderUI({
    selectInput('isscaap_group','Species group', choices = unique(sort(data_group$species_group)), multiple = FALSE)
  })
  
  # Make the choices for production source conditional on the other inputs
  
  observeEvent(list(input$species_choice, input$yearbook_selection, input$isscaap_division, input$isscaap_group, input$year), ignoreInit = T,{
    
    if (input$species_choice == 'Yearbook/SOFIA Selection') {req(input$yearbook_selection)}
    else if (input$species_choice == 'ISSCAAP Division') {req(input$isscaap_division)}
    else if (input$species_choice == 'ISSCAAP Group') {req(input$isscaap_group)}
    
    freezeReactiveValue(input, "source")
    updateCheckboxGroupInput(
      inputId = "source", 
      choices = 
        if (input$species_choice == 'Yearbook/SOFIA Selection') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$year == input$year,]$production_source_name))}
          else if (input$species_choice == 'ISSCAAP Division') {sort(unique(data_division[data_division$species_group == input$isscaap_division & data_division$year == input$year,]$production_source_name))}
          else if (input$species_choice == 'ISSCAAP Group') {sort(unique(data_group[data_group$species_group == input$isscaap_group & data_group$year == input$year,]$production_source_name))},
      selected = 
        if (input$species_choice == 'Yearbook/SOFIA Selection') {sort(unique(data_yearbook[data_yearbook$species_group == input$yearbook_selection & data_yearbook$year == input$year,]$production_source_name))}
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
      rename(z = value) %>%
      group_by(country, species_group, unit, year, lat, lon) %>%
      summarise(z = sum(z)) %>%
      group_by() %>%
      mutate(total = sum(z)) %>%
      ungroup() %>%
      mutate(value_formatted = addUnits(z)) %>%
      mutate(share = sprintf("%0.1f%%", z/total*100))
    
    }
  )
  
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
      summarise(value = sum(value)) %>%
      pull() %>%
      addUnits()
    
    }
  )
  
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
      group_by(production_source_name) %>%
      summarise(value = sum(value)) %>%
      mutate(share = round(value/sum(value)*100, 0)) %>% 
      filter(production_source_name == "Capture production") %>%
      pull(share)

    }
  )
  
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
        group_by(production_source_name) %>%
        summarise(value = sum(value)) %>%
        mutate(share = round(value/sum(value)*100, 0)) %>% 
        filter(production_source_name == "Aquaculture production") %>%
        pull(share)
      }
    }
  )
  
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
      group_by(country, species_group, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      summarise(n = n()) %>%
      pull()
    
    }
  )
  
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
      summarise(unit = first(unit)) %>%
      pull()

    }
  )
  
  output$countrymap <- renderHighchart(
    
    # withProgress(message = 'Loading. Please wait.', value = 0, {
    
      highchart(type = "map") %>%
        hc_add_series(mapData = map, showInLegend = F) %>%
        hc_add_series(data = data_map(), 
                      showInLegend = F,
                      type = "mapbubble", 
                      name = "Producing country",
                      color = ifelse(all(input$source == "Capture production"),  '#377eb8', 
                                     ifelse(all(input$source == "Aquaculture production"), '#4daf4a', 
                                            ifelse(length(input$source) > 1, '#984ea3', '#984ea3'))),
                      tooltip = list(pointFormat = "Country: {point.country}<br>Species group: {point.species_group}<br>Year: {point.year}<br>Production: {point.value_formatted}<br> Unit: {point.unit}<br>Share of world production: {point.share}")) %>%
        hc_title(text = 
                   if (input$species_choice == 'Yearbook/SOFIA Selection') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_yearbook[data_yearbook$species_group == input$yearbook_selection,]$species_group[[1]]), ", ", input$year)} 
                 else if (input$species_choice == 'ISSCAAP Division') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_division[data_division$species_group == input$isscaap_division,]$isscaap_division_en[[1]]), ", ", input$year)} 
                 else if (input$species_choice == 'ISSCAAP Group') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_group[data_group$species_group == input$isscaap_group,]$isscaap_group_en[[1]]), ", ", input$year)}
                 ) %>%
        hc_subtitle(text = paste0('Total ', ifelse(length(input$source) > 1, "production", tolower(input$source)), " (", tolower(data_unit()) ,"): ", data_total(), ifelse(length(input$source) > 1, paste0(" (", data_capture_share(), "% capture, ", data_aquaculture_share(), "% aquaculture)"), ""), ", number of producing countries: ", data_n())) %>%
        hc_mapNavigation(enabled = T) %>%
        hc_legend(enabled = TRUE, 
                  layout = "horizontal", 
                  align = "right",
                  verticalAlign = "bottom") %>%
        hc_exporting(enabled = TRUE, 
                     buttons = list(
                       contextButton = list(
                         menuItems = hc_export_options
            )
          )
        )
      # })
    )
  
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
  
  output$chart <- renderHighchart({
    hchart(data_chart(), 
           type = "column", 
           hcaes(x = country, y = share), 
           name = "Share of production",
           color = ifelse(all(input$source == "Capture production"),  '#377eb8', 
                          ifelse(all(input$source == "Aquaculture production"), '#4daf4a', 
                                 ifelse(length(input$source) > 1, '#984ea3', '#984ea3'))),
           tooltip = list(pointFormat = "Country: {point.country}<br>Species group: {point.species_group}<br>Year: {point.year}<br>Production (tonnes): {point.value_formatted}<br>Share: {point.share_pretty}")) %>%
      hc_xAxis(title = list(text = "Country")) %>%
      hc_yAxis(title = list(text = "Share"),
               labels = list(format = "{value}%")) %>%
      hc_title(text = 
                 if (input$species_choice == 'Yearbook/SOFIA Selection') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_yearbook[data_yearbook$species_group == input$yearbook_selection,]$species_group[[1]]), ", ", input$year)} 
               else if (input$species_choice == 'ISSCAAP Division') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_division[data_division$species_group == input$isscaap_division,]$isscaap_division_en[[1]]), ", ", input$year)} 
               else if (input$species_choice == 'ISSCAAP Group') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_group[data_group$species_group == input$isscaap_group,]$isscaap_group_en[[1]]), ", ", input$year)}
      ) %>%
      hc_subtitle(text = paste0('Total ', ifelse(length(input$source) > 1, "capture and aquaculture production", tolower(input$source)), " (", tolower(data_unit()) ,"): ", data_total(), ", number of producing countries: ", data_n())) %>%
      hc_caption(text = "Note: the 'Others' category groups all countries with a share of world production lower than 1%.") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                     )
                   )
      )
  })
  
  # Table of country production by commodity
  
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
      group_by(country, species_group, year, unit) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(country, species_group, year, value, unit, share)
    
    }
  )
  
  output$data_table <- DT::renderDataTable(server = FALSE, { # server = FALSE used to make sure the entire dataset is downloaded when using the buttons
    datatable(data_table(),
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf'),
                pageLength = 15, 
                lengthMenu = c(10,50,100)
              ),
              class = "display",
              caption = 
                if (input$species_choice == 'Yearbook/SOFIA Selection') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_yearbook[data_yearbook$species_group == input$yearbook_selection,]$species_group[[1]]), ", ", input$year)} 
              else if (input$species_choice == 'ISSCAAP Division') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_division[data_division$species_group == input$isscaap_division,]$isscaap_division_en[[1]]), ", ", input$year)} 
              else if (input$species_choice == 'ISSCAAP Group') {paste0(ifelse(length(input$source) > 1, "Capture and aquaculture production", input$source), " of ", tolower(data_group[data_group$species_group == input$isscaap_group,]$isscaap_group_en[[1]]), ", ", input$year)}, 
              colnames = c("Country", "ISSCAAP group", "Year", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 0)
  })
 
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$prodsource <- input$source
  })
  
  # Read values from state$values when we restore
  onRestored(function(state) {
    
    updateCheckboxGroupInput(inputId = "source", choices = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$species_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year),]$production_source_name),
                             selected = state$values$prodsource
    )
    
    # input$source <- state$values$prodsource
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")