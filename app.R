library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)
library(shinyfullscreen)

# library(rsconnect)
# deployApp()

source("helpers.R")

ui <- function(request){navbarPage("FishStat Production Data",
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")), # Define fullscreen background color using CSS to prevent black background when using fullscreen button
        tabPanel("Data explorer",
          sidebarLayout(
            sidebarPanel(
              selectInput('species_country','ISSCAAP group', choices = c("Please select...", sort(unique(prod_raw_ISSCAAP$conc_isscaap_group)))),
              selectInput('year_country','Year', choices = sort(unique(prod_raw_ISSCAAP$year), decreasing = TRUE), selected = max(unique(prod_raw_ISSCAAP$year))),
              checkboxGroupInput('source_country','Production source', choices = c("Aquaculture production", "Capture production")),
              hr(),
              bookmarkButton(label = "Share this view", icon = shiny::icon("share-alt", lib = "font-awesome")),
              br(),
              br(),
              img(src="https://www.fao.org/images/corporatelibraries/fao-logo/fao-logo-en.svg?sfvrsn=f64522b4_33", width = "100%"),
              width = 2
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Map", highchartOutput("countrymap", height = "700px")),
                tabPanel("Chart", highchartOutput("chart", height = "700px")),
                tabPanel("Table", DT::dataTableOutput("data_table", height = "700px")),
              )
            )
          )
        ),
       fixedPanel(
         fullscreen_button("full_screen", label = "", icon = shiny::icon("expand", lib = "font-awesome"), target = NULL),
         right = 326,
         top = 79
       )
      )
}
server <- function(input, output, session) {
  
  observeEvent(c(input$species_country, input$year_country), ignoreInit = T,{ # Make the choices for production source conditional on the other inputs
    freezeReactiveValue(input, "source_country")
    updateCheckboxGroupInput(inputId = "source_country", choices = sort(unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name))
                             , selected = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name)
    )
  })
  
  # Map of country production by commodity
  
  data_country_map <- eventReactive(input$source_country, { # eventReactive is there to make sure the code doesn't execute before the production source input has updated (it is dependent on the other inputs)
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
           year == input$year_country,
           production_source_name %in% input$source_country) %>%
      rename(z = value) %>%
      group_by(conc_isscaap_group, iso2_code, country, year, unit, lat, lon) %>%
      summarise(z = sum(z)) %>%
      group_by() %>%
      mutate(total = sum(z)) %>%
      ungroup() %>%
      mutate(value_formatted = addUnits(z)) %>%
      mutate(share = sprintf("%0.1f%%", z/total*100))
    }
  )
  
  data_total <- eventReactive(input$source_country, {
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
             year == input$year_country,
             production_source_name %in% input$source_country) %>%
      summarise(value = sum(value)) %>%
      pull() %>%
      addUnits()}
  )
  
  data_capture_share <- eventReactive(input$source_country, {
      if (length(input$source_country) > 1) {
        prod_raw_ISSCAAP %>%
          filter(conc_isscaap_group == input$species_country,
                 year == input$year_country,
                 production_source_name %in% input$source_country) %>%
          group_by(production_source_name) %>%
          summarise(value = sum(value)) %>%
          mutate(share = round(value/sum(value)*100, 0)) %>% 
          filter(production_source_name == "Capture production") %>%
          pull(share)
      }
    }
  )
  
  data_aquaculture_share <- eventReactive(input$source_country, {
    if (length(input$source_country) > 1) {
      prod_raw_ISSCAAP %>%
        filter(conc_isscaap_group == input$species_country,
               year == input$year_country,
               production_source_name %in% input$source_country) %>%
        group_by(production_source_name) %>%
        summarise(value = sum(value)) %>%
        mutate(share = round(value/sum(value)*100, 0)) %>% 
        filter(production_source_name == "Aquaculture production") %>%
        pull(share)
    }
  }
  )
  
  data_n <- eventReactive(input$source_country, {
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
             year == input$year_country,
             production_source_name %in% input$source_country) %>%
      group_by(conc_isscaap_group, iso2_code, country, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      summarise(n = n()) %>%
      pull()}
  )
  
  data_unit <- eventReactive(input$source_country, {
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
             year == input$year_country,
             production_source_name %in% input$source_country) %>%
      summarise(unit = first(unit)) %>%
      pull()}
  )
  
  output$countrymap <- renderHighchart(
    highchart(type = "map") %>%
      hc_add_series(mapData = map, showInLegend = F) %>%
      hc_add_series(data = data_country_map(), 
                    showInLegend = F,
                    type = "mapbubble", 
                    name = "Producing country",
                    color = ifelse(all(input$source_country == "Capture production"),  '#377eb8', 
                                   ifelse(all(input$source_country == "Aquaculture production"), '#4daf4a', 
                                          ifelse(length(input$source_country) > 1, '#984ea3', '#984ea3'))),
                    tooltip = list(pointFormat = "Country: {point.country}<br>ISSCAAP group: {point.conc_isscaap_group}<br>Year: {point.year}<br>Production: {point.value_formatted}<br> Unit: {point.unit}<br>Share of world production: {point.share}")) %>%
      hc_title(text = paste0(ifelse(length(input$source_country) > 1, "Capture and aquaculture production", input$source_country), " of ", tolower(prod_raw_ISSCAAP[prod_raw_ISSCAAP$conc_isscaap_group == input$species_country,]$isscaap_group_en[[1]]), ", ", input$year_country)) %>%
      hc_subtitle(text = paste0('Total ', ifelse(length(input$source_country) > 1, "production", tolower(input$source_country)), " (", tolower(data_unit()) ,"): ", data_total(), ifelse(length(input$source_country) > 1, paste0(" (", data_capture_share(), "% capture, ", data_aquaculture_share(), "% aquaculture)"), ""), ", number of producing countries: ", data_n())) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom") %>%
      # hc_caption(text = "<center>Note: the data presented only includes aquatic animals.</center>") %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(
                     contextButton = list(
                       menuItems = hc_export_options
                       )
                     )
                   )
    )
  
  # Bar chart of production share by country
  
  data_chart <- eventReactive(input$source_country, {
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
             year == input$year_country,
             production_source_name %in% input$source_country) %>%
      group_by(conc_isscaap_group, iso2_code, country, year) %>%
      summarise(value = sum(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      mutate(country = ifelse(share < 1, "Others", country)) %>%
      mutate(bottom = ifelse(country == "Others", 1, 0)) %>%
      group_by(country, conc_isscaap_group, year, bottom) %>%
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
           color = ifelse(all(input$source_country == "Capture production"),  '#377eb8', 
                          ifelse(all(input$source_country == "Aquaculture production"), '#4daf4a', 
                                 ifelse(length(input$source_country) > 1, '#984ea3', '#984ea3'))),
           tooltip = list(pointFormat = "Country: {point.country}<br>ISSCAAP group: {point.conc_isscaap_group}<br>Year: {point.year}<br>Production (tonnes): {point.value_formatted}<br>Share: {point.share_pretty}")) %>%
      hc_xAxis(title = list(text = "Country")) %>%
      hc_yAxis(title = list(text = "Share"),
               labels = list(format = "{value}%")) %>%
      hc_title(text = paste0("Share of world ", ifelse(length(input$source_country) > 1, "capture and aquaculture production", tolower(input$source_country)), ", ", tolower(prod_raw_ISSCAAP[prod_raw_ISSCAAP$conc_isscaap_group == input$species_country,]$isscaap_group_en[[1]]), ", ", input$year_country)) %>%
      hc_subtitle(text = paste0('Total ', ifelse(length(input$source_country) > 1, "capture and aquaculture production", tolower(input$source_country)), " (", tolower(data_unit()) ,"): ", data_total(), ", number of producing countries: ", data_n())) %>%
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
  
  data_table <- eventReactive(input$source_country, {
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
             year == input$year_country,
             production_source_name %in% input$source_country) %>%
      group_by(country, conc_isscaap_group, year, unit) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      arrange(desc(value)) %>%
      group_by() %>%
      mutate(total = sum(value)) %>%
      ungroup() %>%
      mutate(share = value/total*100) %>%
      select(country, conc_isscaap_group, year, value, unit, share)
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
                pageLength = 10, 
                lengthMenu = c(10,50,100)
              ),
              class = "display",
              caption = paste0(ifelse(length(input$source_country) > 1, "Capture and aquaculture production", input$source_country), " of ", tolower(prod_raw_ISSCAAP[prod_raw_ISSCAAP$conc_isscaap_group == input$species_country,]$isscaap_group_en[[1]]), ", ", input$year_country), 
              colnames = c("Country", "ISSCAAP group", "Year", "Value", "Unit", "Share (%)")) %>%
      formatRound(c("share"), 1) %>%
      formatCurrency("value", currency = "", interval = 3, mark = " ", digits = 0)
  })
 
  # Save app state to URL
  
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(updateQueryString)
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$prodsource <- input$source_country
  })
  
  # Read values from state$values when we restore
  onRestored(function(state) {
    
    updateCheckboxGroupInput(inputId = "source_country", choices = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name),
                             selected = state$values$prodsource
    )
    
    # input$source_country <- state$values$prodsource
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")