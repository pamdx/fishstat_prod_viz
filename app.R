library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(DT)

# library(rsconnect)
# deployApp()

source("helpers.R")

ui <- navbarPage("FishStat Production Data",
       #  tabPanel("Home",
       #   fluidPage(
       #      mainPanel(
       #        h1("Welcome"),
       #        p("This website features interactive visualizations to explore FAO's", a(href="https://www.fao.org/fishery/en/collection/global_production?lang=en", "Global Production", target="_blank"), "dataset."),
       #        p("In the", strong("Global Overview"), "section, you will find a visualization of the intercontinental trade of fish products. Go ahead and use the", em("Country highlight"), "filter to see how your country trades with the rest of the world. The", em("Heatmap"), "tab displays this information as a heatmap."),
       #        p("In the", strong("Country Overview"), "section, you can explore in details how the country of your choice trades with its partner countries around the world. The data is represented on a map, but you can also see more details about the data by clicking on the", em("Table"), "tab. Finally, see a chart of the selected country's main trading partners in the", em("Chart"), "tab."),
       #        p("We hope you enjoy this website. Click ", a(href="https://www.fao.org/fishery/en/statistics", "here", target="_blank"), "if you want to learn more about FAO's Fisheries and Aquaculture statistics."),
       #        h2("Notes"),
       #        p("Differences between figures given for total exports and total imports of any one commodity may be due to several factors, e.g. the time lapse between the dispatch of goods from the exporting country and their arrival in the importing country; the use of a different classification of the same product by different countries; or the fact that some countries supply trade data on general trade, while others give data on special trade."),
       #        p("Created by", a(href='https://pamdx.github.io/personal_site/index.html', 'Pierre Maudoux', target='_blank'), "(FAO). See the", a(href='https://github.com/pamdx/fishstat_trade_viz', 'source code', target='_blank'), "on GitHub.")
       #      )
       #   )
       # ),
        tabPanel("Data explorer",
          sidebarLayout(
            sidebarPanel(
              # selectInput('country','Country', choices = unique(prod_raw_ISSCAAP$country), selected = sample(unique(prod_raw_ISSCAAP$country), 1)),
              selectInput('species_country','ISSCAAP group', choices = sort(unique(prod_raw_ISSCAAP$conc_isscaap_group)), selected = sample(unique(prod_raw_ISSCAAP$conc_isscaap_group), 1)), 
              selectInput('year_country','Year', choices = sort(unique(prod_raw_ISSCAAP$year), decreasing = TRUE), selected = max(unique(prod_raw_ISSCAAP$year))),
              checkboxGroupInput('source_country','Production source', choices = c("Capture production", "Aquaculture production")),
              width=2
            ),
            mainPanel(
              tabsetPanel(
                # tabPanel("By FAO fishing area", highchartOutput('famap', height = "700px")),
                tabPanel("By Country", highchartOutput("countrymap", height = "700px")),
                tabPanel("Chart", highchartOutput("chart", height = "700px")),
                # tabPanel("Over time", highchartOutput("linechart", height = "700px")),
                # tabPanel("By environment", highchartOutput("environmentchart", height = "700px")),
                tabPanel("Table", DT::dataTableOutput("data_table", height = "700px")),
              )
            )
          )
        )
      )

server <- function(input, output, session) {
  
  observeEvent(input$species_country, { # Make the choices for production source conditional on the other inputs
    freezeReactiveValue(input, "source_country")
    updateCheckboxGroupInput(inputId = "source_country", choices = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name), selected = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name))
  })  
  observeEvent(input$year_country, {
    freezeReactiveValue(input, "source_country")
    updateCheckboxGroupInput(inputId = "source_country", choices = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name), selected = unique(prod_raw_ISSCAAP[(prod_raw_ISSCAAP$conc_isscaap_group == input$species_country) & (prod_raw_ISSCAAP$year == input$year_country),]$production_source_name))
  })
  
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
  
  # Map of country production by commodity
  
  data_country_map <- eventReactive(input$source_country, { # eventReactive is there to make sure the code doesn't execute before the production source input has updated (it is dependent on the other inputs)
    prod_raw_ISSCAAP %>%
      filter(conc_isscaap_group == input$species_country,
           year == input$year_country,
           production_source_name %in% input$source_country) %>%
      rename(z = value) %>%
      group_by(conc_isscaap_group, iso2_code, country, year, lat, lon) %>%
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
                    tooltip = list(pointFormat = "Country: {point.country}<br>ISSCAAP group: {point.conc_isscaap_group}<br>Year: {point.year}<br>Production (tonnes): {point.value_formatted}<br>Share: {point.share}")) %>%
      hc_title(text = paste0(ifelse(length(input$source_country) > 1, "Capture and aquaculture production", input$source_country), " of ", tolower(prod_raw_ISSCAAP[prod_raw_ISSCAAP$conc_isscaap_group == input$species_country,]$isscaap_group_en[[1]]), ", ", input$year_country)) %>%
      hc_subtitle(text = paste0('Total ', ifelse(length(input$source_country) > 1, "capture and aquaculture production", tolower(input$source_country)), " (tonnes): ", data_total(), ", number of producing countries: ", data_n())) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_legend(enabled = TRUE, 
                layout = "horizontal", 
                align = "right",
                verticalAlign = "bottom") %>%
      hc_caption(text = "<center>Note: the data presented only includes aquatic animals.</center>") %>%
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
      hc_title(text = paste0("Share of ", ifelse(length(input$source_country) > 1, "capture and aquaculture production", tolower(input$source_country)), ", ", tolower(prod_raw_ISSCAAP[prod_raw_ISSCAAP$conc_isscaap_group == input$species_country,]$isscaap_group_en[[1]]), ", ", input$year_country)) %>%
      hc_subtitle(text = paste0('Total ', ifelse(length(input$source_country) > 1, "capture and aquaculture production", tolower(input$source_country)), " (tonnes): ", data_total(), ", number of producing countries: ", data_n())) %>%
      hc_caption(text = "Note: the 'Others' category groups all countries with a share of production lower than 1%.") %>%
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)