# Changing HC default theme

newtheme <- hc_theme_merge(
  getOption("highcharter.theme"),  
  hc_theme(colors = c('#377eb8','#4daf4a','#e41a1c','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'),
           chart = list(style = list(fontFamily = "Open Sans")))
)

options(highcharter.theme = newtheme)

hc_export_options <- c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")

# Load data

data_all <- readRDS("prod_all.RDS")

data_division <- readRDS("prod_ISSCAAP_division.RDS")

data_group <- readRDS("prod_ISSCAAP_group.RDS")

data_yearbook <- readRDS("prod_yearbook_selection.RDS")

map <- readRDS("map.RDS")

# More user-friendly y axes on plots

addUnits <- function(n) {
  labels <- ifelse(n < 1000, round(n, 1),  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, 1), ' thousand'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, 1), ' million'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, 1), ' billion'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12, 1), ' trillion'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}