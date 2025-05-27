##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)
library(highcharter)

# Get production data from FAO's server

temp <- tempfile()
download.file("https://www.fao.org/fishery/static/Data/GlobalProduction_2025.1.0.zip", temp)
data <- read_csv(unz(temp, "Global_production_quantity.csv")) %>%
  clean_names()
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  clean_names() %>%
  select(un_code, iso2_code, name_en, continent_group_en, geo_region_group_en)
species <- read_csv(unz(temp, "CL_FI_SPECIES_GROUPS.csv")) %>%
  clean_names() %>%
  select(x3a_code, name_en, scientific_name, isscaap_group_en, yearbook_group_en)
water <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv")) %>%
  clean_names() %>%
  select(code, name_en, inland_marine_group_en)
source <- read_csv(unz(temp, "CL_FI_PRODUCTION_SOURCE_DET.csv")) %>%
  clean_names() %>%
  select(code, name_en)
unlink(temp)

# Join tables, restructure and export data

prod_raw <- data %>%
  left_join(countries, by = c("country_un_code" = "un_code"), keep = FALSE) %>%
  rename(un_code = country_un_code, country = name_en) %>%
  left_join(species, by = c("species_alpha_3_code" = "x3a_code"), keep = FALSE) %>%
  rename(`3alpha_code` = species_alpha_3_code, species_name = name_en) %>%
  left_join(water, by = c("area_code" = "code"), keep = FALSE) %>%
  rename(fishing_area_code = area_code, fishing_area_name = name_en) %>%
  left_join(source, by = c("production_source_det_code" = "code"), keep = FALSE) %>%
  rename(production_source_detailed_name = name_en) %>%
  mutate(production_source_name = case_when(
    production_source_detailed_name == "Aquaculture production (freshwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (brackishwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (marine)" ~ "Aquaculture production",
    production_source_detailed_name == "Capture production" ~ "Capture production",
    production_source_detailed_name == "Aquaculture production" ~ "Aquaculture production")
    ) %>%
  mutate(measure = case_when(
    measure == "Q_tlw" ~ "Tonnes - live weight", 
    measure == "Q_no_1" ~ "Number")
  ) %>%
  rename(unit = measure,	year = period,	flag = status) %>%
  select(un_code, country, continent_group_en, geo_region_group_en, `3alpha_code`, species_name, scientific_name, isscaap_group_en, yearbook_group_en, fishing_area_code, fishing_area_name, inland_marine_group_en, production_source_detailed_name, production_source_name, unit, year, value, flag) %>%
  group_by_at(vars(-value)) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Add "China, " in front of the name for Taiwan as per OCC request

prod_raw$country[prod_raw$un_code == "158"] <- "China, Taiwan Province of China"

# Join geographical coordinates

cou_coordinates <- read_csv("https://raw.githubusercontent.com/pamdx/country_coordinates/refs/heads/main/country_coordinates.csv") %>%
  select(un_code, lat, lon)

prod_raw <- prod_raw %>%
  left_join(y = cou_coordinates)

if (any(is.na(prod_raw$lat)) | any(is.na(prod_raw$lon))) {
  
  warning(paste("The following countries are missing coordinates:", 
             prod_raw %>%
               filter(is.na(lat)) %>%
               pull(country) %>%
               unique()
             )
       )
  
}

# Aggregate data at species group level

isscaap_classif <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_GROUP.csv", col_types = "ccccc", col_select = c(ISSCAAP_Code, Name_En)) %>%
  rename(isscaap_group_code = ISSCAAP_Code, isscaap_group_en = Name_En) %>%
  mutate(conc_isscaap_group = paste(isscaap_group_code, "-", isscaap_group_en),
         isscaap_division_code = substr(isscaap_group_code, 1, 1)) %>%
  left_join(read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_DIVISION.csv", col_types = "ccccc", col_select = c(ISSCAAP_Code, Name_En)), by = c("isscaap_division_code" = "ISSCAAP_Code")) %>%
  rename(isscaap_division_en = Name_En) %>%
  mutate(conc_isscaap_division = paste(isscaap_division_code, "-", isscaap_division_en))

# All classifications

prod_all <- prod_raw %>%
  left_join(isscaap_classif) %>%
  group_by(country, yearbook_group_en, conc_isscaap_division, conc_isscaap_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup()

saveRDS(prod_all, "prod_all.RDS")

# Yearbook selection

prod_yearbook_selection <- prod_raw %>%
  left_join(isscaap_classif) %>%
  group_by(country, yearbook_group_en, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(yearbook_group_en = case_when(
    yearbook_group_en == "Fish, crustaceans and molluscs, etc." ~ "Aquatic animals",
    yearbook_group_en == "Other aq. animals & products" ~ "Other aq. animals & products",
    yearbook_group_en == "Aquatic plants" ~ "Algae",
    .default = yearbook_group_en
  )) %>%
  rename(species_group = yearbook_group_en)

saveRDS(prod_yearbook_selection, "prod_yearbook_selection.RDS")

# ISSCAAP division

prod_ISSCAAP_division <- prod_raw %>%
  left_join(isscaap_classif) %>%
  group_by(country, isscaap_division_code, isscaap_division_en, conc_isscaap_division, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(species_group = conc_isscaap_division)

saveRDS(prod_ISSCAAP_division, "prod_ISSCAAP_division.RDS")

# ISSCAAP group

prod_ISSCAAP_group <- prod_raw %>%
  left_join(isscaap_classif) %>%
  group_by(country, isscaap_group_code, isscaap_group_en, conc_isscaap_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(species_group = conc_isscaap_group)

saveRDS(prod_ISSCAAP_group, "prod_ISSCAAP_group.RDS")

# Download map for HC viz

# map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)
# 
# saveRDS(map, "map.RDS")