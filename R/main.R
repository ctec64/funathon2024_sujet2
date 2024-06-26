library(readr)
library(dplyr)
library(stringr)
library(sf)
library(plotly)
library(ggplot2)
library(gt)
library(leaflet)

source("correction/R/import_data.R")
source("correction/R/create_data_list.R")
source("correction/R/clean_dataframe.R")
source("correction/R/figures.R")
source("correction/R/divers_functions.R")
sources("correction/R/tables.R")

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12
year <- YEARS_LIST[1]
month <- MONTHS_LIST[1]

# Load data ----------------------------------
urls <- create_data_list("./sources.yml")


pax_apt_all <- import_airport_data(unlist(urls$airports))
pax_cie_all <- import_compagnies_data(unlist(urls$compagnies))
pax_lsn_all <- import_liaisons_data(unlist(urls$liaisons))

airports_location <- st_read(urls$geojson$airport)

liste_aeroports <- unique(pax_apt_all$apt)
default_airport <- liste_aeroports[1]

#Objets nécessaires à l'application ---------------------
trafic_aeroports <- pax_apt_all %>% 
  filter(apt==default_airport) %>%
  mutate(trafic=apt_pax_dep+apt_pax_tr+apt_pax_arr,
         date=as.Date(paste(anmois,"01",sep=""),format="%Y%m%d"))

stats_aeroports <- summary_stat_airport(
  create_data_from_input(pax_apt_all, year, month)
)

stats_aeroports_table <- stats_aeroports %>%
  mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
  ) %>%
  select(name_clean, everything())


#Valorisation ----
graph_dyna_trafic <- plot_airport_line(trafic_aeroports,default_airport)

table_airports <- create_table_airports(stats_aeroports)

carte_interactive <- map_leaflet_airport(pax_apt_all,airports_location,month,year)







