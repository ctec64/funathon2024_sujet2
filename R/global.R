library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(sf)
library(plotly)
library(ggplot2)
library(gt)
library(leaflet)
library(bslib)

source("R/import_data.R")
source("R/create_data_list.R")
source("R/clean_dataframe.R")
source("R/figures.R")
source("R/divers_functions.R")
sources("R/tables.R")

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12
years <- YEARS_LIST[1]
months <- MONTHS_LIST[1]

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

