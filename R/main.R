library(readr)
library(dplyr)
library(stringr)
library(sf)
library(plotly)
library(ggplot2)

source("correction/R/import_data.R")
source("correction/R/create_data_list.R")
source("correction/R/clean_dataframe.R")
source("correction/R/figures.R")


# Load data ----------------------------------
urls <- create_data_list("./sources.yml")


pax_apt_all <- import_airport_data(unlist(urls$airports))
pax_cie_all <- import_compagnies_data(unlist(urls$compagnies))
pax_lsn_all <- import_liaisons_data(unlist(urls$liaisons))

airports_location <- st_read(urls$geojson$airport)

liste_aeroports <- unique(pax_apt_all$apt)
default_airport <- liste_aeroports[1]

#Graphique statique de fréquentation des aéroports ---------------------
trafic_aeroports <- pax_apt_all %>% 
  filter(apt==default_airport) %>%
  mutate(trafic=apt_pax_dep+apt_pax_tr+apt_pax_arr,
         date=as.Date(paste(anmois,"01",sep=""),format="%Y%m%d"))

graph_trafic <- ggplot(trafic_aeroports,aes(x=date,y=trafic)) +
  geom_line(stat="identity")

graph_trafic

#Graphique dynamique de fréquentation des aéroports ----
graph_dyna_trafic <- plot_airport_line(trafic_aeroports,default_airport)

graph_dyna_trafic





