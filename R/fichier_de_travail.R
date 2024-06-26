
#Paramétrages ----
urls <- create_data_list("sources.yml")


#Table aeroports ----
#importation des données
library(readr)

aeroports <- readr::read_csv2(unlist(urls$airports),
          col_types = cols(
            ANMOIS = col_character(),
            APT = col_character(),
            APT_NOM = col_character(),
            APT_ZON = col_character(),
            .default = col_double()
          ))



#fonction pour créer les variables an et mois
library(dplyr)

clean_dataframe <- function(dataframe) {
  
  #création des variables an et mois
  dataframe_corr <- dataframe %>% 
    mutate(an=substr(ANMOIS,1,4),
           mois=ifelse(substr(ANMOIS,5,5)=="0",substr(ANMOIS,6,6),substr(ANMOIS,5,6)))
  
  #passage des noms de colonnes en minuscule
  colnames(dataframe_corr) <- tolower(colnames(dataframe_corr))
  
  return(dataframe_corr)
}


aeroports <- clean_dataframe(aeroports)

#fonction importation aeroports
import_airport_data <- function(list_files){
  
  table_aeroports <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(table_aeroports)
  
}


#Table compagnies ----
compagnies <- readr::read_csv2(unlist(urls$airports),
                               col_types = cols(
                                 ANMOIS = col_character(),
                                 CIE = col_character(),
                                 CIE_NOM = col_character(),
                                 CIE_NAT = col_character(),
                                 CIE_PAYS = col_character(),
                                 .default = col_double()
                               ))


#fonction d'importation
import_compagnies_data <- function(list_file) {
  table_compagnie <- readr::read_csv2(file=list_files,
                                col_types = cols(
                                  ANMOIS = col_character(),
                                  CIE = col_character(),
                                  CIE_NOM = col_character(),
                                  CIE_NAT = col_character(),
                                  CIE_PAYS = col_character(),
                                  .default = col_double()
                                )) %>% 
    clean_dataframe()
  
  return(table_compagnie)
  
}

compagnies <- clean_dataframe(compagnies)


#Table liaison ----
liaisons <- read_csv2(unlist(urls$liaisons),
                      col_types = cols(
                        ANMOIS = col_character(),
                        LSN = col_character(),
                        LSN_DEP_NOM = col_character(),
                        LSN_ARR_NOM = col_character(),
                        LSN_SCT = col_character(),
                        LSN_FSC = col_character(),
                        .default = col_double()
                      ))

liaisons <- clean_dataframe(liaisons)

#fonction importation liaisons

import_liaisons_data <- function(list_files){
  
  table_liaisons <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  
  return(table_liaisons)
  
  
}

#Cartographie ----
library(sf)
library(leaflet)

#importation fichier geolocalisation
airports_location <- sf::st_read(unlist(urls$geojson))

#vérification du système de représentation
sf::st_crs(airports_location)

#carte de localisation des aéroports
carte_localisation_aeroport <- leaflet(data=airports_location) %>% 
  addTiles() %>% 
  addMarkers(popup = ~Nom)

carte_localisation_aeroport
