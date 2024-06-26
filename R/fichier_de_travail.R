
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

#graphique dynamique des trafics
graph_trafic <- ggplot(trafic_aeroports,aes(x=date,y=trafic)) +
  geom_line(stat="identity")

graph_trafic

#Tableau HTML pour afficher les données ----

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12

#filtre sur un mois et une année
aeroport_filtre <- pax_apt_all %>% 
  filter(mois=="7" & an=="2022")

#création de la fonction
create_data_from_input <- function(dataframe, year, month) {
  dataframe <- dataframe %>% 
    filter(mois==month & an==year)
  
  return(dataframe)
    
}

#classement des aéroports par fréquentation
stats_aeroports <- pax_apt_all %>% 
  group_by(apt,apt_nom) %>% 
  summarise(tot_pax_dep=round(sum(apt_pax_dep,na.rm=T),3),
            tot_pax_arr=round(sum(apt_pax_arr,na.rm=T),3),
            tot_pax_tr=round(sum(apt_pax_tr,na.rm=T),3)) %>% 
   arrange(desc(tot_pax_dep)) %>% 
  ungroup()


stats_aeroports_table <- stats_aeroports %>%
  mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
  ) %>%
  select(name_clean, everything())

#création de la fonction
summary_stat_airport <- function(dataframe) {
  stats_aeroports <- dataframe %>%  group_by(apt,apt_nom) %>% 
    summarise(tot_pax_dep=round(sum(apt_pax_dep,na.rm=T),3),
              tot_pax_arr=round(sum(apt_pax_arr,na.rm=T),3),
              tot_pax_tr=round(sum(apt_pax_tr,na.rm=T),3)) %>% 
    arrange(desc(tot_pax_dep)) %>% 
    ungroup()
  
  return(stats_aeroports)
  
    
}

#fonction nb de passagers locaux 
summary_stat_liaisons <- function(data){
  agg_data <- data %>%
    group_by(lsn_fsc) %>%
    summarise(
      paxloc = round(sum(lsn_pax_loc, na.rm = TRUE)*1e-6,3)
    ) %>%
    ungroup()
  return(agg_data)
}

stat_liaisons <- summary_stat_liaisons(pax_lsn_all)


#création d'un tableau HTML à l'aide du package GT

table_aeroports <- stats_aeroports_table %>% 
  select(-starts_with("apt")) %>% 
  gt() %>% 
  tab_header(title = md("**Statistiques de fréquentation**"),
             subtitle=md("Classement des aéroports")) %>% 
  cols_label(name_clean=md("**Aéroport**"),
             tot_pax_dep=md("**Départs**"),
             tot_pax_arr=md("**Arrivées**"),
             tot_pax_tr=md("**Transit**")) %>% 
  tab_source_note(md("_Source: DGAC, à partir des données sur data.gouv.fr_")) %>% 
  tab_style(style=list(cell_fill(color="lightcyan"),
                       cell_text(weight="bold")),
            locations=cells_body(columns=name_clean)) %>% 
   fmt_number(columns = everything(),suffixing=T) %>% 
  fmt_markdown(columns = "name_clean") %>% 
  opt_interactive()

table_aeroports

#fonction associée
create_table_airports <- function(stats_aeroports){
  
  stats_aeroports_table <- stats_aeroports %>%
    mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
    ) %>%
    select(name_clean, everything())
  
  table_aeroports <- gt(stats_aeroports_table)
  
  table_aeroports <- table_aeroports %>%
    cols_hide(columns = starts_with("apt"))
  
  table_aeroports <- table_aeroports %>%
    fmt_number(columns = starts_with("pax"), suffixing = TRUE)
  
  table_aeroports <- table_aeroports %>%
    fmt_markdown(columns = "name_clean")
  
  table_aeroports <- table_aeroports %>%
    cols_label(
      name_clean = md("**Aéroport**"),
      paxdep = md("**Départs**"),
      paxarr = md("**Arrivée**"),
      paxtra = md("**Transit**")
    ) %>%
    tab_header(
      title = md("**Statistiques de fréquentation**"),
      subtitle = md("Classement des aéroports")
    ) %>%
    tab_style(
      style = cell_fill(color = "powderblue"),
      locations = cells_title()
    ) %>%
    tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_"))
  
  table_aeroports <- table_aeroports %>%
    opt_interactive()
  
  return(table_aeroports)
  
}


#Carte trafic des aéroports

month <- 1
year <- 2019

palette <- c("darkred", "dodgerblue","forestgreen","gold")

# icônes
icons <- awesomeIcons(
  icon = 'plane',
  iconColor = 'black',
  library = 'fa',
  markerColor = trafic_aeroports$color
)

#filtre sur janvier 2019
trafic_date <- pax_apt_all %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  ) %>%
  filter(mois == month, an == year)

#fusion avec lieu de localisation des aéroports
trafic_aeroports <- airports_location %>%
  inner_join(trafic_date, by = c("Code.OACI" = "apt"))

#création d'une variable volume qui classe chaque observation dans son tercile 
# et transforme la valeur en couleur à partir de palette.

trafic_aeroports <- trafic_aeroports %>% 
  mutate(
    volume = ntile(trafic, 3)
  ) %>%
  mutate(
    color = palette[volume]
  )

#création de la carte interactive  
carte_interactive <- leaflet(trafic_aeroports) %>% addTiles() %>%
  addAwesomeMarkers(
    icon=icons[],
    label=~paste0(Nom, "", " (",Code.OACI, ") : ", trafic, " voyageurs")
  )

carte_interactive


#fonction associée

map_leaflet_airport <- function(df,airports_location,month,year) {

 #palette de couleur
   palette <- c("darkred", "dodgerblue","forestgreen","gold")
  
  # icônes
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = trafic_aeroports$color
  )
  
  #filtre un mois et une année
  trafic_date <- df %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    ) %>%
    filter(mois == month, an == year)
  
  #fusion avec lieu de localisation des aéroports
  trafic_aeroports <- airports_location %>%
    inner_join(trafic_date, by = c("Code.OACI" = "apt"))
  
  #création d'une variable volume qui classe chaque observation dans son tercile 
  # et transforme la valeur en couleur à partir de palette.
  
  trafic_aeroports <- trafic_aeroports %>% 
    mutate(
      volume = ntile(trafic, 3)
    ) %>%
    mutate(
      color = palette[volume]
    )
  
  #création de la carte interactive  
  carte_interactive <- leaflet(trafic_aeroports) %>% addTiles() %>%
    addAwesomeMarkers(
      icon=icons[],
      label=~paste0(Nom, "", " (",Code.OACI, ") : ", trafic, " voyageurs")
    )
  
  return(carte_interactive)
  }




