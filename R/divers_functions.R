#filtre sur une année et un mois
create_data_from_input <- function(data, year, month){
  data <- data %>%
    filter(mois %in% month, an %in% year)
  return(data)
}


#classement des aéroports par fréquentation

summary_stat_airport <- function(dataframe) {
  stats_aeroports <- dataframe %>%  group_by(apt,apt_nom) %>% 
    summarise(tot_pax_dep=round(sum(apt_pax_dep,na.rm=T),3),
              tot_pax_arr=round(sum(apt_pax_arr,na.rm=T),3),
              tot_pax_tr=round(sum(apt_pax_tr,na.rm=T),3)) %>% 
    arrange(desc(tot_pax_dep)) %>% 
    ungroup()
  
  return(stats_aeroports)
  
  
}

#
summary_stat_liaisons <- function(data){
  agg_data <- data %>%
    group_by(lsn_fsc) %>%
    summarise(
      paxloc = round(sum(lsn_pax_loc, na.rm = TRUE)*1e-6,3)
    ) %>%
    ungroup()
  return(agg_data)
}