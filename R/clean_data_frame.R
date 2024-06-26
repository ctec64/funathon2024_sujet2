#fonction pour créer les variables an et mois
clean_dataframe <- function(dataframe) {
  
  #création des variables an et mois
  dataframe <- dataframe %>% 
    mutate(an=substr(ANMOIS,1,4),
           mois=ifelse(substr(ANMOIS,5,5)=="0",substr(ANMOIS,6,6),substr(ANMOIS,5,6)))
  
  #passage des noms de colonnes en minuscule
  colnames(dataframe) <- tolower(colnames(dataframe))
  
  return(dataframe)
}