clean_dataframe <- function(df){
  df2 <- df %>%
    separate(
      ANMOIS,
      c("an","mois"),
      4
    ) %>% mutate(mois = str_remove(mois,"0"))
  
  
  colnames(df2) <- tolower(colnames(df2))
  
  return(df2)
}

