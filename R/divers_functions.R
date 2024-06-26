create_data_from_input <- function(df,year,month){
  
  df_filt <- df %>%
    filter(an == year & mois==month)
  
  return(df_filt)
  
}


summary_stat_airport <- function(df){
  stat_airports <- df %>%
    group_by(apt, apt_nom) %>%
    summarise(
      apt_pax_dep = sum(apt_pax_dep), 
      apt_pax_arr=sum(apt_pax_arr) ,
      apt_pax_tr=sum(apt_pax_tr),
      apt_peq=sum(apt_peq)     
    ) %>%
    arrange(-apt_peq)
  
  return(stat_airports)
  
}