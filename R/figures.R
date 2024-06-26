plot_airport_line <- function(df,airport){
  a <- df %>%
    mutate(trafic=apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
    mutate(date=lubridate::ym(anmois)) %>%
    mutate(infobulle=paste("Aeroport :",apt_nom,"trafic :", trafic))%>%
    filter(apt==airport) 
  
  fig <- plot_ly(data=a, x = ~date, y = ~trafic, text=~infobulle, hovertemplate=~infobulle, type = 'scatter', mode = 'lines+markers')
  
  return(fig)
}