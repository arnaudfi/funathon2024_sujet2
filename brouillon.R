a <- read_csv2(
  unlist(urls$airports),
  col_types = cols(
    ANMOIS = col_character(),
    APT = col_character(),
    APT_NOM = col_character(),
    APT_ZON = col_character(),
    .default = col_double()
  )
)

b <- a %>%
  separate(
    ANMOIS,
    c("an","mois"),
    4
  )

d <- b %>%
  mutate(mois = str_remove(mois,"0"))


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

import_airport_data <- function(list_files){
  df <- read_csv2(
    list_files,
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  )
    
  df_clean <- clean_dataframe(df) 
  
  return(df_clean)
}

a <- import_airport_data(unlist(urls$airports))


e <- clean_dataframe(unlist(urls$airports))


# import_compagnies_data 

a <- read_csv2(
  unlist(urls$compagnies),
  col_types = cols(
    ANMOIS = col_character(),
    CIE  = col_character(),
    CIE_NOM  = col_character(),
    CIE_NAT  = col_character(),
    CIE_PAYS   = col_character(),
    .default = col_double()
  )
)

import_compagnies_data <- function(list_files){
  df <- read_csv2(
    list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE  = col_character(),
      CIE_NOM  = col_character(),
      CIE_NAT  = col_character(),
      CIE_PAYS   = col_character(),
      .default = col_double()
    )
  )
  
  df_clean <- clean_dataframe(df) 
  
  return(df_clean)
}

a <- import_compagnies_data(unlist(urls$compagnies))


a <- read_csv2(
  unlist(urls$liaisons),
  col_types = cols(
    ANMOIS = col_character(),
    LSN = col_character(),
    LSN_DEP_NOM = col_character(),
    LSN_ARR_NOM = col_character(),
    LSN_SCT = col_character(),
    LSN_FSC = col_character(),,
    .default = col_double()
  )
)

import_liaisons_data <- function(list_files){
  df <- read_csv2(
    list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),,
      .default = col_double()
    )
  )
  
  df_clean <- clean_dataframe(df) 
  
  return(df_clean)
}

a <- import_liaisons_data(unlist(urls$liaisons))




airports_location <- st_read(unlist(urls$geojson$airport))

leaflet(airports_location) %>%
  addTiles() %>%
 addMarkers(popup = ~Nom)

leaflet(airports_location) %>%
  addProviderTiles(providers$GeoportailFrance.plan) %>%
  addMarkers(popup = ~Nom)









x <- c(1:100)

random_y <- rnorm(100, mean = 0)

data <- data.frame(x, random_y)


fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')


fig






plot_airport_line <- function(df,airport){
  a <- df %>%
    mutate(trafic=apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
    mutate(date=lubridate::ym(anmois)) %>%
    mutate(infobulle=paste("Aeroport :",apt_nom,"trafic :", trafic))%>%
    filter(apt==airport) 
  
  fig <- plot_ly(data=a, x = ~date, y = ~trafic, text=~infobulle, hovertemplate=~infobulle, type = 'scatter', mode = 'lines+markers')
  
  return(fig)
}

a <- plot_airport_line(df=pax_apt_all, airport="LFRZ")



year <- YEARS_LIST[4]
month <- MONTHS_LIST[8]
df <- pax_apt_all

create_data_from_input <- function(df,year,month){

df_filt <- df %>%
  filter(an == year & mois==month)

return(df_filt)

}

a <- create_data_from_input(pax_apt_all, "2021",2)

summary_stat_airport <- function(df){
stat_airports <- df %>%
  group_by(apt, apt_nom) %>%
  summarise(
    apt_pax_dep = sum(apt_pax_dep), 
    apt_pax_arr=sum(apt_pax_arr) ,
    apt_pax_tr=sum(apt_pax_tr),
    apt_peq=sum(apt_peq)     
  ) %>%
  arrange(-apt_peq) %>%
  ungroup()

return(stat_airports)

}

a <- summary_stat_airport(pax_apt_all)


# tableau propre

colonnes_numeriques <- stats_aeroports_table %>% select_if(is.numeric) %>% colnames()

stats_aeroports_table %>% 
  select(-apt,-apt_nom) %>%
  gt() %>%
  fmt_number(columns=colonnes_numeriques, suffixing=T) %>%
  fmt_markdown(columns=name_clean) %>%
  cols_label(
    name_clean="Aéroport",
    paxdep="Passagers au départ", 
    paxarr="Passagers à l'arrivée", 
    paxtra="Passagers en vol", 
  ) %>%
  opt_interactive()



