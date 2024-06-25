library(readr)
library(dplyr)
library(stringr)
library(sf)
library(plotly)

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

a <- pax_apt_all %>%
  mutate(trafic=apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  mutate(date=lubridate::ym(anmois)) %>%
  mutate(infobulle=paste("Aeroport :",apt_nom,", trafic :"))
  filter(apt==default_airport) 

ggplot(data = a, aes(x = date, y = trafic)) + geom_line()

fig <- plot_ly(data=a, x = ~date, y = ~trafic, type = 'scatter', mode = 'lines+markers')
fig




