#################################################
# Title: Maps for Mod Biodiverstiy SOP
# Purpose: create maps of CABR and CHIS sites for SOP
# Author: LP
# Created: 2/22/22
# Last edited: 2/22/22
##################################################

##### presets #####

# folder to save things 
save_folder <- './figures/maps_mod_biodiversity_figs/'

##### packages #####
library(readxl)     # read excel files
library(tidyverse)  # tidyverse packages
library(leaflet)    # mapping
library(mapview)    # save static leaflet map output

##### load site info & lat/long data #####

sites <- read_excel("data/mod_biodiversity_site_table.xlsx", sheet = "Sheet3")

# load park tiles
NPSbasic = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

NPSlight = 'https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg'

##### map! #####

# CABR sites map
map_fn <- function(dataset, zoomset, savename){

makemap <- leaflet() %>% 
  # set zoom
  setView(lng = mean(dataset$Longitude), lat = mean(dataset$Latitude), zoom = zoomset) %>%
  # add basemap 'NPS basic' from url
  addTiles(group = 'Map',
           urlTemplate = NPSlight) %>%
  # points for sites with site name label
  addCircleMarkers(data = dataset,
                   lng = dataset$Longitude,
                   lat = dataset$Latitude,
                   label = dataset$`Site Name`,
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, 
                                               direction = 'left', textsize = '14px'),
                   fillColor = '#EAFF16',
                   radius = 4,
                   fillOpacity = 1) %>% 
  # scale bar and settings
  addScaleBar(position = 'bottomright') %>% 
  scaleBarOptions(maxWidth = 10, metric = TRUE) 

# save map as static
mapview::mapshot(
  makemap,
  file = paste(save_folder, 'map_', savename, '.png'),
  remove_controls = c('zoomControl')
)
  
}

map_fn(dataset = filter(sites, `Park Unit` == 'CABR'),
       savename = 'cabr',
       zoomset = 15
       )
