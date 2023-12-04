library(leaflet)
library(tidyverse)
library(sf)
library(csustats)
library(ggmap)
library(tigris)
library(remotes)
library(tidycensus)
library(readxl)
library(mapview)
library(tmap)
library(tmaptools)
library(fastmap)
library(ggplot2)
library(dplyr)
library(rmapshaper)
library(mapboxapi)
library(fasterize)
library(leafsync)
library(leaflet.extras)
library(readxl)
library(leafpop)
library(GeoWeightedModel)
library(maps)
library(mapproj)
library(gt)
library(readxl)
library(kableExtra)
library(knitr)
library(spdep)
library(tmap)
library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='csuciadvancedresearchproject',
                          token='9D53848362FA9C8E152FBC60D97C4015',
                          secret='GQX5ZdSm7c+1Pg/IdcBNAddlwPe/1zkuhUo99woB')

duplicated_merged_data <- read_excel("~/Advanced Research Project/duplicated_merged_data.xlsx")

duplicated_merged_data$Latitude <- as.numeric(duplicated_merged_data$Latitude)
duplicated_merged_data$Longitude <- as.numeric(duplicated_merged_data$Longitude)

duplicated_merged_data<- st_as_sf(mergedData, coords = c("Longitude", "Latitude"), crs = 4326)

duplicated_merged_data <- st_transform(duplicated_merged_data, "+proj=longlat +datum=WGS84")


CAcounties <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/ca-county-boundaries/CA_Counties/CA_Counties_TIGER2016.shp")

CAcounties <- st_transform(CAcounties, crs = "+proj=longlat +datum=WGS84")

Ventura <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/VCMaster/Master_Address.shp")

Ventura <- st_transform(Ventura, crs = 4326)

vcCities <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/Cities_gdb/Cities_gdb.shp")

vcCities <- st_transform(vcCities, "+proj=longlat +datum=WGS84")

mergedData <- read_excel("~/Advanced Research Project/mergedData.xlsx")

print(mergedData)

mergedData$Latitude <- as.numeric(mergedData$Latitude)
mergedData$Longitude <- as.numeric(mergedData$Longitude)

mergedsf <- st_as_sf(mergedData, coords = c("Longitude", "Latitude"), crs = 4326)



#mergedData_sf <- st_transform(mergedData_sf, "+proj=longlat +datum=WGS84")


Vaccine_1 <- read_excel("~/Advanced Research Project/Vaccine_1.xlsx")


merged_c <- left_join(mergedsf, Vaccine_1, by = "SCHOOL_NAME")

merged_c <- st_as_sf(merged_c, coords = c("Longitude", "Latitude"), crs = 4326)

view(merged_c)

# Merging and Cleaning Data

merged_c <- merged_c %>%
  distinct(SCHOOL_NAME, .keep_all = TRUE)

merged_c <- merged_c %>%
  select(-contains(".y"))


schls <- merged_c %>%
  rename_at(vars(ends_with(".x")), ~sub("\\.x$", "", .))



bbox <- st_bbox(Ventura)


bbox <- matrix(bbox, nrow = 1, byrow = TRUE)


clusterMap <- leaflet() %>%
  addTiles() %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  addPolygons(data = vcCities, fillOpacity = 0, color = "blue", weight = 2, label = ~CITY_NAME, group = "Cities")%>%
  addPolygons(data = CAcounties, fillOpacity = 0, color = "red", weight = 2, label = ~NAMELSAD, group = "Counties")%>% 
  addMarkers(
    data = duplicated_merged_data_sf,
    group = "Schools",
    label = ~SCHOOL_NAME,
    lng = ~ st_coordinates(geometry)[, 2],  # Adjust for longitude
    lat = ~ st_coordinates(geometry)[, 1],   # Adjust for latitude
    clusterOptions = markerClusterOptions()
  )

HeatMap <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = vcCities, fillOpacity = 0, color = "blue", weight = 2, label = ~CITY_NAME, group = "Cities")%>%
  addHeatmap(data = duplicated_merged_data_sf,
             lng = ~ st_coordinates(geometry)[, 2],  # Adjust for longitude
             lat = ~ st_coordinates(geometry)[, 1],   # Adjust for latitude
             intensity = ~COUNT,
             radius = 20, 
             blur = 15,
             max = 1)


synced_maps <- sync(clusterMap, HeatMap)

synced_maps

library(shiny)




shinyApp(
  ui = tagList(synced_maps),
  server = function(input, output) {}
)


