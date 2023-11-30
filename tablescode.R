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


install.packages("leaflet.extras")

duplicated_merged_data <- read_excel("~/Advanced Research Project/duplicated_merged_data.xlsx")

duplicated_merged_data$Latitude <- as.numeric(duplicated_merged_data$Latitude)
duplicated_merged_data$Longitude <- as.numeric(duplicated_merged_data$Longitude)

duplicated_merged_data<- st_as_sf(mergedData, coords = c("Longitude", "Latitude"), crs = 4326)

duplicated_merged_data <- st_transform(duplicated_merged_data, "+proj=longlat +datum=WGS84")


CAcounties <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/ca-county-boundaries/CA_Counties/CA_Counties_TIGER2016.shp")

CAcounties <- st_transform(CAcounties, crs = "+proj=longlat +datum=WGS84")

#Ventura <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/VCMaster/Master_Address.shp")

#Ventura <- st_transform(Ventura, crs = 4326)

vcCities <- read_sf("C:/Users/ARios/Documents/Advanced Research Project/Cities_gdb/Cities_gdb.shp")

vcCities <- st_transform(vcCities, "+proj=longlat +datum=WGS84")

mergedData <- read_excel("~/Advanced Research Project/mergedData.xlsx")

print(mergedData)

mergedData$Latitude <- as.numeric(mergedData$Latitude)
mergedData$Longitude <- as.numeric(mergedData$Longitude)

mergedData_sf <- st_as_sf(mergedData, coords = c("Longitude", "Latitude"), crs = 4326)

mergedData_sf <- st_transform(mergedData_sf, "+proj=longlat +datum=WGS84")

mergedData

Vaccine_1 <- read_excel("~/Advanced Research Project/Vaccine_1.xlsx")


merged_c <- left_join(mergedData_sf, Vaccine_1, by = "SCHOOL_NAME")

view(merged_c)

merged_c <- merged_c %>%
  distinct(SCHOOL_NAME, .keep_all = TRUE)

merged_c <- merged_c %>%
  select(-contains(".y"))


merged_c <- merged_c %>%
  rename_at(vars(ends_with(".x")), ~sub("\\.x$", "", .))

school_count <- merged_c %>%
  group_by(CITY) %>%
  summarize(number_of_schools = n())

print(school_count)


percentage_vaccinated <- merged_c %>%
  group_by(CITY) %>%
  summarize(
    total_students = sum(ENROLLMENT, na.rm = TRUE),
    vaccinated_students = sum(COUNT, na.rm = TRUE),
    percentage_vaccinated = across(
      c(COUNT, ENROLLMENT),
      ~ sum(.x, na.rm = TRUE) / total_students * 100,
      .names = "{.col}_percentage"
    )
  )










library(dplyr)

percentage_vaccinated <- percentage_vaccinated %>%
  rename(`Percentage Vaccinated` = COUNT_percentage)

print(percentage_vaccinated)

weightedv <- percentage_vaccinated %>%
  as.data.frame() %>%
  left_join(as.data.frame(school_count), by = "CITY") %>%
  mutate(
    multiplied_result = percentage_vaccinated * number_of_schools / 100
  )

weightedv <- weightedv %>%
  select(-contains(".y"))


weightedv <- weightedv %>%
  rename_at(vars(ends_with(".x")), ~sub("\\.x$", "", .))


view(weightedv)

# Create a data frame from the tibble column
weightedv <- weightedv %>%
  mutate(pweightedv = as.data.frame(weightedv))

remove <- c("geometry", "pweightedv")  

weightedv <- weightedv %>%
  select(-one_of(remove))


# Display the data
print(weightedv)



weightedev <- "CITY"  
weightedv <- weightedv %>%
  mutate_if(~is.numeric(.) && colnames(.) != column_to_exclude, as.numeric)


#high to low Percents
weightedev <- weightedev %>% 
  arrange('multiplied_result') %>% 
  mutate('Percentage Vaccinated' = factor('Percentage Vaccinated', levels = 'Percentage Vaccinated'))



# Display the modified data frame
print(weightedv)


# Create a table with kable
styled_table <- weightedv %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

library(knitr)
library(kableExtra)

# Assuming 'weightedv' is your data frame

# Select relevant columns
selected_columns <- c("CITY", "total_students", "vaccinated_students", "percentage_vaccinated", "number_of_schools", "multiplied_result")
weightedv_selected <- weightedv[selected_columns]

# Rename columns for better readability
colnames(weightedv_selected) <- c("City", "Total Students", "Vaccinated Students", "Percentage Vaccinated", "Number of Schools", "Weighted Count")

# Create the table without styling
styled_table <- weightedv_selected %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))



# Display the table
print(styled_table)

library(gt)

# Selecting non-geometry columns
my_table <- weightedv %>%
  select(-geometry) 


my_table <- my_table %>%
  tab_spanner(
    label = "Summary Information",
    columns = c(Total_Count,Mean_Percent)
  )

# Print the table
print(my_table)

print(my_table_subset)



# Assuming your data frame is named 'percentage_vaccinated' and you have a data frame 'school_counts'
# containing the total number of schools in each city

result <- percentage_vaccinated %>%
  left_join(school_counts, by = "CITY_NAME") %>%
  mutate(
    multiplied_result = percentage_vaccinated_percentage * total_schools / 100
  )


view(school_count)









library(dplyr)

# Assuming your data frame is named 'vaccination_data'
percentage_vaccinated <- vaccination_data %>%
  group_by(CITY_NAME) %>%
  summarize(
    total_students = sum(total_number_of_students, na.rm = TRUE),
    vaccinated_students = sum(number_of_vaccinated_students, na.rm = TRUE),
    percentage_vaccinated = vaccinated_students / total_students * 100
  )







vcCities <- st_transform(vcCities, crs = "+proj=longlat +datum=WGS84")


VCcenter = c(34.45192115, -119.10819923)

#Ventura <- st_transform(Ventura, crs = "+proj=longlat +datum=WGS84")
                          

# Get the bounding box coordinates
bbox <- st_bbox(vcCities)

vcCities$CITY_NAME <- factor(vcCities$CITY_NAME)
pal <- colorFactor(
  palette = "Blues",
  domain = vcCities$CITY_NAME)



# Create a leaflet map
map1 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = vcCities, fillOpacity = 0, color = "blue", weight = 2, label = ~CITY_NAME, group = "Cities")

  #fitBounds(min(bbox["xmin"], na.rm = TRUE), min(bbox["ymin"], na.rm = TRUE),
            #max(bbox["xmax"], na.rm = TRUE), max(bbox["ymax"], na.rm = TRUE))
map1



bbox <- st_bbox(vcCities)

CAcounties$NAMELSAD <- factor(CAcounties$NAMELSAD)
pal <- colorFactor(
  palette = "Reds",
  domain = CAcounties$NAMELSAD)


# Create a leaflet map

CAcounties$NAMELSAD <- factor(CAcounties$NAMELSAD)
pal <- colorFactor(
  palette = "Reds",
  domain = CAcounties$NAMELSAD)

map2 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = CAcounties, fillOpacity = 0, color = "red", weight = 2, label = ~NAMELSAD, group = "Counties") 

map2

map3 <- leaflet() %>%
  addTiles() %>%
  addMarkers(
    data = mergedData_sf,
    group = "Schools",
    label = ~ SCHOOL_NAME,
    lng = ~ st_coordinates(geometry)[, 1],  # Adjust for longitude
    lat = ~ st_coordinates(geometry)[, 2],   # Adjust for latitude
    clusterOptions = markerClusterOptions()
  )
    

map3


map4 <- leaflet() %>%
  addTiles() %>%
  addMarkers(
    data = mergedData_sf,
    group = "Schools",
    label = ~ SCHOOL_NAME,
    clusterOptions = markerClusterOptions()
  )

map4

pal_state_pop <- colorNumeric("viridis", mergedData_sf$COUNT)

label_county <- function(SCHOOL_NAME, COUNT){
  str_glue("{SCHOOL_NAME} with count of {COUNT}")
}

mergedData_sf %>% 
  leaflet() %>% 
  addPolygons(data = mergedData_sf,
              weight = 1,
              color = "white",
              fillColor = ~pal_state_pop(COUNT),
              fillOpacity = 1,
              popup = ~label_county(SCHOOL_NAME, COUNT)) %>% 
  addLegend(pal = pal_state_pop,
            values = ~COUNT,
            opacity = 1)

map5


PercentVacc<-ggplot(mergedData_sf, aes(x = SCHOOL_NAME, y = PERCENT)) +
  geom_bar(stat="identity", fill="steelblue")

# Filter CAcounties to include only the desired county

overlayMap <- leaflet() %>%
  addTiles() %>%
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

overlayMap


VCcenter = c(34.45192115, -119.10819923)


HeatMap <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = vcCities, fillOpacity = 0, color = "blue", weight = 2, label = ~CITY_NAME, group = "Cities")%>%
  addHeatmap(data = duplicated_merged_data_sf,
             lng = ~ st_coordinates(geometry)[, 2],  # Adjust for longitude
             lat = ~ st_coordinates(geometry)[, 1],   # Adjust for latitude
             intensity = ~COUNT,
             radius = 20, 
             blur = 15,
             max = 1)%>%
  addMarkers(
    data = duplicated_merged_data_sf,
    group = "Schools",
    label = ~ SCHOOL_NAME,
    lng = ~ st_coordinates(geometry)[, 2],  # Adjust for longitude
    lat = ~ st_coordinates(geometry)[, 1],   # Adjust for latitude
    clusterOptions = markerClusterOptions()
  )
  

HeatMap
# Print the map
overlayMap


popupGraph(
  group = "Schools",
  options = popupOptions(width = 300, height = 200),
  plot = function(feature, map) {
    # Customize the content of the popup based on the clicked area (feature)
    plot_ly(
      x = ~SCHOOL_NAME,
      y = ~PERCENT,
      type = "bar",
      marker = list(color = "blue")
    ) %>%
      layout(title = "Percent Vaccinated")
  }
)








library(maps)
library(mapproj)
source("census-app/helpers.R")

percent_map(vcCities$white, "purple", "% White")



Ventura <- st_transform(Ventura, crs = "+proj=longlat +datum=WGS84")


ventura_county_vars <- load_variables(2019, "acs5", cache = TRUE)


ventura_sf <- get_acs(
  geography = "tract",
  variables = c("B17001_001"),
  state = "CA",
  county = "Ventura",
  year = 2019
)

names(ventura_sf)
names(vcCities)

ventura_sf <- st_as_sf(vcCities, crs = 4326)

ventura_sf <- vcCities %>%
  st_as_sf() %>%
  st_set_geometry(NULL) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  setNames(c("longitude", "latitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



ventura_sf <- st_as_sf(ventura_sf, coords = c("GEOID"), crs = 4326)

map1 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = vcCities, fillOpacity = 0, color = "blue", weight = 2, label = ~CITY_NAME, group = "Cities")

ventura_sf <- merge(vcCities, ventura_sf, by = "GEOID")

# Create a simple leaflet map
ventura_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = ventura_sf, popup = ~paste("Poverty Rate: ", round(poverty_rate, 2), "%"))

# Display the map
ventura_map

# Check column names
names(ventura_poverty)





runGeoWeightedModel ()

  

  
  



