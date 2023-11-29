library(tmap)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(sf)
library(leaflet)
library(mapboxapi)
library(fasterize)
library(leafsync)
library(plotly)
library(dplyr)
library(readr)
library(readxl)
library(RColorBrewer)
library(gt)


# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)



masterVac <- read_csv("Desktop/Vaccine Research Project/Vaccination_Coverage_and_Exemptions_among_Kindergartners.csv", 
                                                                     col_types = cols(`Estimate (%)` = col_number(), 
                                                                                      `Population Size` = col_number(), 
                                                                                      Footnotes = col_skip()))

vc <- read_excel("Desktop/Vaccine Research Project/mergedData.xlsx")


write.csv(vc, file = "vcKinderVaccRates.csv", row.names = FALSE)


vcKVC <- read_csv("vcKinderVaccRates.csv")



schoolkey <- read_csv("Desktop/Vaccine Research Project/iz_kindergarten2016-17_to_2018-19_school_year.csv")


# Replace "Name_Column" with the actual name of the column you're interested in

# Filter the data to include only rows where the specified name is in the specified column
filteredVC <- schoolkey %>% filter((COUNTY == "VENTURA") & (PUBLIC_PRIVATE == "PUBLIC") & (SCHOOL_YEAR == "2018-2019") & (CATEGORY == "MMR"))

# Print or use the filtered data as needed
print(filteredVC)




# Specify the common column on which to merge (adjust column names accordingly)

addcounty <- "SCHOOL_NAME"

# Merge the columns from csv1 to csv2 based on the common column

vcKVC$COUNTY <- filteredVC$COUNTY[match(vcKVC$SCHOOL_NAME, filteredVC$SCHOOL_NAME)]


# Specify the common column on which to merge (adjust column names accordingly)

addcity <- "SCHOOL_NAME"

# Merge the columns from csv1 to csv2 based on the common column

vcKVC$CITY <- filteredVC$CITY[match(vcKVC$SCHOOL_NAME, filteredVC$SCHOOL_NAME)]


View(vcKVC)




# Sum up counts by city
citycount <- vcKVC %>%
  group_by(CITY) %>%
  summarise(Total_Count = sum(COUNT))


citycount <- citycount %>%
  arrange(Total_Count) %>%
  mutate(CITY = factor(CITY, levels = CITY))

print(citycount)

citymean <- vcKVC %>%
  group_by(CITY) %>%
  summarise(Mean_Percent = mean(PERCENT, na.rm = TRUE))



citymean <- citymean %>%
  arrange(Mean_Percent) %>%
  mutate(CITY = factor(CITY, levels = CITY))

print(citymean)


mc <- "CITY"

# Merge the columns from csv1 to csv2 based on the common column

citycount$Mean_Percent <- citymean$Mean_Percent[match(citycount$CITY, citymean$CITY)]

print(citycount)

# Create a table with the gt() function
my_table <- citycount %>%
  gt()

# Add column spanners
my_table <- my_table %>%
  tab_spanner(
    label = "Summary Information",
    columns = c(Total_Count,Mean_Percent)
  )

# Print the table
print(my_table)




# Create a color palette using RColorBrewer

palette_name <- "Set3"
num_colors <- 20


custom_palette <- colorRampPalette(brewer.pal(n = 8, name = palette_name))(num_colors)


plot_ly(citycount, x = ~CITY, y = ~Total_Count, type = 'bar', name = 'Total Count',marker = list(color = custom_palette)) %>%
  layout(title = "City Vaccination Count Totals",
         xaxis = list(title = "City"),
         yaxis = list(title = "Total Kindergartners Vaccinated"))

plot_ly(citymean, x = ~CITY, y = ~Mean_Percent, type = 'bar', name = 'Mean Percent', marker = list(color = custom_palette)) %>%
  layout(title = "Mean Kinder Vaccination Rates by City",
         xaxis = list(title = "City"),
         yaxis = list(title = "Mean Kinder Vaccination Rates", range = c(85, 101)))

         









vpop <- get_acs(geography = "place",
                                    variables = "B01003_001",
                                    state = "CA",
                                    county = "Ventura",
                                    geometry = TRUE)
# Filter and process data
vpop <- vpop %>%
  filter(!grepl("balance", NAME)) %>%
  select(NAME, estimate = B01003_001) %>%
  arrange(desc(estimate))











library(readr)
schoolMaster <- read_csv("Desktop/Vaccine Research Project/iz_kindergarten2016-17_to_2018-19_school_year.csv")
options(tigris_use_cache = TRUE)


merge(df1,df2,by=c('transect_id','year'),all.x=T)


