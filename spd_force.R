library(leaflet)
library(sf)
library(sp)
library(dplyr)
library(rmapshaper)
library(caret)
library(RColorBrewer)
library(htmltools)

############# Data reading and transformations #################

#Reading in Shapefile with polygons representing each police beat
beats = st_read("SPD_Beats_WGS84.shp")
beats = st_as_sf(beats)
beats_sp = as(beats, 'Spatial')

#A third of the use of force data doesn't have an accurate value for beat, so dissolving beats into sectors
sector = ms_dissolve(beats, field = 'sector')
sector = sector[c(-1),]
rownames(sector) = NULL

#Reading in the use of force data
data = read.csv("force_2019.csv", stringsAsFactors = F)
#Separating the first letter of sector to use for the join
data$New_sector = substring(data$Sector,1,1)

#creating dummy vars for race
dummied <- cbind(data, predict(dummyVars(~Subject_Race, data=data), data, sep="_"))

#Summing up race totals by sector
counts = dummied %>% group_by(New_sector) %>% summarise_at(.vars = names(.[18:23]), sum)

#merging summaries with geographic data
merge = merge(sector, counts, by.x = 'sector', by.y = 'New_sector')

#Adding a summary of overall counts and merging
total = dummied %>% group_by(New_sector) %>% summarise(count = sum(count))
merge = merge(merge, total, by.x = 'sector', by.y = 'New_sector')

#changing to SF
merge_sp = st_as_sf(merge)

################### Making the map ############################

#Setting color palettes
pal <- colorNumeric("Oranges", domain = merge_sp$count)
pal2 <- colorNumeric("Greens", domain = merge_sp$Subject_RaceAsian)
pal3 <- colorNumeric("Reds", domain = merge_sp$`Subject_RaceBlack or African American`)
pal4 <- colorNumeric("Blues", domain = merge_sp$`Subject_RaceHispanic or Latino`)

#setting up labels
label1 <- sprintf(
  "<strong>Sector %s</strong><br/>%g uses of force",
  merge_sp$sector, merge_sp$count
) %>% lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>Sector %s</strong><br/>%g uses of force",
  merge_sp$sector, merge_sp$`Subject_RaceBlack or African American`
) %>% lapply(htmltools::HTML)

label3 <- sprintf(
  "<strong>Sector %s</strong><br/>%g uses of force",
  merge_sp$sector, merge_sp$Subject_RaceAsian
) %>% lapply(htmltools::HTML)

label4 <- sprintf(
  "<strong>Sector %s</strong><br/>%g uses of force",
  merge_sp$sector, merge_sp$`Subject_RaceHispanic or Latino`
) %>% lapply(htmltools::HTML)

#The map
leaflet(data = merge_sp) %>% 
  addProviderTiles('CartoDB.DarkMatter') %>% 
  
  #Total
  addPolygons(
  fillColor = ~pal(merge_sp$count),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = .8,
  highlight = highlightOptions(
    weight = 2,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = label1, group = 'total') %>%
  
  #Black
  addPolygons(
    fillColor = ~pal3(merge_sp$`Subject_RaceBlack or African American`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = label2,
    group = 'Black') %>%
  
  #Asian
  addPolygons(
    fillColor = ~pal2(merge_sp$Subject_RaceAsian),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = label3,
    group = 'Asian') %>%
  
  #Hispanic
  addPolygons(
    fillColor = ~pal4(merge_sp$`Subject_RaceHispanic or Latino`),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.8,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = label4,
    group = 'Hispanic') %>%
  
  #Adding Menu
  addLayersControl(baseGroups = c('total','Black','Asian','Hispanic'),
                   options = layersControlOptions(collapsed = FALSE))









