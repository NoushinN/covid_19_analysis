# declare dependencies
if (!exists("setup_sourced")) source(here::here("scripts", "setup.R"))

#-----------------------------------------------------------

# load confirmed cases
confirmed <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Confirmed.csv")) 
death <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Deaths.csv"))
recovered <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Recovered.csv"))

#-----------------------------------------------------------

# make a dataframe for all of Canada
drop_columns <- c("Country/Region", "Lat", "Long")

ccc <- confirmed %>%
  filter(`Country/Region` == "Canada") %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "confirmed_counts") %>%
  mutate(date = mdy(date)) %>%
  rename(Province = `Province/State`)


dcc <- death %>%
  filter(`Country/Region` == "Canada") %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "death_counts") %>%
  mutate(date = mdy(date)) %>%
  rename(Province = `Province/State`)


rcc <- recovered %>%
  filter(`Country/Region` == "Canada") %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "recovered_counts") %>%
  mutate(date = mdy(date)) %>%
  rename(Province = `Province/State`)


combinedc <- cbind(ccc, dcc, rcc, by = "date") 

combinedc_cleanup <- combinedc[,c(1:3, 6, 9)]

# make data to visualize
combinedc_cleanups <- combinedc_cleanup %>%
  filter(!Province == "Grand Princess") %>%
  mutate(death_counts = case_when(death_counts == "1" ~ "Yes",
                                  TRUE ~ "No")) %>%
  mutate(week_in_2020 = week(date)) 

#-----------------------------------------------------------

# visualize combined data
combinedc_cleanups %>%
  ggplot(aes(x = date, y = confirmed_counts, fill = factor(death_counts))) + 
  geom_point(aes(), 
             shape = 23, alpha = 0.5) +
  scale_fill_manual(values = c("azure3", "magenta4")) +
  labs(title="COVID-19 observations in Canada until March 21, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin",
       x="Time", y = "Count",
       fill = "Deaths?", size = "Recovered counts") +
  facet_wrap(~Province)

ggsave("covid19_canada.png")

#-----------------------------------------------------------

# Canada's data 
canada <- combinedc_cleanup %>%
  mutate(week_in_2020 = week(date)) %>%
  mutate(Province = as.factor(Province))

canada$Province[119:177] <-"Alberta"                                            
canada$Province[1:59] <-"British Columbia / Colombie-Britannique"            
canada$Province[296:354] <-"Manitoba"                                           
canada$Province[237:295] <-"New Brunswick / Nouveau-Brunswick"                  
canada$Province[473:531] <-"Newfoundland and Labrador / Terre-Neuve-et-Labrador"
canada$Province[591:649] <-"Nova Scotia / Nouvelle-Écosse"                      
canada$Province[60:118] <-"Ontario"                                            
canada$Province[178:236] <-"Quebec / Québec"                                    
canada$Province[355:413] <-"Saskatchewan"                                       


library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(sf)
library(rgdal)
library(leaflet)
library(mapview)

## set the map colors:
pal_ct <- colorRampPalette(RColorBrewer::brewer.pal(9, "BrBG"))
pal_cd <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrBr"))
pal_csd <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set3"))
pal_fsa <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))

# download shp files from statscan
## https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm
## e.g. for shp files for census tracts, download: https://www12.statcan.gc.ca/census-recensement/alternative_alternatif.cfm?l=eng&dispext=zip&teng=lct_000a16a_e.zip&k=%20%20%20%20%207190&loc=http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lct_000a16a_e.zip

# set the path for files
path <- file.path(here::here("polygons", "lct_000a16a_e"))
path

# plotting Canada:

canada_map <- sf::st_read("/Users/noushinnabavi/covid_19_analysis/polygons/lct_000a16a_e/lct_000a16a_e.shp") %>%
  rename(Province = PRNAME)

class(canada_map)
plot(canada_map)

x <- factor(canada_map$Province)
levels(x)

# plot/map canada
canada_geom <- inner_join(canada, canada_map, by = "Province") %>%
  st_sf()

class(canada_geom)
plot(canada_geom)

# use ggplot to plot data
ggplot(canada_geom) +
  geom_sf(aes(fill = confirmed_counts)) + 
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#-----------------------------------------------------------

# Map covid cases on Canada map
# If the .shp files (provinces) aren't already downloaded on your system, this command downloads them
library(leaflet)
if (!file.exists("./polygons/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/50m/cultural',
                          'ne_50m_admin_1_states_provinces_lakes.zip'),
                f <- tempfile())
  unzip(f, exdir = "./polygons/ne_50m_admin_1_states_provinces_lakes")
  rm(f)
}

# Read the .shp files
provinces <- rgdal::readOGR("./polygons/ne_50m_admin_1_states_provinces_lakes", 'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

# Canada's data 
canada <- combinedc_cleanup %>%
  mutate(week_in_2020 = week(date)) 
canada$Province[178:236] <-"Québec"

# combine to map
provinces2  <- sp::merge(
  provinces,
  canada,
  by.x = "name",
  by.y = "Province",
  sort = FALSE,
  incomparables = NULL,
  duplicateGeoms = TRUE
)


clear <- "#F2EFE9"
lineColor <- "#000000"
hoverColor <- "red"
lineWeight <- 0.5

pal <- leaflet::colorNumeric(palette = 'Purples', c(max(canada$confirmed_counts), min(canada$confirmed_counts)), reverse = FALSE)
pal2 <- leaflet::colorNumeric(palette = 'Blues', c(max(canada$death_counts), min(canada$death_counts)), reverse = FALSE)
pal3 <- leaflet::colorNumeric(palette = 'Reds', c(max(canada$recovered_counts), min(canada$recovered_counts)), reverse = FALSE)


provinces2 %>%
  leaflet() %>%
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 3, maxZoom = 3,
                                   dragging = FALSE)) %>%
  addTiles() %>%
  setView(-110.09, 62.7,  zoom = 3) %>%
  
  addPolygons(data = subset(provinces2, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Québec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut")),
              fillColor = ~ pal(confirmed_counts),
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = lineWeight,
              color = lineColor,
              highlightOptions = highlightOptions(fillOpacity = 1, bringToFront = TRUE, sendToBack = TRUE),
              label=~stringr::str_c(
                name,' ',
                formatC(confirmed_counts)),
              labelOptions= labelOptions(direction = 'auto'),
              group = "confirmed") %>%
  
  addPolygons(data = subset(provinces2, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Québec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut")),
              fillColor = ~ pal2(death_counts),
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = lineWeight,
              color = lineColor,
              highlightOptions = highlightOptions(fillOpacity = 1, bringToFront = TRUE, sendToBack = TRUE),
              label=~stringr::str_c(
                name,' ',
                formatC(death_counts)),
              labelOptions= labelOptions(direction = 'auto'),
              group = "death") %>%
  
  addPolygons(data = subset(provinces2, name %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Québec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador", "Yukon", "Northwest Territories", "Nunavut")),
              fillColor = ~ pal3(recovered_counts),
              fillOpacity = 0.5,
              stroke = TRUE,
              weight = lineWeight,
              color = lineColor,
              highlightOptions = highlightOptions(fillOpacity = 1, bringToFront = TRUE, sendToBack = TRUE),
              label = ~ stringr::str_c(
                name, ' ',
                formatC(recovered_counts)),
              labelOptions= labelOptions(direction = 'auto'),
              group = "recovered") %>%
  
  addLayersControl(overlayGroups = c("confirmed", "death", "recovered"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  addLegend(pal = pal,
            values = canada$confirmed_counts,
            position = "bottomleft",
            title = "COVID-19 in Canada",
            labFormat = labelFormat(suffix = "", transform = function(x) sort(x, decreasing = FALSE)))
  

