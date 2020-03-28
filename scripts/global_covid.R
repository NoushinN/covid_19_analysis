# declare dependencies
if (!exists("setup_sourced")) source(here::here("scripts", "setup.R"))

#-----------------------------------------------------------

# load confirmed cases
confirmed <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_confirmed_global.csv")) 
death <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_deaths_global.csv"))
recovered <- fread(here::here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_covid19_recovered_global.csv"))

#-----------------------------------------------------------

# make a dataframe for all of Canada
drop_columns <- c("Province/State", "Lat", "Long")

ccc <- confirmed %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Country/Region`, names_to = "date", values_to = "confirmed_counts") %>%
  mutate(date = mdy(date)) %>%
  rename(Country = `Country/Region`) %>%
  filter(date == "2020-03-07") %>%
  distinct(Country, .keep_all = TRUE) %>%
  top_n(10, confirmed_counts)

# Visualize top 10 countries with confirmed cases
options(scipen=999)

ggplot(ccc, aes(x=Country, y=confirmed_counts)) + 
  geom_point(aes(col=Country, size=confirmed_counts)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Count of confirmed cases", 
       y="Countries", 
       x="Counts", 
       title="Data release on March 07, 2020", 
       caption = "Source: JHU CSSE")

#-----------------------------------------------------------
# extract lat/longs
latts <- confirmed %>%
  distinct(`Country/Region`, .keep_all = TRUE) %>%
  select(`Country/Region`, Lat, Long) %>%
  rename(country = `Country/Region`)


# cleanup global data
drop_columns <- c("Province/State", "Lat", "Long")

global_confirmed <- confirmed %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Country/Region`, names_to = "date", values_to = "confirmed_counts") %>%
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  rename(country = `Country/Region`)

gobal_cleaned <- global_confirmed %>%
  group_by(country, year) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) 

# make data frame for spatial analysis

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
glimpse(world)

# convert names so they match
global_latts <- inner_join(gobal_cleaned, latts, by = "country")
global_latts$country[1] <-"United States"


# make sf data frame
global_latts_geom <- global_latts %>%
  rename(name = country) %>%
  left_join(world, by = "name") %>%
  select(name, year, count, Lat, Long, geometry) %>%
  ungroup()

global_sf <- sf::st_as_sf(global_latts_geom)
class(global_sf)


#-----------------------------------------------------------
# load spatial packages
library(sf)
library(mapview)

mapview(as(global_sf, "Spatial"))


ggplot(data = global_sf) +
  geom_sf(aes(fill = count)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(title="Global Coronavirus confirmed cases as of March 21, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin")

ggsave("world_map.pdf")



