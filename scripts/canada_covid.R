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
  mutate(date = mdy(date)) 

dcc <- death %>%
  filter(`Country/Region` == "Canada") %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "death_counts") %>%
  mutate(date = mdy(date)) 


rcc <- recovered %>%
  filter(`Country/Region` == "Canada") %>%
  select(-one_of(drop_columns)) %>%
  pivot_longer(-`Province/State`, names_to = "date", values_to = "recovered_counts") %>%
  mutate(date = mdy(date)) 


combinedc <- cbind(ccc, dcc, rcc, by = "date") 

combinedc_cleanup <- combinedc[,c(1:3, 6, 9)]


combinedc_cleanup <- combinedc_cleanup %>%
  filter(!`Province/State` == "Grand Princess") %>%
  mutate(death_counts = case_when(death_counts == "1" ~ "Yes",
                                  TRUE ~ "No")) %>%
  mutate(week_in_2020 = week(date)) %>%
  rename(Province = `Province/State`)

#-----------------------------------------------------------

# visualize combined data
combinedc_cleanup %>%
  filter(!Province == "Grand Princess") %>%
  ggplot(aes(x = date, y = confirmed_counts, fill = factor(death_counts))) + 
  geom_point(aes(), 
             shape = 23, alpha = 0.5) +
  scale_fill_manual(values = c("azure3", "magenta4")) +
  labs(title="COVID-19 observations in Canada until March 16, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin",
       x="Time", y = "Count",
       fill = "Deaths?", size = "Recovered counts") +
  facet_wrap(~Province)

ggsave("covid19_canada.png")

#-----------------------------------------------------------

# extract lat/longs
latts <- confirmed %>%
  distinct(`Country/Region`, .keep_all = TRUE) %>%
  select(`Country/Region`, Lat, Long) %>%
  rename(country = `Country/Region`)


# cleanup global data
canada_confirmed <- combinedc_cleanup %>%
  pivot_longer(-Province, names_to = "date", values_to = "confirmed_counts") %>%
  mutate(date = mdy(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) 

cnd_cleaned <- canada_confirmed %>%
  group_by(Province, year) %>%
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
  labs(title="Global Coronavirus confirmed cases as of March 16, 2020", 
       subtitle = "Date source: CSSEGISandData/COVID-19",
       caption = "@nabavinoushin")

