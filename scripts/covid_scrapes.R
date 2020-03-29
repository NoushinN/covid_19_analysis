# declare dependencies
if (!exists("setup_sourced")) source(here::here("scripts", "setup.R"))

#-----------------------------------------------------------
# to scrape out the case data in R in case anyone is interested in graphing these:
require(dplyr,httr)
canada_data <- content(GET("https://covid19tracker.ca/dist/api/controller/cases.php"))$individualCases %>% lapply(as_tibble) %>% bind_rows


canada_data_tidy <- canada_data %>%
  mutate(date = as.Date(date)) %>%
  filter(province == "British Columbia") %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  mutate(sum_to_date = sum(count))
  

#-----------------------------------------------------------

# install package
devtools::install_github("kenarab/COVID19", build_opts = NULL)

# load dependent packages
library(COVID19)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(knitr)
library(ggplot2)
library(gridExtra)
library(magrittr)

#-----------------------------------------------------------

# COVID19 package is still not functional
data.processor <- COVID19DataProcessor$new(force.download = FALSE)
data.processor$curate()

rg <- ReportGeneratorEnhanced$new(data.processor)


rc <- ReportGeneratorDataComparison$new(data.processor = data.processor)

top.countries <- data.processor$top.countries
international.countries <- unique(c(data.processor$top.countries,
                                    "Japan", "Singapur", "Hong Kong"))

latam.countries <- sort(c("Mexico",
                          data.processor$countries$getCountries(division = "sub.continent", name = "Caribbean"),
                          data.processor$countries$getCountries(division = "sub.continent", name = "Central America"),
                          data.processor$countries$getCountries(division = "sub.continent", name = "South America")))



rg$ggplotTopCountriesStackedBarDailyInc(included.countries = latam.countries,
                                                  map.region = "Latam")

#-----------------------------------------------------------

# Comparation by epidemy day
countries.plot <- unique(c(data.processor$top.countries,
                           "Japan", "Singapur", "Hong Kong",
                           data.processor$countries$getCountries(division = "sub.continent", name = "South America")))

rc$ggplotComparisonExponentialGrowth(included.countries = countries.plot, min.cases = 20)

#-----------------------------------------------------------
latam.countries <- c("Mexico",
                     data.processor$countries$getCountries(division = "sub.continent", name = "Caribbean"),
                     data.processor$countries$getCountries(division = "sub.continent", name = "Central America"),
                     data.processor$countries$getCountries(division = "sub.continent", name = "South America"))


# other plots
rc$ggplotComparisonExponentialGrowth(included.countries = latam.countries, min.cases = 20)
rg$ggplotTopCountriesLines(field = "confirmed.inc", log.scale = TRUE)
rg$ggplotTopCountriesLines(field = "rate.inc.daily", log.scale = FALSE)
rg$ggplotTopCountriesPie()
rg$ggplotTopCountriesBarPlots()


