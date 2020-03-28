# declare dependencies
if (!exists("setup_sourced")) source(here::here("scripts", "setup.R"))

#-----------------------------------------------------------
# to scrape out the case data in R in case anyone is interested in graphing these:
require(dplyr,httr)
canada_data <- content(GET("https://covid19tracker.ca/dist/api/controller/cases.php"))$individualCases %>% lapply(as_tibble) %>% bind_rows


canada_data_tidy <- 
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


ggplot <- rg$ggplotTopCountriesStackedBarDailyInc(included.countries = latam.countries,
                                                  map.region = "Latam")
ggsave(file.path(data.dir, paste("latam-daily-increment-", dataviz.date, ".png", sep ="")), ggplot,
       width = 7, height = 5, dpi = 300)       
