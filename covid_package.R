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
