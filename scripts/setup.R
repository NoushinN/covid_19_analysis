# list of libraries
libraries <- c("tidyverse", "data.table", "here", "table1",
               "fpp2", "lubridate")

# load libraries
lapply(libraries, library, character.only = TRUE)

# source the script
.setup_sourced <- TRUE
