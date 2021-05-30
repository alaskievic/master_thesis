# Options
options(digits=10)   # number of digits to show
options(scipen=999) # disable scientific notation
options(stringsAsFactors = FALSE)
memory.limit(size=50000) # allocate more RAM
set.seed(42) # set seed for replications


#Updates packages
update.packages(ask = FALSE)


# Install packages
list.of.packages <- c("rgdal", "sf", "ggplot2", "data.table", "tidyverse", "readxl", 
                      "tmap", "fabricatr", "raster", "rgeos", "mapview", "leaflet",
                      "RColorBrewer", "broom", "sp", "ggthemes", "viridis", "grid", "broom",
                      "viridis", "readstata13", "lubridate", "plm", "haven", "RColorBrewer",
                      "ineq", "writexl", "xlsx", "foreign", "knitr", "stargazer", "glue",
                      "zoo", "stringi", "magick", "devtools", "Rdpack", "installr", "magrittr", 
                      "janitor", "ggpmisc", "ipumsr", "survey", "srvyr", "DBI", 
                      "bigrquery", "here", "")

# Use install = TURN if it is the first time running the code
pacman::p_load(list.of.packages, character.only = TRUE, install = FALSE)

