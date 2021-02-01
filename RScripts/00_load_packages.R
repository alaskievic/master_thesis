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
                      "bigrquery", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, type = "source", dependencies = TRUE)


# Load packages
library(rgdal)
library(sf)
library(tidyverse)
library(readxl)
library(tmap)
library(fabricatr)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(RColorBrewer)
library(sp)
library(ggthemes)
library(viridis)
library(readstata13)
library(lubridate)
library(data.table)
library(haven)
library(foreign)
library(writexl)
library(ineq)
library(knitr)
library(stargazer)
library(glue)
library(plm)
library(xlsx)
library(zoo)
library(stringi)
library(magick)
library(devtools)
library(installr)
library(magrittr)
library(janitor)
library(ggpmisc)
library(ipumsr)
library(survey)
library(srvyr)
library(DBI)
library(bigrquery)
library(here)

#Updates R

#updateR()