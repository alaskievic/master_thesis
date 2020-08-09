# Options
options(digits=10)   # number of digits to show
options(scipen=999) # disable scientific notation
options(stringsAsFactors = FALSE)
memory.limit(size=50000) # allocate more RAM
set.seed(42) # set seed for replications


# Install packages
list.of.packages <- c("rgdal", "sf", "ggplot2", "data.table", "tidyverse", "readxl", 
                      "tmap", "fabricatr", "raster", "rgeos", "mapview", "leaflet",
                      "RColorBrewer", "broom", "sp", "ggthemes", "viridis", "grid", "broom",
                      "viridis", "readstata13", "lubridate", "plm", "haven", "RColorBrewer",
                      "ineq", "writexl", "xlsx", "foreign", "knitr", "stargazer", "glue")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Load packaages

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
library(plm)
library(data.table)
library(haven)
library(xlsx)
library(foreign)
library(writexl)
library(ineq)
library(knitr)
library(stargazer)
library(glue) #will try this later




