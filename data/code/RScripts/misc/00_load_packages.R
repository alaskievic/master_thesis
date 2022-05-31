# Options
options(digits=10)        # number of digits to show
options(scipen=999)       # disable scientific notation
memory.limit(size=50000)  # allocate more RAM
set.seed(42)              # set seed for replications
options(stringsAsFactors = FALSE)


# Install packages
if (!require("pacman")) install.packages("pacman")

list.of.packages <- c("rgdal", "sf", "ggplot2", "data.table", "tidyverse", "readxl", 
                      "tmap", "fabricatr", "raster", "rgeos", "mapview", "leaflet",
                      "RColorBrewer", "broom", "sp", "ggthemes", "viridis", "grid", "broom",
                      "viridis", "readstata13", "lubridate", "plm", "haven", "RColorBrewer",
                      "ineq", "writexl", "xlsx", "foreign", "knitr", "stargazer", "glue",
                      "zoo", "stringi", "devtools", "Rdpack", "installr", "magrittr", 
                      "janitor", "ggpmisc", "ipumsr", "survey", "srvyr", "DBI", 
                      "bigrquery", "here", "terra", "geobr", "crul")


# In the first time running, turn install = TRUE
pacman::p_load(list.of.packages, character.only = TRUE, install = FALSE)

#Updates packages
update.packages(ask = FALSE)

# Defining Palette
c25 <- c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)



#Updates R
#updateR()





