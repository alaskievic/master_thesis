# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


data_gini_land_stata <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/agro_2006.dta")




data_gini_land <- pivot_wider(data_gini_land_stata, -c("cod", "group", "num", "area", "mun_group", "num_tot"), values_from = "gini", names_from = "municip")



shp_ibge_gini_land <- merge(shp_ibge@data, data_gini_land, all.x = TRUE)
