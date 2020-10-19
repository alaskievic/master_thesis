# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")



######### 1. Read and explore FAO-GAEZ dataset ################################################################################################

maize_low = raster("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/maize_low.asc")
soy_low = raster("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/soybean_low.asc")

#Plotting the raw data
plot(maize_low, main = "Potential Yields for Maize - Low Inputs")
plot(soy_low, main = "Potential Yields for Soy - Low Inputs")


#Reads shapefile for state borders
estados_shp <- readOGR("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/uf_2019", "BR_UF_2019", stringsAsFactors = F)

plot(estados_shp,
     main = "Brazillian State Borders",
     axes = TRUE,
     border = "blue")


#Plot raster file on top of state borders

##Cuts the raster file to match the states coordinates
aux <-  crop(maize_low, estados_shp)

plot(aux, main = "Maize Potential Yields")

plot(estados_shp, add = TRUE)


#Reads shapefile for municipalities borders
shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)

##Plot raster file on top of municipalities borders (demora alguns minutinhos)
plot(aux, main = "Maize Potential Yields")
plot(shp_ibge, add = TRUE)


#Uses velox package to compute the mean and sum value for each pixel inside each municipality
options(buildtools.check=NULL)

library(devtools)
install_github("hunzikp/velox")

library(velox)
maize_low_vx <- velox("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/maize_low.asc")
soy_low_vx <- velox("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/soybean_low.asc")

teste1 <- maize_low_vx$extract(shp_ibge, fun = mean)
teste2 <- maize_low_vx$extract(shp_ibge, fun = sum)

teste3 <- soy_low_vx$extract(shp_ibge, fun = mean)
teste4 <- soy_low_vx$extract(shp_ibge, fun = sum)


