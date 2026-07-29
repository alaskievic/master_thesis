# Set Working Directory
setwd("C:/Users/Andrei/Desktop//Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")



#################### 1. Measure Map for Microregion ############################


# Reads shapefiles for microregion borders
shp_micro <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Analysis/Shapefiles/br_microreg", "BR_Microrregioes_2020", stringsAsFactors = F)

# Reads shapefiles for state borders
shp_ufs <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Analysis/Shapefiles/uf_2019", "BR_UF_2019", stringsAsFactors = F)


# Read aggregate file from Stata
pop_struc_micro <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc_micro.dta")

# Renaming
names(shp_micro@data)[1] = "codmicro"
shp_micro@data$codmicro <- as.integer(shp_micro@data$codmicro)


# Merging
shp_micro_pop <- merge(shp_micro, pop_struc_micro, all.x = TRUE)


# Plotting only the borders
micro_borders <- tm_shape(shp_micro) +
  tm_polygons(col = NA) +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

micro_borders



# Plotting Measures (No Cattle)
map_micro_dfao <- tm_shape(shp_micro_pop) +
  tm_polygons(col = "dfao", style = "quantile", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Difference in Predicted\nExposure 2000-2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

map_micro_dfao

# Saving
tmap_save(map_micro_dfao, "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures/microreg_fao.png")





# Plotting Measures (With Cattle)
map_micro_dfaoc <- tm_shape(shp_micro_pop) +
  tm_polygons(col = "dfaoc95", style = "quantile", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Difference in Predicted\nExposure 2000-2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

map_micro_dfaoc


# Saving
tmap_save(map_micro_dfaoc, "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures/microreg_faoc.png")








#install.packages("devtools")
#devtools::install_github("lucasmation/microdadosBrasil")

library('microdadosBrasil')




# Censo Demográfico 2000
download_sourceData("CENSO", 2000, unzip = T)
d <- read_CENSO('domicilios', 2000)



d <- read_CENSO('pessoas', 2000)


### Graphs

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/fao_pr.dta")


# Banana
banana <- ggplot(data = fao_pr, aes(x = pr_banana, y = banana)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

banana


soybean <- ggplot(data = fao_pr, aes(x = pr_soybean, y = soybean)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

soybean


# Full measure
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/fao_final.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pq_bartik_final.Rdata")


library(unikn)

# (a) Vector of colors (as RGB values):
o_i_colors <- c(rgb(  0,   0,   0, maxColorValue = 255),  # black
                rgb(230, 159,   0, maxColorValue = 255),  # orange
                rgb( 86, 180, 233, maxColorValue = 255),  # skyblue
                rgb(  0, 158, 115, maxColorValue = 255),  # green
                rgb(240, 228,  66, maxColorValue = 255),  # yellow
                rgb(  0, 114, 178, maxColorValue = 255),  # blue
                rgb(213,  94,   0, maxColorValue = 255),  # vermillion
                rgb(204, 121, 167, maxColorValue = 255)   # purple
)

# (b) Vector of color names:
o_i_names <- c("black", "orange", "skyblue", "green", "yellow", "blue", "vermillion", "purple")

# (c) Use newpal() to combine colors and names:
pal_okabe_ito <- newpal(col = o_i_colors,
                        names = o_i_names)




scatter <- full_join(fao_final, pq_bartik_final, by = c("cod", "year"))



fao_scatter <- ggplot(data = scatter, aes(x = log(sum_fao), y = log(pq_shares))) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  theme_bw()

fao_scatter


fao_scatter <- ggplot(data = scatter, aes(x = sum_fao, y = pq_shares)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  theme_bw()

fao_scatter








