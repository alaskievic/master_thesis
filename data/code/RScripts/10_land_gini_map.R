#Load packages
source("./00_load_packages.R")

######### 1. Reads and plots Land Gini Inequality data calculated in Stata #####


### 1920 ###

# 1920 Shapefile
shp_1920 <- st_read(here("data", "raw", "shapefiles", "municip_1920",
                         "05-malha_municipal_1920.shp"))

# Plotting Borders
state_1920 <- st_read(here("data", "raw", "shapefiles", "state_1920",
                           "04-limite estadual 1920.shp"))

# plot(st_geometry(shp_1920))
# plot(st_geometry(state_1920))


# Reads from Stata
land_ineq_1920 <- read_dta(here("data", "raw", "historical", "land_gini_1920.dta")) %>%
  rename(codigo = code2010)

# Merging with IBGE data
shp_ibge_gini_1920 <- full_join(shp_1920, land_ineq_1920, by = "codigo")

# Just to analyze the average
mean(shp_ibge_gini_1920$land_gini_1920, na.rm = TRUE)


shp_ibge_gini_1920 %<>% filter(!is.na(land_gini_1920)) %>% filter(!is.na(codigo)) %>% 
  filter(!is.na(nome))


# Plotting
map_land_1920 <- tm_shape(shp_ibge_gini_1920) +
  tm_polygons(col = "land_gini_1920", style = "fisher", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini in 1920") +
  tm_shape(state_1920) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 


map_land_1920


# Saving
tmap_save(map_land_1920, here("analysis", "output", "figures",
                              "gini_land_2017.png"))





# Reads IBGE's 2019 shapefile containing 5570 municipalities
shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)

# Reads shapefiles for state borders
shp_ufs <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/uf_2019", "BR_UF_2019", stringsAsFactors = F)


#### 1995

# Reads from Stata
stata_agro_1995 <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/agro_1995.dta")

# Get unique values for each municipality
gini_land_1995 <- dplyr::select(stata_agro_1995, c("cod", "municip", "gini_1995_corr")) %>%
  distinct(gini_1995_corr, .keep_all = TRUE) %>%
  rename(gini_1995 = "gini_1995_corr")

# Restrain 1 negative value
gini_land_1995 <- gini_land_1995 %>% mutate(gini_1995 = if_else(gini_1995 < 0, 0, gini_1995))

# Merging with IBGE data
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_1995 <- merge(shp_ibge, gini_land_1995, all.x = TRUE)

# Just to analyze the average
mean(gini_land_1995$gini_1995)

# Plotting
map_land_1995 <- tm_shape(shp_ibge_gini_1995) +
  tm_polygons(col = "gini_1995",  style = "fisher", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini in 1995") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

map_land_1995


# Saving
tmap_save(map_land_1995, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_land_1995.png")


tmap_save(map_land_1995, "C:/Users/Andrei/Desktop/teste.png")




### 2006 ###

# Reads from Stata
stata_agro_2006 <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/agro_2006.dta")

# Get unique values for each municipality
gini_land_2006 <- dplyr::select(stata_agro_2006, c("cod", "municip", "gini_2006_corr")) %>%
  distinct(gini_2006_corr, .keep_all = TRUE) %>%
  rename(gini_2006 = "gini_2006_corr")

# Take out possible negative values
gini_land_2006 <- gini_land_2006 %>% mutate(gini_2006 = if_else(gini_2006 < 0, 0, gini_2006))

# Merging with IBGE data
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_2006 <- merge(shp_ibge, gini_land_2006, all.x = TRUE)

# Just to analyze the average
mean(gini_land_2006$gini_2006)

# Plotting
map_land_2006 <- tm_shape(shp_ibge_gini_2006) +
  tm_polygons(col = "gini_2006",  style = "fisher", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini in 2006") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_land_2006

# Saving
tmap_save(map_land_2006, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_land_2006.png")


######## 2017



# Reads from Stata
stata_agro_2017 <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/agro_2017.dta")

# Get unique values for each municipality
gini_land_2017 <- dplyr::select(stata_agro_2017, c("cod", "municip", "gini_2017_corr")) %>%
  distinct(gini_2017_corr, .keep_all = TRUE) %>%
  rename(gini_2017 = "gini_2017_corr")

# Take out possible negative values
gini_land_2017 <- gini_land_2017 %>% mutate(gini_2017 = if_else(gini_2017 < 0, 0, gini_2017))

# Merging with IBGE data
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_2017 <- merge(shp_ibge, gini_land_2017, all.x = TRUE)

# Just to analyze the average
mean(gini_land_2017$gini_2017)

# Plotting
map_land_2017 <- tm_shape(shp_ibge_gini_2017) +
  tm_polygons(col = "gini_2017", style = "fisher", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini in 2017") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 



map_land_2017

# Saving
tmap_save(map_land_2017, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_land_2017.png")



# Saving merged dataset
tmp <- full_join(dplyr::select(gini_land_1995, -"municip"), gini_land_2006, by = "cod")
land_gini <- full_join(dplyr::select(tmp, -"municip"), gini_land_2017, by = "cod")

land_gini <- land_gini %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "gini_land", names_prefix = "gini_") %>%
  mutate(year = as.integer(year))

#Saving
save(land_gini, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/land_gini.RData")




######### Making maps for the changes in Land Gini
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/land_gini.RData")

gini_diff <- pivot_wider(land_gini, names_from = "year", values_from = "gini_land")

gini_diff %<>% mutate(diff_1 = `2017` - `2006`) %>%
  mutate(diff_2 = `2006` - `1995`) %>%
  mutate(diff_3 =`2017` - `1995`)

# Merging with IBGE data
names(shp_ibge@data)[1] = "cod"
gini_diff <- merge(shp_ibge, gini_diff, all.x = TRUE)



# Plotting the maps
map_land_diff1 <- tm_shape(gini_diff) +
  tm_polygons(col = "diff_1", style = "fisher", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", midpoint = NA,
              title = "Land Gini Change\n(2017 - 2006)") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_land_diff1

#Saving
tmap_save(map_land_diff1, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_land_diff1.png")




map_land_diff2 <- tm_shape(gini_diff) +
  tm_polygons(col = "diff_2",  style = "fisher", palette = "YlOrRd",
              border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini Change (2006 - 1995)") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_land_diff2

#Saving
save(map_land_diff2, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/land_gini_diff2.RData")




map_land_diff3 <- tm_shape(gini_diff) +
  tm_polygons(col = "diff_3",  style = "fisher", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Land Gini Change (2017 - 1995)") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0,
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_land_diff3

#Saving
save(map_land_diff3, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/land_gini_diff3.RData")







###################### 2. Make maps for Income Gini ############################

###### 1991
# Sets up Gini data for each year
gini_censo_1991 <- dplyr::select(atlas_mun, c("ANO", "Codmun7", "Município", "GINI")) %>%
  filter(ANO == 1991) %>%
  rename(cod = "Codmun7") %>%
  arrange(cod)

# Merge with IBGE shapefile
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_1991 <- merge(shp_ibge, gini_censo_1991, all.x = TRUE)


# Plotting
gini_income_1991 <- tm_shape(shp_ibge_gini_1991) +
  tm_polygons(col = "GINI",  style = "fisher", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 1991") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_1991

# Saving
tmap_save(gini_income_1991, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_income_1991.png")



##### 2000
# Sets up Gini data for each year
gini_censo_2000 <- dplyr::select(atlas_mun, c("ANO", "Codmun7", "Município", "GINI")) %>%
  filter(ANO == 2000) %>%
  rename(cod = "Codmun7") %>%
  arrange(cod)

# Merge with IBGE shapefile
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_2000 <- merge(shp_ibge, gini_censo_2000, all.x = TRUE)


# Plotting
gini_income_2000 <- tm_shape(shp_ibge_gini_2000) +
  tm_polygons(col = "GINI",  style = "fisher", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_2000

# Saving
tmap_save(gini_income_2000, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_income_2000.png")


# 2010
# Sets up Gini data for each year
gini_censo_2010 <- dplyr::select(atlas_mun, c("ANO", "Codmun7", "Município", "GINI")) %>%
  filter(ANO == 2010) %>%
  rename(cod = "Codmun7") %>%
  arrange(cod)

# Merge with IBGE shapefile
names(shp_ibge@data)[1] = "cod"
shp_ibge_gini_2010 <- merge(shp_ibge, gini_censo_2010, all.x = TRUE)


# Plotting
gini_income_2010 <- tm_shape(shp_ibge_gini_2010) +
  tm_polygons(col = "GINI",  style = "fisher", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_2010

# Saving
tmap_save(gini_income_2010, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_income_2010.png")


############### 3. Plotting Income Gini with equal bracket sizes ###############

# Plotting
gini_income_1991_brack <- tm_shape(shp_ibge_gini_1991) +
  tm_polygons(col = "GINI",  style = "fixed", breaks = c(0.27, 0.35, 0.5, 0.65, 0.8, 0.92), palette = "YlOrRd", border.col = "black", 
              border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 1991") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_1991_brack

# Saving
tmap_save(gini_income_1991_brack, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_income_1991_brack.png")


#2000
gini_income_2000_brack <- tm_shape(shp_ibge_gini_2000) +
  tm_polygons(col = "GINI",  style = "fixed", breaks = c(0.30, 0.35, 0.5, 0.65, 0.8, 0.87), palette = "YlOrRd", border.col = "black", 
              border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_2000_brack

tmap_save(gini_income_1991_brack, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/gini_income_1991_brack.png")


gini_income_2010_brack <- tm_shape(shp_ibge_gini_2010) +
  tm_polygons(col = "GINI",  style = "fixed", breaks = c(0.28, 0.35, 0.5, 0.65, 0.8, 0.8), palette = "YlOrRd", border.col = "black", 
              border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Income Gini in 2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

gini_income_2010_brack









