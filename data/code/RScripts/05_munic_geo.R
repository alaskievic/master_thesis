
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

######### 1. Reads Brazilian Municipaalities shapefiles and make some maps with crops data #################################################

# Reads shapefile with 5564 municipalities
shp_5564 <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/municipio5564", "municipio5564", stringsAsFactors = F)

# Reads IBGE's 2019 shapefile containing 5570 municipalities
shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)

# Reads shapefiles for state borders
shp_ufs <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/uf_2019", "BR_UF_2019", stringsAsFactors = F)

# Reads Municipalities GDP data
municip_pib <-read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/municip_pib_IPEA.xls", 
                     sheet = "Séries", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Doing some clean up of the data and focusing on 2000 GDP values
municip_pib_2000 <- municip_pib %>% rename ("cod" = "Codigo", "municip" = "Município", "pib_2000" = "2000") %>%
  dplyr::select(cod, municip, pib_2000) %>%
  arrange(cod)

names(shp_ibge@data)[1] = "cod"

# Using merge funciton which is the recommended way of doing merges inside shapefiles
shp_ibge <- merge(shp_ibge, municip_pib_2000, all.x = TRUE)

# Changing Na values to zero
for(i in 1:length(shp_ibge@data[,6])){if(is.na(shp_ibge@data[i,6])){shp_ibge@data[i,6]=0}}

# Ranking 2000 GDP values in quintiles
municip_pib_2000_rank <- mutate(municip_pib_2000, pcls = cut(100 * percent_rank(pib_2000), seq(0, 100, len = 6),
                                                             include.lowest = TRUE))
# Changing pq dataset a bit to match it to shapefiles
pq_final_wider <- pq_final_wider %>% dplyr::select("cod", "2000") %>% rename("index_2000" = "2000")


# 
shp_ibge_2 <- merge(shp_ibge, pq_final_wider, all.x = TRUE)

for(i in 1:length(shp_ibge_2@data[,7])){if(is.na(shp_ibge_2@data[i,7])){shp_ibge_2@data[i,7]=0}}

shp_ibge_2@data[,8] = shp_ibge_2@data$index_2000/shp_ibge_2@data$pib_2000

shp_ibge_2@data[,9] = cut(100 * percent_rank(shp_ibge_2@data$V8), seq(0, 100, len = 6),
                          include.lowest = TRUE)




### Using tmap package to make the maps
shp_ibge <- merge(shp_ibge, municip_pib_2000_rank, all.x = TRUE)

# Make map of Municipalities GDP in 2000
municip_tm <- tm_shape(shp_ibge) +
  tm_polygons("pcls", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
              title = "Municipalities GDP in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 
  
municip_tm

# Save the map
tmap_save(municip_tm, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_tm.png")


# Make map of the value of bartik instrument over 2000 GDP
municip_tm_2 <- tm_shape(shp_ibge_2) +
  tm_polygons("V9", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
              title = "Crops over GDP in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.75, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 


municip_tm_2

# Saving
tmap_save(municip_tm_2, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_tm_2.png")


# Map in BW scale
municip_tm_3 <- tm_shape(shp_ibge_2) +
  tm_polygons("V9", palette = "Greys", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
              title = "Crops over GDP in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.75, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 


municip_tm_3

# Saving
tmap_save(municip_tm_3, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_tm_2_bw.png")



######### 2. Getting some information about the shape files, specifically about the municipalities that are not matched between shapefiles and datasets #####

# Arranging the dataset
municip_5572 <- as.data.frame(shp_ibge$cod)
municip_5572 <- as_tibble(municip_5572)
municip_5572 <- municip_5572 %>% rename("cod" = "shp_ibge$cod") %>%
  mutate (cod = as.character(cod))

# To illustrate the PAM dataset; cod might have another name like Cód.
banana <- banana %>% rename(cod = Cód.)
banana <- banana %>% filter(cod > 1) %>% ungroup()

#PAM has 5563 observations, meaning that 7 places did not produced any kind of crops in the period.
diff_1 <- anti_join(shp_ibge@data, banana, by = "cod")

# Making a table for the municipalities that are not in PAM
table_na_another <- stargazer(diff_1[, 2][1:7], summary=FALSE, rownames=FALSE)

# Difference between PAM and 2000GDP dataset
codmunicip_diff_pam_2 <- anti_join(municip_pib_2000, banana, by = "cod")


#There are 26 municipalities missing that do not have any observation for their 2000 GDP
municip_diff_pib <- anti_join(municip_pib_2000, municip_5572, by = "cod")

# 5572 territories observations mean that we have 5570 municipalities and two lagoons that are large enough to appear in the map:
# Lagoa dos patos and Lagoa Mirim (thw two largest lagoons in Brazil and Latam). Therefore, the 5570 municipalities dataset is ok to go 
municip_diff_pib2 <- anti_join(municip_5572, municip_pib_2000, by = "cod")








############################ Appendix. Trying ggplot #######################################################################


mapamun <- tidy(shp_ibge, region = "cod") %>%
  rename("cod" = "id")

data_rpib <- left_join(mapamun, municip_pib_2000_rank, "cod")


# Color = NA no borders
plot_1 <- ggplot(data_rpib) +
  geom_polygon(aes(long, lat, group = group, fill = pcls), color = NA) +
  coord_map("bonne", parameters = 41.6) +
  geom_path(data = shp_ufs, color = "grey", size = 0.1, aes(long, lat, group = group)) +
  ggthemes::theme_map() +
  scale_fill_brewer(palette = "Greens", name = "Brazilian Municipalities GDP in 2000", na.value = "white", 
                    labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", "No Data")) +
  theme(legend.background = element_rect(fill = NA))

plot_1


ggplot(data_rpib) +
  geom_polygon(aes(long, lat, group = group, fill = pcls), size = 0.1) +
  coord_map("bonne", parameters = 41.6) +
  geom_path(data = shp_ufs, color = "grey", size = 0.1, aes(long, lat, group = group)) +
  ggthemes::theme_map() +
  scale_fill_brewer(palette = "YlOrRd", name = "Brazilian Municipalities GDP in 2000", na.value = "white", 
                    labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5", "No Data")) +
  theme(legend.background = element_rect(fill = NA))


ggsave("gdp_ggplot.png", plot_1, path = "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures", dpi = 300)


