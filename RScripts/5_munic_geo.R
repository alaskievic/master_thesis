
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")
options(stringsAsFactors = FALSE)


memory.limit(size=50000)

#dsn - data source (path)
#layers - shapefile name without extension
#all <- geobr::lookup_muni(name_muni = 'all')

#########

shp_5564 <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/municipio5564", "municipio5564", stringsAsFactors = F)

shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)

shp_ufs <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Dados/Shapefiles/uf_2019", "BR_UF_2019", stringsAsFactors = F)

municip_pib <-read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/municip_pib_IPEA.xls", 
                     sheet = "Séries", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2000 <- municip_pib %>% rename ("cod" = "Codigo", "municip" = "Município", "pib_2000" = "2000") %>%
  dplyr::select(cod, municip, pib_2000) %>%
  arrange(cod)

names(shp_ibge@data)[1] = "cod"

#fazer desse jeito e não com @data
shp_ibge <- merge(shp_ibge, municip_pib_2000, all.x = TRUE)

for(i in 1:length(shp_ibge@data[,6])){if(is.na(shp_ibge@data[i,6])){shp_ibge@data[i,6]=0}}


municip_pib_2000_rank <- mutate(municip_pib_2000, pcls = cut(100 * percent_rank(pib_2000), seq(0, 100, len = 6),
                                                             include.lowest = TRUE))

pq_final_wider <- pq_final_wider %>% dplyr::select("cod", "2000") %>% rename("index_2000" = "2000")

shp_ibge_2 <- merge(shp_ibge, pq_final_wider, all.x = TRUE)

for(i in 1:length(shp_ibge_2@data[,7])){if(is.na(shp_ibge_2@data[i,7])){shp_ibge_2@data[i,7]=0}}

shp_ibge_2@data[,8] = shp_ibge_2@data$index_2000/shp_ibge_2@data$pib_2000

shp_ibge_2@data[,9] = cut(100 * percent_rank(shp_ibge_2@data$V8), seq(0, 100, len = 6),
                          include.lowest = TRUE)

#mutate (cod = str_sub(cod, end = -2))

#####opção 1: ggplot com tidy()


mapamun <- tidy(shp_ibge, region = "cod") %>%
  rename("cod" = "id")

data_rpib <- left_join(mapamun, municip_pib_2000_rank, "cod")


#color = NA retira as bordas
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
  
#########

######### opção 2 usando tm

shp_ibge <- merge(shp_ibge, municip_pib_2000_rank, all.x = TRUE)


municip_tm <- tm_shape(shp_ibge) +
  tm_polygons("pcls", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
              title = "Municipalities GDP in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)
  
municip_tm

tmap_save(municip_tm, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_tm.png", scale = 0.7, 
          width = 6.125)



municip_tm_2 <- tm_shape(shp_ibge_2) +
  tm_polygons("V9", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data", labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
              title = "Crops over GDP in 2000") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.75, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)

municip_tm_2

tmap_save(municip_tm_2, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_tm_2.png", scale = 0.7, 
          dpi = 300)

tmap_save(municip_tm_2, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_tm_2.eps", dpi = 300)

#####

##### informaçoes sobre os joins
municip_5572 <- as.data.frame(shp_ibge$cod)
municip_5572 <- as_tibble(municip_5572)
municip_5572 <- municip_5572 %>% rename("cod" = "shp_ibge$cod") %>%
  mutate (cod = as.character(cod))


municip_diff_pib <- anti_join(municip_pib_2000, municip_5564, by = "cod")




banana <- banana %>% filter (cod > 1) %>% ungroup()

diff_1 <- anti_join(shp_ibge@data, banana, by = "cod")

#banana <- banana %>% mutate (cod = str_sub(cod, end = -2)) %>%
  arrange(cod)

codmunicip_diff_pam <- anti_join(municip_5564, banana, by = "cod")

#municip_join <- merge(municip_pib_2000, municip_5564, by = "cod")

codmunicip_diff_pam_2 <- anti_join(municip_pib_2000, banana, by = "cod")
codmunicip_diff_pam_3 <- anti_join(banana, municip_5564, by = "cod")
codmunicip_diff_pam_4 <- anti_join(banana, municip_pib_2000, by = "cod")

#26 municipios não estão, nenhum deles tem valor para o PIB
municip_diff_pib <- anti_join(municip_pib_2000, municip_5572, by = "cod")

###quais são os dois muncipios a mais, na verdade são duas lagos, lagoa dos patos e lagoa mirim (as duas maiores lagos do BR e AL)
municip_diff_pib2 <- anti_join(municip_5572, municip_pib_2000, by = "cod")

#####

