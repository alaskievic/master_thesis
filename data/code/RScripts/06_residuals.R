
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 1. Uses the residuals from the Bartik instrument regression to make maps ######################################################################

# Read Stata file containing the residuals of the Bartik instrument partialing out for municipality and year fixed effects
data <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/com_municip.dta")


# Ranks the residuals in quintiles

data <- as_tibble(data)
data <- data %>%  mutate(resid_rank = cut(100 * percent_rank(resid), seq(0, 100, len = 6),
                                include.lowest = TRUE)) %>%
  mutate(resid_rank_2 = cut(100 * percent_rank(resid), seq(0, 100, len = 5),
                          include.lowest = TRUE))


data_wider <- data %>% dplyr::select(-c("pq", "pq_log", "resid_rank", "resid_rank_2")) %>%
  pivot_wider(names_from = "year", values_from = "resid") 


data_sample <- sample_frac(data_wider, 0.1)

data_sample_plot <- pivot_longer(data_sample, -c("cod", "municip"), names_to = "year", values_to = "resid") %>%
  mutate(year = ymd(year, truncated = 2L))


data_sample_year <- pivot_wider(data_sample_plot, -c("cod") , names_from = "municip", values_from = "resid")
data_sample_year <- data_sample_year %>% mutate(year = ymd(year, truncated = 2L))



# Calculating some percentiles
quantile(data_sample$`2000`, probs = seq(.1, .9, by = .1), na.rm = TRUE)

quantiles <- data_sample %>%
map_if(is_double, quantile, probs = seq(.1, .9, by = .1), na.rm = TRUE)

a <- bind_rows(quantiles[-c(1,2)])

a <- a %>% add_column(pcls = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%"), .before = "2000")

data_sample_pcls <- pivot_longer(a, -pcls, names_to = "year", values_to = "resid") %>%
  mutate(year = ymd(year, truncated = 2L))


data_plot_2 <- ggplot(data_sample_pcls, aes(x = year, y = resid, group = pcls)) +
  geom_line()

data_plot_2


data_plot <- ggplot(data_sample_plot, aes(x = year, y = resid, group = municip)) +
  geom_line() +
  ylim(-0.4, 0.4) +
  theme_minimal()

data_plot




# Generating multiple datasets for some years

data_2000 <- dplyr::select(data_wider, c("cod", "municip", "2000")) %>%
  rename(resid_2000 = "2000") %>%
  mutate( pcls = cut(100 * percent_rank(resid_2000), seq(0, 100, len = 11),
                           include.lowest = TRUE))

data_2005 <- dplyr::select(data_wider, c("cod", "municip", "2005"))%>%
  rename(resid_2005 = "2005") %>%
  mutate( pcls = cut(100 * percent_rank(resid_2005), seq(0, 100, len = 11),
                     include.lowest = TRUE))

data_2010 <- dplyr::select(data_wider, c("cod", "municip", "2010"))%>%
  rename(resid_2010 = "2010")%>%
  mutate( pcls = cut(100 * percent_rank(resid_2010), seq(0, 100, len = 11),
                     include.lowest = TRUE))

data_2015 <- dplyr::select(data_wider, c("cod", "municip", "2015"))%>%
  rename(resid_2015 = "2015")%>%
  mutate( pcls = cut(100 * percent_rank(resid_2015), seq(0, 100, len = 11),
                     include.lowest = TRUE))

# Merging the data to the shapefiles and plotting them

shp_ibge_2000 <- merge(shp_ibge, data_2000, all.x = TRUE)
shp_ibge_2005 <- merge(shp_ibge, data_2005, all.x = TRUE)
shp_ibge_2010 <- merge(shp_ibge, data_2010, all.x = TRUE)
shp_ibge_2015 <- merge(shp_ibge, data_2015, all.x = TRUE)

# Map for 2000
map_resid_2000 <- tm_shape(shp_ibge_2000) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_resid_2000

# Map for 2005
map_resid_2005 <- tm_shape(shp_ibge_2005) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_resid_2005

# Map for 2010
map_resid_2010 <- tm_shape(shp_ibge_2010) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_resid_2010

# Map for 2015
map_resid_2015 <- tm_shape(shp_ibge_2015) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom")) 

map_resid_2015


# Saving all the maps
tmap_save(map_resid_2000, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_resid_2000.png")
tmap_save(map_resid_2005, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_resid_2005.png")
tmap_save(map_resid_2010, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_resid_2010.png")
tmap_save(map_resid_2015, "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/Figures/municip_resid_2015.png")


######### 2. Using R packages to run the regression previously done in Stata and comparing them ############################################################

model <- plm(pq_log, data = data, index =c("municip", "year"), model = "within", effect = "twoways")



#########
