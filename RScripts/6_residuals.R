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


options(stringsAsFactors = FALSE)
memory.limit(size=50000)
set.seed(42)

setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/R Codes/master_thesis")


data <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/Stata Codes/com_municip.dta")


##########

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

##########
#calculando medianas e percentis

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






#########
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


shp_ibge_2000 <- merge(shp_ibge, data_2000, all.x = TRUE)
shp_ibge_2005 <- merge(shp_ibge, data_2005, all.x = TRUE)
shp_ibge_2010 <- merge(shp_ibge, data_2010, all.x = TRUE)
shp_ibge_2015 <- merge(shp_ibge, data_2015, all.x = TRUE)

map_resid_2000 <- tm_shape(shp_ibge_2000) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)

map_resid_2000


map_resid_2005 <- tm_shape(shp_ibge_2005) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)

map_resid_2005

map_resid_2010 <- tm_shape(shp_ibge_2010) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)

map_resid_2010

map_resid_2015 <- tm_shape(shp_ibge_2015) +
  tm_polygons("pcls", palette = "RdYlGn", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA = "No Data",
              title = "Residuals Deciles") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.position = c("left", "bottom"), 
            frame = FALSE)

map_resid_2015


tmap_save(map_resid_2000, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2000.png", scale = 0.7, 
          dpi = 300)

tmap_save(map_resid_2000, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2000.eps", dpi = 300)

tmap_save(map_resid_2005, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2005.png", scale = 0.7, 
          dpi = 300)

tmap_save(map_resid_2005, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2005.eps", dpi = 300)

tmap_save(map_resid_2010, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2010.png", scale = 0.7, 
          dpi = 300)

tmap_save(map_resid_2010, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2010.eps", dpi = 300)

tmap_save(map_resid_2015, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/municip_resid_2015.png", scale = 0.7, 
          dpi = 300)

tmap_save(map_resid_2015, "C:/Users/Andrei/Desktop/Dissertation/Dados/Figures/mmunicip_resid_2015.eps", dpi = 300)


#########
model <- plm(pq_log, data = data, index =c("municip", "year"), model = "within", effect = "twoways")

#oi :)


#########
