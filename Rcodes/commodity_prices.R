library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(pals)


setwd("C:/Users/Andrei/Desktop/Dissertation/Dados")

comm_names <- c("...1", "COCOA", "COFFEE_ARABIC", "COFFEE_ROBUS", "TEA_AVG" , "TEA_COLOMBO", "TEA_KOLKATA", "TEA_MOMBASA", "SOYBEANS", 
                "BARLEY", "MAIZE", "SORGHUM", "RICE_05", "RICE_25", "RICE_A1", "RICE_05_VNM", "WHEAT_US_SRW", "WHEAT_US_HRW", 
                "BANANA_EU", "BANANA_US", "ORANGE", "SUGAR_EU", "SUGAR_US", "SUGAR_WLD", "TOBAC_US", "COTTON_A_INDX", "RUBBER_TSR20",
                "RUBBER1_MYSG")

cambio <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/cambio_nominal.xls", 
                          sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 1990 &  Date <= 2015)

ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 1990 &  Date <= 2015)


#ler e arrumar cpi
cpi <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/cpi_annual.xls", sheet = "FRED Graph", 
                  col_names = TRUE, na = "")

cpi$Date <- year(cpi$Date)

cpi <- cpi %>% filter (Date >= 1990 & Date <= 2015)


#ler commodities price
pink_prices <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/CMOHistoricalDataMonthly.xlsx", 
                          sheet = "Monthly Prices", col_names = TRUE, na = "..", skip = 6) %>% 
  select(comm_names)


#pink_prices2 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/CMOHistoricalDataMonthly.xlsx", 
                          #sheet = "Monthly Prices", col_names = TRUE, na = "..", skip = 6) %>%
  #select(1, 12, 13, 14, 15, 16, 17, 18, 25, 30, 31, 32, 33, 34, 35, 36, 37, 38 ,39, 40, 41, 46, 47, 48, 49, 56, 57, 58)

#colnames(pink_prices2)

#vamos checar
#all.equal(pink_prices, pink_prices2)

pink_prices <- pink_prices %>% 
  dplyr::rename(Month = ...1) %>%
  mutate(Month = str_replace(Month, "[M]", "-")) %>%
  mutate(Month = paste0(Month, '-01')) %>%
  mutate(Month = as.Date(Month, format = "%Y-%m-%d"))

pink_prices <- pink_prices %>% filter(Month >= "1990-01-01" & Month <=  "2015-12-01")

years <- format(as.Date(pink_prices$Month), format = "%Y")

years


#transformar medidas em ton
pink_prices <- pink_prices %>% mutate(COCOA = COCOA*1000,
                        COFFEE_ARABIC = COFFEE_ARABIC*1000,
                        COFFEE_ROBUS = COFFEE_ROBUS*1000,
                        TEA_AVG = TEA_AVG*1000,
                        TEA_COLOMBO = TEA_COLOMBO*1000,
                        TEA_KOLKATA = TEA_KOLKATA,
                        TEA_MOMBASA = TEA_MOMBASA*1000,
                        BANANA_EU = BANANA_EU*1000,
                        BANANA_US = BANANA_US*1000,
                        ORANGE = ORANGE*1000,
                        SUGAR_EU = SUGAR_EU*1000,
                        SUGAR_US = SUGAR_US*1000,
                        SUGAR_WLD = SUGAR_WLD*1000,
                        RUBBER_TSR20 = RUBBER_TSR20*1000,
                        RUBBER1_MYSG = RUBBER1_MYSG*1000)



#pink_prices10 <- pink_prices9 %>% mutate (Years = format(Month, "%Y")) %>%
  #group_by(Years)


pink_prices3 <- pink_prices %>% mutate (Years = format(Month, "%Y"))

pink_prices3 <- pink_prices %>% 
  gather(Commodity, Price, COCOA:RUBBER1_MYSG) %>%
  mutate (Years = format(Month, "%Y"))

#pink_prices3 <- group_by(pink_prices3, Commodity)

#summarize(pink_prices3, average = mean(Price, na.rm = TRUE))


#matou a pau
pink_prices3_avg <- aggregate(Price~Commodity+Years, pink_prices3, mean)

#verificar
#cocoa_avg <- tapply(pink_prices$COCOA, years, mean)
#coffee_arab_avg <- tapply(pink_prices$COFFEE_ARABIC, years, mean)
#coffe_robus_avg <- tapply(pink_prices$COFFEE_ROBUS, years, mean)
#tea_avg <- tapply(pink_prices$TEA_AVG, years, mean)
#tea_colombo_avg <- tapply(pink_prices$TEA_COLOMBO, years, mean)
#tea_kolkata_avg <- tapply(pink_prices$TEA_KOLKATA, years, mean)
#tea_mombasa_avg <- tapply(pink_prices$TEA_MOMBASA, years, mean)
#soy_avg <- tapply(pink_prices$SOYBEANS, years, mean)
#barley_avg <- tapply(pink_prices$BARLEY, years, mean)
#maize_avg <- tapply(pink_prices$MAIZE, years, mean)
#sorghum_avg <- tapply(pink_prices$SORGHUM, years, mean)
#rice_05_avg <- tapply(pink_prices$RICE_05, years, mean)
#rice_25_avg <- tapply(pink_prices$RICE_25, years, mean)
#rice_a1_avg <- tapply(pink_prices$RICE_A1, years, mean)
#rice_vnm_avg <- tapply(pink_prices$RICE_05_VNM, years, mean)
#wheat_s_avg <- tapply(pink_prices$WHEAT_US_SRW, years, mean)
#wheat_h_avg <- tapply(pink_prices$WHEAT_US_HRW, years, mean)
#banana_eu_avg <- tapply(pink_prices$BANANA_EU, years, mean)
#banana_us_avg <- tapply(pink_prices$BANANA_US, years, mean)
#orange_avg <- tapply(pink_prices$ORANGE, years, mean)
#sugar_eu_avg <- tapply(pink_prices$SUGAR_EU, years, mean)
#sugar_us_avg <- tapply(pink_prices$SUGAR_US, years, mean)
#sugar_world_avg <- tapply(pink_prices$SUGAR_WLD, years, mean)
#tobac_avg <- tapply(pink_prices$TOBAC_US, years, mean)
#cotton_avg <- tapply(pink_prices$COTTON_A_INDX, years, mean)
#rubber_ts_avg <- tapply(pink_prices$RUBBER_TSR20, years, mean)
#rubber_my_avg <- tapply(pink_prices$RUBBER1_MYSG, years, mean)


#verificar
#cocoa.df <- as.data.frame(cocoa_avg)
#coffe_arab.df <- as.data.frame(coffee_arab_avg)
#coffe_robus.df <- as.data.frame(coffe_robus_avg)
#tea.df <- as.data.frame(tea_avg)
#soy.df <- as.data.frame(soy_avg)
#barley.df <- as.data.frame(barley_avg)
#maize.df <- as.data.frame(maize_avg)
#sorghum.df <- as.data.frame(sorghum_avg)
#rice_05.df <- as.data.frame(rice_05_avg)
#rice_25.df <- as.data.frame(rice_25_avg)
#rice_a1.df <- as.data.frame(rice_a1_avg)
#wheat_h.df <- as.data.frame(wheat_h_avg)
#wheat_s.df <- as.data.frame(wheat_s_avg)
#banana_us.df <- as.data.frame(banana_us_avg)
#orange.df <- as.data.frame(orange_avg)
#sugar_world.df <- as.data.frame(sugar_world_avg)
#tobac.df <- as.data.frame(tobac_avg)
#cotton.df <- as.data.frame(cotton_avg)
#rubber.df <- as.data.frame(rubber_my_avg)

#lets spread
pink_prices3_avg <- pink_prices3_avg %>%
  spread(Commodity, Price)


#opam
comm_names <- comm_names[-1]


#aiaiai <- lmap(pink_prices3_avg[comm_names], ~{.x/ .x[[1]][1]*100})

indexes <- lmap(pink_prices3_avg[-1], ~{.x/ .x[[1]][1]*100}) %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25", "SUGAR_EU", "SUGAR_US")) %>%
  add_column (Years = pink_prices3_avg[[1]], .before = "BANANA_US")


#remove NAs
#aiaiai <- aiaiai[, colSums(is.na(aiaiai)) == 0]


#verificar
#cocoa_index <- transform(cocoa.df, index_cocoa = cocoa_avg/cocoa_avg[1]*100)
#coffe_arab_index <- transform(coffe_arab.df, index_arab = coffee_arab_avg/coffee_arab_avg[1]*100)
#coffe_robus_index <- transform(coffe_robus.df, index_robus = coffe_robus_avg/coffe_robus_avg[1]*100)
#tea_index <- transform(tea.df, index_tea = tea_avg/tea_avg[1]*100)
#soy_index <- transform(soy.df, index_soy = soy_avg/soy_avg[1]*100)
#barley_index <- transform(barley.df, index_barley = barley_avg/barley_avg[1]*100)
#maize_index <- transform(maize.df, index_maize = maize_avg/maize_avg[1]*100)
#sorghum_index <- transform(sorghum.df, index_sorghum = sorghum_avg/sorghum_avg[1]*100)
#rice_05_index <- transform(rice_05.df, index_rice05 = rice_05_avg/rice_05_avg[1]*100)
#rice_25_index <- transform(rice_25.df, index_rice25 = rice_25_avg/rice_25_avg[1]*100)
#rice_a1_index <- transform(rice_a1.df, index_ricea1 = rice_a1_avg/rice_a1_avg[1]*100)
#wheat_h_index <- transform(wheat_h.df, index_wheat_h = wheat_h_avg/wheat_h_avg[1]*100)
#wheat_s_index <- transform(wheat_s.df, index_wheat_s = wheat_s_avg/wheat_s_avg[1]*100)
#banana_us_index <- transform(banana_us.df, index_banana = banana_us_avg/banana_us_avg[1]*100)
#orange_index <- transform(orange.df, index_orange = orange_avg/orange_avg[1]*100)
#sugar_world_index <- transform(sugar_world.df, index_sugar = sugar_world_avg/sugar_world_avg[1]*100)
#tobac_index <- transform(tobac.df, index_tobac = tobac_avg/tobac_avg[1]*100)
#cotton_index <- transform(cotton.df, index_cotton = cotton_avg/cotton_avg[1]*100)
#rubber_index <- transform(rubber.df, index_rubber = rubber_my_avg/rubber_my_avg[1]*100)



#com_indexes <- cbind(cocoa_index[2], coffe_arab_index[2], coffe_robus_index[2], tea_index[2], soy_index[2], barley_index[2], maize_index[2], sorghum_index[2],
                     #rice_05_index[2], rice_a1_index[2], wheat_h_index[2],wheat_s_index[2], banana_us_index[2], orange_index[2], sugar_world_index[2],
                     #tobac_index[2], cotton_index[2], rubber_index[2])




ggplot(com_indexes, aes(x=row.names(cocoa_index), y = index_cocoa, group =1))+
      geom_line(colour = "Black")+
      geom_point()+
      labs(x = "Year", y="Price Index (1990=100)", title = "Individual Commodity Prices")


com_indexes_long <- com_indexes %>% gather (Commodity, Index, index_cocoa:index_rubber)


com_indexes_date <- cbind(row.names(cocoa_index), cocoa_index[2], coffe_arab_index[2], coffe_robus_index[2], tea_index[2], soy_index[2], barley_index[2], maize_index[2], sorghum_index[2],
                          rice_05_index[2], rice_a1_index[2], wheat_h_index[2],wheat_s_index[2], banana_us_index[2], orange_index[2], sugar_world_index[2],
                          tobac_index[2], cotton_index[2], rubber_index[2])


colnames(com_indexes_date)[1]  <- "Year"
colnames(com_indexes_date)[2:19]  <- c("Cocoa", "Coffee Arabic", "Coffee Robust", "Tea", "Soy", "Barley", "Maize", "Sorghum",
                                            "Rice 05", "Rice A1", "Wheat H", "Wheat S", "Banana", "Orange", "Sugar", "Tobacco", "Cotton", "Rubber")



com_indexes_long_date <- com_indexes_date %>% gather (Commodity, Index, Cocoa:Rubber)

indexes <- indexes %>%
  rename("Banana" = BANANA_US, "Cocoa" = COCOA, "Coffee Arabic" = COFFEE_ARABIC, "Coffee Robust" = COFFEE_ROBUS, "Tea" = TEA_AVG, "Soy" = SOYBEANS, 
         "Barley" = BARLEY, "Maize" = MAIZE, "Sorghum" = SORGHUM, "Rice 05" = RICE_05, "Rice A1" = RICE_A1, "Wheat H" = WHEAT_US_HRW, 
         "Wheat S" = WHEAT_US_SRW, "Orange" = ORANGE, "Sugar" = SUGAR_WLD, "Tobacco" = TOBAC_US, "Cotton" = COTTON_A_INDX, "Rubber" = RUBBER1_MYSG) %>%
  gather(Commodity, Index, Banana:`Wheat S`)




clrs = c("Cocoa"="black", "Coffee Arabic" = "coral", "Coffee Robust" = "chocolate" , "Tea" = "brown", "Soy" = "blue",
          "Barley" = "bisque", "Maize" = "navy", "Sorghum" = "violet", "Rice 05" = "darksalmon", "Rice A1" = "deepskyblue",
         "Wheat H" = "pink", "Wheat S" = "green", "Banana" = "red", "Orange" = "orange", "Sugar" = "yellow", "Tobacco" = "grey",
         "Cotton" = "blueviolet" , "Rubber" = "sienna")



graph <- ggplot(com_indexes_long_date ,aes(x=Year))+
  geom_line(data = com_indexes_long_date, aes(y=Index,color=Commodity, group = Commodity))+
  scale_color_manual(values=clrs) +
  geom_point(data=com_indexes_long_date,aes(x=Year, y=Index,color=Commodity),size=1) + 
  labs(x = "Year", y="Price Index (1990=100)", title = "Individual Commodity Prices") +
  graph + scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))

graph
  



graph2 <- ggplot(com_indexes_long_date ,aes(x=Year))+
  geom_line(data = com_indexes_long_date, aes(y=Index,color=Commodity, group = Commodity))+
  scale_color_manual(values=clrs) +
  geom_point(data=com_indexes_long_date,aes(x=Year, y=Index,color=Commodity),size=1) + 
  labs(x = "Year", y="Price Index (1990=100)") + 
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))
  
    
graph2



graph3 <- ggplot(indexes, aes(x=Years)) +
  geom_line(data = indexes, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=clrs)+
  geom_point(data = indexes, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))

graph3


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


#tentando trocar a cor
graph4 <- ggplot(indexes, aes(x=Years)) +
  geom_line(data = indexes, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=c25)+
  geom_point(data = indexes, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))

graph4


pie(rep(1, 25), col = c25)

graph5 <- ggplot(indexes, aes(x=Years)) +
  geom_line(data = indexes, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=as.vector(polychrome(18)))+
  geom_point(data = indexes, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))

graph5


year_prices <- pink_prices3_avg %>%
  select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25", "SUGAR_EU", "SUGAR_US")) %>%
  select_if(~ !any(is.na(.)))

#passar log
year_prices_log <- log(year_prices[-1]) %>%
  add_column (Years = pink_prices3_avg[[1]], .before = "BANANA_US")

#deflacionar pelo CPI
cpi_prices <- (pink_prices_final[-1]/(cpi$Index/100))
cpi_prices <- add_column(cpi_prices, Years = pink_prices3_avg[[1]], .before = "Banana") %>%
  as_tibble(cpi_prices)

#conversão
pink_prices_final <- pink_prices3_avg %>%
  select_if(~ !any(is.na(.))) %>%
  select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25", "SUGAR_EU", "SUGAR_US")) %>%
  rename("Banana" = BANANA_US, "Cocoa" = COCOA, "Coffee Arabic" = COFFEE_ARABIC, "Coffee Robust" = COFFEE_ROBUS, "Tea" = TEA_AVG, "Soy" = SOYBEANS, 
         "Barley" = BARLEY, "Maize" = MAIZE, "Sorghum" = SORGHUM, "Rice 05" = RICE_05, "Rice A1" = RICE_A1, "Wheat H" = WHEAT_US_HRW, 
         "Wheat S" = WHEAT_US_SRW, "Orange" = ORANGE, "Sugar" = SUGAR_WLD, "Tobacco" = TOBAC_US, "Cotton" = COTTON_A_INDX, "Rubber" = RUBBER1_MYSG)


br_prices <- cambio$Cambio * pink_prices_final[-1]
br_prices <- add_column(br_prices, Years = pink_prices3_avg[[1]], .before = "Banana")

#deflacionar
real_br_prices <- (br_prices[-1]/ipca$Index/100)
real_br_prices <- add_column(real_br_prices, Years = pink_prices3_avg[[1]], .before = "Banana") %>%
  as_tibble(real_br_prices)

#indexar preços reias em real
index_real_br_prices <- lmap(real_br_prices[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices3_avg[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat S`)



graph10 <- ggplot(index_real_br_prices, aes(x=Years)) +
  geom_line(data = index_real_br_prices, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=c25)+
  geom_point(data = index_real_br_prices, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015"))

graph10




