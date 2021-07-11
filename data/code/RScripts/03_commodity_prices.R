#Load packages
source("00_load_packages.R")

##### 1. Clean World Bank Pink Sheet database of global commodities prices #####


# Set commodities names that match the excel file
comm_names <- c("...1", "COCOA", "COFFEE_ARABIC", "COFFEE_ROBUS", "TEA_AVG",
                "TEA_COLOMBO", "TEA_KOLKATA", "TEA_MOMBASA", "SOYBEANS", 
                "BARLEY", "MAIZE", "SORGHUM", "RICE_05", "RICE_25", "RICE_A1",
                "RICE_05_VNM", "WHEAT_US_SRW", "WHEAT_US_HRW", "BANANA_EU",
                "BANANA_US", "ORANGE", "BEEF", "SUGAR_EU", "SUGAR_US", "SUGAR_WLD",
                "TOBAC_US", "COTTON_A_INDX", "RUBBER_TSR20", "RUBBER1_MYSG")

# Reads BRL(R$)/US$ exchange rate data
cambio <- read_excel(here("data", "raw", "prices", "cambio_nominal.xls"),
                          sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 1990 &  Date <= 2015)

# Reads Brazilian IPCA inflation data with 1990 = 100
ipca <- read_excel(here("data", "raw", "prices", "IPCA_anual.xls"), 
                   sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 1990 &  Date <= 2015)


# Reds CPI data with 1990 = 100
cpi <- read_excel(here("data", "raw", "prices", "cpi_annual.xls"), sheet = "FRED Graph", 
                  col_names = TRUE, na = "")

cpi$Date <- year(cpi$Date)
cpi      <- cpi %>% filter (Date >= 1990 & Date <= 2015)


# Reads the Pink Sheet prices
pink_prices <- read_excel(here("data", "raw", "prices", "CMOHistoricalDataMonthly.xlsx"), 
                          sheet = "Monthly Prices", col_names = TRUE, na = "..", skip = 6) %>% 
               dplyr::select(comm_names)


# Doing some renaming, formatting, and filtering
pink_prices <- pink_prices %>% 
  dplyr::rename(Month = ...1) %>%
  mutate(Month = str_replace(Month, "[M]", "-")) %>%
  mutate(Month = paste0(Month, '-01')) %>%
  mutate(Month = as.Date(Month, format = "%Y-%m-%d"))

pink_prices <- pink_prices %>% filter(Month >= "1990-01-01" & Month <  "2020-01-01")

years <- format(as.Date(pink_prices$Month), format = "%Y")

# Transform all measures in metric tons
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
                        BEEF = BEEF*1000,
                        SUGAR_EU = SUGAR_EU*1000,
                        SUGAR_US = SUGAR_US*1000,
                        SUGAR_WLD = SUGAR_WLD*1000,
                        COTTON_A_INDX = COTTON_A_INDX*1000,
                        RUBBER_TSR20 = RUBBER_TSR20*1000,
                        RUBBER1_MYSG = RUBBER1_MYSG*1000)


pink_prices <- pink_prices %>% 
  gather(Commodity, Price, COCOA:RUBBER1_MYSG) %>%
  mutate (Years = format(Month, "%Y"))


# Calculates the price for each commodity in each year by averaging wihtin 12 months
pink_prices_avg <- aggregate(Price~Commodity+Years, pink_prices, mean)


# Spread the data set
pink_prices_avg <- pink_prices_avg %>%
  spread(Commodity, Price)


# Takes out first value of commodity names
comm_names <- comm_names[-1]


# Create indexes for all commodity prices with 1990 = 100
indexes <- lmap(pink_prices_avg[-1], ~{.x/ .x[[1]][1]*100}) %>%
  dplyr::select_if(~ !any(is.na(.))) %>%
  dplyr::select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25", "SUGAR_EU", "SUGAR_US")) %>%
  add_column (Years = pink_prices_avg[[1]], .before = "BANANA_US")




#################### 2. Making graphs of the results ###########################

# Transform de dataframe to plot it
indexes <- indexes %>%
  rename("Banana" = BANANA_US, "Cocoa" = COCOA, "Coffee Arabic" = COFFEE_ARABIC,
         "Coffee Robust" = COFFEE_ROBUS, "Tea" = TEA_AVG, "Soy" = SOYBEANS, 
         "Barley" = BARLEY, "Maize" = MAIZE, "Sorghum" = SORGHUM, "Rice 05" = RICE_05,
         "Rice A1" = RICE_A1, "Wheat H" = WHEAT_US_HRW, "Wheat S" = WHEAT_US_SRW,
         "Orange" = ORANGE, "Sugar" = SUGAR_WLD, "Tobacco" = TOBAC_US,
         "Cotton" = COTTON_A_INDX, "Rubber" = RUBBER1_MYSG) %>%
  gather(Commodity, Index, Banana:`Wheat S`)



# Setting up some colors
clrs = c("Cocoa"="black", "Coffee Arabic" = "coral", "Coffee Robust" = "chocolate",
         "Tea" = "brown", "Soy" = "blue", "Barley" = "bisque", "Maize" = "navy",
         "Sorghum" = "violet", "Rice 05" = "darksalmon", "Rice A1" = "deepskyblue",
         "Wheat H" = "pink", "Wheat S" = "green", "Banana" = "red", "Orange" = "orange",
         "Sugar" = "yellow", "Tobacco" = "grey", "Cotton" = "blueviolet" , "Rubber" = "sienna")

# Plotting the graph
graph_1 <- ggplot(indexes, aes(x=Years)) +
  geom_line(data = indexes, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=clrs)+
  geom_point(data = indexes, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) +
  ggtitle("Individual Commodity Prices")

graph_1
# Saving the graph
# ggsave(filename = "com_prices_1.eps", plot = graph_1,
# path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/Figures")


# Taking out rubber
indexes_norubber <- filter(indexes, Commodity != "Rubber")

graph_2 <- ggplot(indexes_norubber, aes(x=Years)) +
  geom_line(data = indexes_norubber, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=clrs)+
  geom_point(data = indexes_norubber, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) +
  ggtitle("Individual Commodity Prices")

graph_2

#ggsave(filename = "com_prices_2_norubber.eps", plot = graph_2, path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/Figures")




# Trying another palette
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


# Graph with new palette
graph_3 <- ggplot(indexes, aes(x=Years)) +
  geom_line(data = indexes, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=c25)+
  geom_point(data = indexes, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) +
  ggtitle("Individual Commodity Prices")

graph_3
#ggsave(filename = "com_prices_3.eps", plot = graph_3, path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/Figures")


graph_4 <- ggplot(indexes_norubber, aes(x=Years)) +
  geom_line(data = indexes_norubber, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=c25)+
  geom_point(data = indexes_norubber, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) +
  ggtitle("Individual Commodity Prices")

graph_4
# ggsave(filename = "com_prices_4_norubber.eps", plot = graph_2,
# path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/Figures")







####### 3. Deflating and converting series to BRL(R$) and plotting them ########


# Select only the prices that match the quantities dataset
year_prices <- pink_prices_avg %>%
  dplyr::select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25",
                   "SUGAR_EU", "SUGAR_US")) %>%
  select_if(~ !any(is.na(.))) %>%
  as_tibble ()


# Create log prices
year_prices_log <- log(year_prices[-1]) %>%
  add_column (Years = pink_prices_avg[[1]], .before = "BANANA_US")

# Rename variable to consolidate the NOMINAL commodity prices dataset
pink_prices_final <- pink_prices_avg %>%
  select_if(~ !any(is.na(.))) %>%
  dplyr::select(-c("TEA_COLOMBO", "TEA_MOMBASA", "TEA_KOLKATA", "RICE_25",
                   "SUGAR_EU", "SUGAR_US")) %>%
  rename("Banana" = BANANA_US, "Cocoa" = COCOA, "Coffee Arabic" = COFFEE_ARABIC,
         "Coffee Robust" = COFFEE_ROBUS, "Tea" = TEA_AVG, "Soy" = SOYBEANS, 
         "Barley" = BARLEY, "Maize" = MAIZE, "Sorghum" = SORGHUM, "Rice 05" = RICE_05,
         "Rice A1"= RICE_A1, "Wheat H" = WHEAT_US_HRW, "Wheat S" = WHEAT_US_SRW,
         "Orange" = ORANGE, "Beef" = BEEF, "Sugar" = SUGAR_WLD, "Tobacco" = TOBAC_US,
         "Cotton" = COTTON_A_INDX, "Rubber" = RUBBER1_MYSG) %>%
  as_tibble()


# Using CPI to deflate the series with 1990=100
pink_prices_final <- pink_prices_final %>% filter(Years >=1990 & Years<=2015)
cpi_prices <- (pink_prices_final[-1]/(cpi$Index/100))

cpi_prices <- add_column(cpi_prices, Years = pink_prices_final[[1]], .before = "Banana") %>%
  as_tibble()


# Using CPI to deflate the series with 2010=100
cpi_prices_2010 <- (pink_prices_final[-1]/(cpi$Index_2/100))

cpi_prices_2010 <- add_column(cpi_prices_2010, Years = pink_prices_final[[1]], .before = "Banana") %>%
  as_tibble()


# Creates an index for cpi_prices
index_cpi_prices <- lmap(cpi_prices[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices_final[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat S`)

index_cpi_prices_2010 <- lmap(cpi_prices_2010[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices_final[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat S`)


# Get real prices variations between 2000 and 2010
price_var <- cpi_prices_2010 %>% filter(Years == 2000 | Years == 2010)

price_var <- lmap(price_var[-1], ~{.x/ .x[[1]][1]}) %>%
  add_column(year = price_var[[1]], .before = "Banana") %>%
  gather(commodity, price_var, Banana:`Wheat S`)

price_var %<>% filter(year == 2010) %>% mutate(price_var = (price_var - 1)* 100)




# Convert prices to BRL(R$)
br_prices <- cambio$Cambio * pink_prices_final[-1]
br_prices <- add_column(br_prices, Years = pink_prices_avg[[1]], .before = "Banana")

# Deflate the BR prices series using IPCA
real_br_prices <- (br_prices[-1]/ipca$Index/100)
real_br_prices <- add_column(real_br_prices, Years = pink_prices_avg[[1]], .before = "Banana") %>%
  as_tibble()

# Creating an index of the real_br_prices
index_real_br_prices <- lmap(real_br_prices[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices_avg[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat S`)





#Plot the indexed real_br_prices
graph_5 <- ggplot(index_real_br_prices, aes(x=Years)) +
  geom_line(data = index_real_br_prices, aes(y=Index, color = Commodity, group = Commodity))+
  scale_color_manual(values=c25)+
  geom_point(data = index_real_br_prices, aes(x=Years, y=Index, color=Commodity), size=1)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) + 
  ggtitle("Individual Commodity Prices")

graph_5


# Plot the deflated prices in US$ 1990=100
graph_aux1 <- cpi_prices %>% dplyr::select(-c("Coffee Robust", "Rice A1",
                                                   "Wheat H", "Sorghum", "Rubber")) %>%
  rename(Wheat = "Wheat S", Coffee = "Coffee Arabic", Rice = "Rice 05")

prices_graph1 <- lmap(graph_aux1[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices_final[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat`)

graph_6 <- ggplot(prices_graph1, aes(x=Years)) +
  geom_line(data = prices_graph1, aes(y=Index, color = Commodity, group = Commodity), lwd = 1)+
  scale_color_manual(values=c25)+
  geom_point(data = prices_graph1, aes(x=Years, y=Index, color=Commodity), size=2)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) + 
  ggtitle("Individual Commodity Prices")

graph_6

#Saving prices dataset
save(pink_prices_final, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_nominal_bartik.Rdata")
save(cpi_prices, file ="C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik.Rdata")
save(cpi_prices_2010, file ="C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

# Plot the deflated prices in US$ with 2010=100
# final graph
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_nominal_bartik.Rdata")


graph_aux2 <- cpi_prices_2010 %>% dplyr::select(-c("Coffee Robust", "Rice A1",
                                               "Wheat H", "Sorghum", "Rubber")) %>%
  rename(Wheat = "Wheat S", Coffee = "Coffee Arabic", Rice = "Rice 05")

prices_graph2 <- lmap(graph_aux2[-1], ~{.x/ .x[[1]][1]*100}) %>%
  add_column(Years = pink_prices_final[[1]], .before = "Banana") %>%
  gather(Commodity, Index, Banana:`Wheat`)


graph_7 <- ggplot(prices_graph2, aes(x=Years)) +
  geom_line(data = prices_graph2, aes(y=Index, color = Commodity, group = Commodity), lwd = 1)+
  scale_color_manual(values=c25)+
  geom_point(data = prices_graph2, aes(x=Years, y=Index, color=Commodity), size=2)+
  labs(x = "Year", y="Price Index (1990=100)") +
  scale_x_discrete(breaks = c("1990", "1995", "2000", "2005", "2010", "2015")) +
  ggtitle("Individual Commodity Prices (2010$)") + 
  theme_bw(base_size = 13)

graph_7

ggsave(filename = "com_prices.eps", plot = graph_7, path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")
ggsave(filename = "com_prices.png", plot = graph_7, path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")




# Plotting
com_prices <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ExternalData.xlsx",
                         sheet = "Values", col_names = TRUE)


com_prices %<>% rename(date = "Commodity") %>% slice(-c(1:3))


com_prices %<>% mutate(date = str_replace_all(date, "M", "-0")) %>%
  mutate(date = paste0(date, "-01")) %>%
  mutate(date = str_replace_all(date, "-012", "-12")) %>%
  mutate(date = str_replace_all(date, "-011", "-11")) %>%
  mutate(date = str_replace_all(date, "-010", "-10"))

com_prices %<>% mutate(date = parse_date(date, "%Y-%m-%d"))


com_prices %<>% .[, 1:10]

com_prices %<>% set_names(c("date", "all_comm", "all_nolgold", "nfuel", "foodbev", "food", "bev", "indust",
                            "agro", "agroraw"))




com_graph <- com_prices %>% filter(year(date) >=1992) %>% mutate(agro = as.double(agro)) %>%
  mutate(foodbev = as.double(foodbev)) %>% mutate(all_comm = as.double(all_comm))


prices_plot <- ggplot(data = com_graph, aes(x = date, y = agro, lty = "Commodity Index")) + 
  geom_line(lwd = 1.2, color = "blue") +
  labs(y = "Agricultural Price Index (2016=100)", x = "Year") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=16), legend.position = c(0.80, 0.3), 
        legend.box.background = element_blank(), legend.title = element_blank())
  

prices_plot


 ggsave(filename = "prices_plot.png", 
        plot = prices_plot, 
        path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")
# 
# 
# prices_plot <- ggplot(data = com_graph, aes(x = date, y = agro)) + 
#   geom_line(lwd = 1, color = "blue") +
#   labs(title = "Commodity Price Index (Agricultural Sector in US$)",
#        y = "Agricultural Price Index (2016=100)", x = "Year") +
#   scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
#   theme_bw(base_size = 13)
