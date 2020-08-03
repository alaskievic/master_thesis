library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(ggplot2)


setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr")

crops_names <- c("banana", "barley", "cocoa", "coffee", "cotton_1", "cotton_2", "indiantea", "maize", "oatmeal", 
               "orange", "rice", "rubber", "sorghum", "soybean", "sugar_cane", "tobacco", "wheat", "yerba_mate")

#crops_all <- c(banana, barley , cocoa, coffee, cotton_1, cotton_2, indiantea, maize, oatmeal, orange, rice, rubber, sorghum, soybean, sugar_cane,
               #tobacco, wheat, yerba_mate)


banana <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/banana.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
barley <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/barley.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
cocoa <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/cocoa.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
coffee <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/coffee.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
cotton_1 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/cotton_1.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
cotton_2 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/cotton_2.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
indiantea <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/indiantea.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
maize <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/maize.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
oatmeal <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/oatmeal.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
orange <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/orange.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
rice <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/rice.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
rubber <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/rubber.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
sorghum <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/sorghums.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
soybean <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/soybeans.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
sugar_cane <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/sugar_cane.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
tobacco <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/tobacco.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
wheat <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/wheat.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
yerba_mate <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr/yerba_mate.xlsx", sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


#banana_quant <- banana %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  #select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  #pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  #replace_na(list(quant = 0))
  
#banana_quant <- aggregate(quant~cod+municip, banana_quant, mean)
#banana_quant <- arrange(banana_quant, cod)

##### tentando usar map

file.list <- list.files(pattern='*.xlsx')
#df.list <- lapply(file.list, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}


df_list_mean <- map(.x = df_list_map, .f = df_fix)

quantities_map <- reduce(df_list_mean, merge, by = c("cod", "municip"))

quantities_map <- quantities_map %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)



#####

#outrjeito
banana <- banana %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  dplyr::select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_banana = mean(quant))
  
barley <- barley %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_barley = mean(quant))

cocoa <- cocoa %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_cocoa = mean(quant))

coffee <- coffee %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_coffee = mean(quant))


cotton_1 <- cotton_1 %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_cotton1 = mean(quant))


cotton_2 <- cotton_2 %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_cotton2 = mean(quant))



indiantea <- indiantea %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_indiantea = mean(quant))




maize <- maize %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_maize = mean(quant))



oatmeal <- oatmeal %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_oatmeal = mean(quant))



orange <- orange %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_orange = mean(quant))


rice <- rice %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_rice = mean(quant))


rubber <- rubber %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_rubber = mean(quant))



sorghum <- sorghum %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_sorghum = mean(quant))



soybean <- soybean %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_soybean = mean(quant))



sugar_cane <- sugar_cane %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_sugarcane = mean(quant))


tobacco <- tobacco %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_tobacco = mean(quant))


wheat <- wheat %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_wheat = mean(quant))



yerba_mate <- yerba_mate %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
  select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
  pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
  replace_na(list(quant = 0)) %>%
  arrange(cod) %>%
  group_by(cod, municip) %>%
  summarize(quant_yerbamate = mean(quant))


quantities <- Reduce(function(x, y) merge(x, y, by = c("cod", "municip")), list(banana, barley , cocoa, coffee, cotton_1, cotton_2,
                    indiantea, maize, orange, rice, rubber, sorghum, soybean, sugar_cane,tobacco, wheat, yerba_mate))

quantities <- quantities[-1, ]







#summary(banana)[7,17]
#summary(barley)[7,17]
#summary(cocoa)[7,17]
#summary(coffee)[7,17]
#summary(cotton_2)[7,17]
#summary(indiantea)[7,17]
#summary(maize)[7,17]
#summary(orange)[7,17]
#summary(rice)[7,17]
#summary(rubber)[7,17]
#summary(sorghum)[7,17]
#summary(soybean)[7,17]
#summary(sugar_cane)[7,17]
#summary(tobacco)[7,17]
#summary(wheat)[7,17]
#summary(yerba_mate)[7,17]

#banana
5563-1755
#barley
5563-5387
#cocoa
5563-5287
#coffee
5563-3549
#cotton
5563-4379
#indiantea
5563-5556
#maize
5563-249
#orange
5563-1859
#rice
5563-1388
#rubber
5563-5149
#sorghum
5563-5151
#soybean
5563-4098
#sugarcane
5563-2067
#tobacco
5563-4590
#wheat
5563-4749
#yerbamate
5563-5010
