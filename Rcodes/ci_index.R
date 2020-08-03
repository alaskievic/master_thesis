library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(pals)
library(haven)
library(xlsx)
library(foreign)
library(writexl)

#vamos fazer um por um
prices <- pink_prices_final %>% filter (Years >= 2000)
quantities <- as_tibble(quantities)


#somar os dois cotton
quantities_cotton_sum <- mutate(quantities, cotton = rowSums(select(quantities, quant_cotton1, quant_cotton2))) %>%
  select(-c(quant_cotton1, quant_cotton2))


##### mais sucinto

names_quant <- colnames(quantities[-c(1,2)])
names_prices <- colnames(prices[-1])


#####

#banana
pq_banana <- NULL

for (i in unique(prices$Banana)) {
  tmp <- quantities$quant_banana*i
  pq_banana <- cbind(tmp, pq_banana)
}

pq_banana <- as.data.frame(pq_banana)
pq_banana <- pq_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
pq_barley <- NULL

for (i in unique(prices$Barley)) {
  tmp <- quantities$quant_barley*i
  pq_barley <- cbind(tmp, pq_barley)
}

pq_barley <- as.data.frame(pq_barley)
pq_barley <- pq_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
pq_cocoa <- NULL

for (i in unique(prices$Cocoa)) {
  tmp <- quantities$quant_cocoa*i
  pq_cocoa <- cbind(tmp, pq_cocoa)
}

pq_cocoa <- as.data.frame(pq_cocoa)
pq_cocoa <- pq_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
pq_coffee_arabic <- NULL

for (i in unique(prices$`Coffee Arabic`)) {
  tmp <- quantities$quant_coffee*i
  pq_coffee_arabic <- cbind(tmp, pq_coffee_arabic)
}

pq_coffee_arabic <- as.data.frame(pq_coffee_arabic)
pq_coffee_arabic <- pq_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#coffee usando o preço do robust
pq_coffe_robust <- NULL

for (i in unique(prices$`Coffee Robust`)) {
  tmp <- quantities$quant_coffee*i
  pq_coffe_robust <- cbind(tmp, pq_coffe_robust)
}

pq_coffe_robust <- as.data.frame(pq_coffe_robust)
pq_coffe_robust <- pq_coffe_robust %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_1  tem poucos
pq_cotton_1 <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities$quant_cotton1*i
  pq_cotton_1 <- cbind(tmp, pq_cotton_1)
}

pq_cotton_1 <- as.data.frame(pq_cotton_1)
pq_cotton_1 <- pq_cotton_1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_2
pq_cotton_2 <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities$quant_cotton2*i
  pq_cotton_2 <- cbind(tmp, pq_cotton_2)
}

pq_cotton_2 <- as.data.frame(pq_cotton_2)
pq_cotton_2 <- pq_cotton_2 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_sum
pq_cotton <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities_cotton_sum$cotton*i
  pq_cotton <- cbind(tmp, pq_cotton)
}

pq_cotton <- as.data.frame(pq_cotton)
pq_cotton <- pq_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#maize
pq_maize <- NULL

for (i in unique(prices$Maize)) {
  tmp <- quantities$quant_maize*i
  pq_maize <- cbind(tmp, pq_maize)
}

pq_maize <- as.data.frame(pq_maize)
pq_maize <- pq_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
pq_orange <- NULL

for (i in unique(prices$Orange)) {
  tmp <- quantities$quant_orange*i
  pq_orange <- cbind(tmp, pq_orange)
}

pq_orange <- as.data.frame(pq_orange)
pq_orange <- pq_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
pq_sorghum <- NULL

for (i in unique(prices$Sorghum)) {
  tmp <- quantities$quant_sorghum*i
  pq_sorghum <- cbind(tmp, pq_sorghum)
}

pq_sorghum <- as.data.frame(pq_sorghum)
pq_sorghum <- pq_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
pq_soy <- NULL

for (i in unique(prices$Soy)) {
  tmp <- quantities$quant_soybean*i
  pq_soy <- cbind(tmp, pq_soy)
}

pq_soy <- as.data.frame(pq_soy)
pq_soy <- pq_soy %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
pq_tobacco <- NULL

for (i in unique(prices$Tobacco)) {
  tmp <- quantities$quant_tobacco*i
  pq_tobacco <- cbind(tmp, pq_tobacco)
}

pq_tobacco <- as.data.frame(pq_tobacco)
pq_tobacco <- pq_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
pq_sugarcane <- NULL

for (i in unique(prices$Sugar)) {
  tmp <- quantities$quant_sugarcane*i
  pq_sugarcane <- cbind(tmp, pq_sugarcane)
}

pq_sugarcane <- as.data.frame(pq_sugarcane)
pq_sugarcane <- pq_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Rubber
pq_rubber <- NULL

for (i in unique(prices$Rubber)) {
  tmp <- quantities$quant_rubber*i
  pq_rubber <- cbind(tmp, pq_rubber)
}

pq_rubber <- as.data.frame(pq_rubber)
pq_rubber <- pq_rubber %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Tea Indian
pq_indiantea <- NULL

for (i in unique(prices$Tea)) {
  tmp <- quantities$quant_indiantea*i
  pq_indiantea <- cbind(tmp, pq_indiantea)
}

pq_indiantea <- as.data.frame(pq_indiantea)
pq_indiantea <- pq_indiantea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Tea Yerba
pq_yerbamate <- NULL

for (i in unique(prices$Tea)) {
  tmp <- quantities$quant_yerbamate*i
  pq_yerbamate <- cbind(tmp, pq_yerbamate)
}

pq_yerbamate <- as.data.frame(pq_yerbamate)
pq_yerbamate <- pq_yerbamate %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat Hard

pq_wheat_hard <- NULL

for (i in unique(prices$`Wheat H`)) {
  tmp <- quantities$quant_wheat*i
  pq_wheat_hard <- cbind(tmp, pq_wheat_hard)
}

pq_wheat_hard <- as.data.frame(pq_wheat_hard)
pq_wheat_hard <- pq_wheat_hard %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Wheat Soft
pq_wheat_soft <- NULL

for (i in unique(prices$`Wheat S`)) {
  tmp <- quantities$quant_wheat*i
  pq_wheat_soft <- cbind(tmp, pq_wheat_soft)
}

pq_wheat_soft <- as.data.frame(pq_wheat_soft)
pq_wheat_soft <- pq_wheat_soft %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice 05
pq_rice_05 <- NULL

for (i in unique(prices$`Rice 05`)) {
  tmp <- quantities$quant_rice*i
  pq_rice_05 <- cbind(tmp, pq_rice_05)
}

pq_rice_05 <- as.data.frame(pq_rice_05)
pq_rice_05 <- pq_rice_05 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice A1
pq_rice_a1 <- NULL

for (i in unique(prices$`Rice A1`)) {
  tmp <- quantities$quant_rice*i
  pq_rice_a1 <- cbind(tmp, pq_rice_a1)
}

pq_rice_a1 <- as.data.frame(pq_rice_a1)
pq_rice_a1 <- pq_rice_a1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



#unindo tudo
pq_banana <- pq_banana %>% pivot_longer(-municip, names_to = "year", values_to = "banana")
pq_barley <- pq_barley %>% pivot_longer(-municip, names_to = "year", values_to = "barley")
pq_cocoa <- pq_cocoa %>% pivot_longer(-municip, names_to = "year", values_to = "cocoa")
pq_coffee_arabic <- pq_coffee_arabic %>% pivot_longer(-municip, names_to = "year", values_to = "coffee")
pq_cotton <- pq_cotton %>% pivot_longer(-municip, names_to = "year", values_to = "cotton")
pq_indiantea <- pq_indiantea %>% pivot_longer(-municip, names_to = "year", values_to = "indiantea")
pq_maize <- pq_maize %>% pivot_longer(-municip, names_to = "year", values_to = "maize")
pq_orange <- pq_orange %>% pivot_longer(-municip, names_to = "year", values_to = "orange")
pq_rice_05 <- pq_rice_05 %>% pivot_longer(-municip, names_to = "year", values_to = "rice")
pq_rubber <- pq_rubber %>% pivot_longer(-municip, names_to = "year", values_to = "rubber")
pq_sorghum <- pq_sorghum %>% pivot_longer(-municip, names_to = "year", values_to = "sorghum")
pq_soy <- pq_soy %>% pivot_longer(-municip, names_to = "year", values_to = "soybean")
pq_sugarcane <- pq_sugarcane %>% pivot_longer(-municip, names_to = "year", values_to = "sugarcane")
pq_tobacco <- pq_tobacco %>% pivot_longer(-municip, names_to = "year", values_to = "tobacco")
pq_wheat_soft <- pq_wheat_soft %>% pivot_longer(-municip, names_to = "year", values_to = "wheat")
pq_yerbamate <- pq_yerbamate %>% pivot_longer(-municip, names_to = "year", values_to = "yerbamate")

##### mais sucinto
df_pivot <- function(x){
  x %>% pivot_longer(-municip, names_to = "year", values_to = "pq")
  }
#####

a <-list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
    pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate)


pqzada_map <- map(.x = a, .f = df_pivot)

pqzada_2 <- reduce(pqzada_map, merge, by = "municip") %>%
  setNames




pqzada_2 <- Reduce(inner_join, pqzada_map)

pqzada <- Reduce(inner_join, list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
                     pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate))
 

pq_final <- pqzada %>% mutate(sum_pq = select(., banana:yerbamate) %>% 
                  rowSums(na.rm = TRUE)) %>%
                  select(municip, year, sum_pq)

pq_final_wider <- pq_final %>% pivot_wider(names_from = "year", values_from = "sum_pq", names_repair = "minimal")
           







#####fazer o mesmo com os preços deflacioandos pelo CPI

#vamos fazer um por um
prices <- cpi_prices %>% filter (Years >= 2000)
quantities <- as_tibble(quantities)

#banana
pq_banana <- NULL

for (i in unique(prices$Banana)) {
  tmp <- quantities$quant_banana*i
  pq_banana <- cbind(tmp, pq_banana)
}

pq_banana <- as.data.frame(pq_banana)
pq_banana <- pq_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
pq_barley <- NULL

for (i in unique(prices$Barley)) {
  tmp <- quantities$quant_barley*i
  pq_barley <- cbind(tmp, pq_barley)
}

pq_barley <- as.data.frame(pq_barley)
pq_barley <- pq_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
pq_cocoa <- NULL

for (i in unique(prices$Cocoa)) {
  tmp <- quantities$quant_cocoa*i
  pq_cocoa <- cbind(tmp, pq_cocoa)
}

pq_cocoa <- as.data.frame(pq_cocoa)
pq_cocoa <- pq_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
pq_coffee_arabic <- NULL

for (i in unique(prices$`Coffee Arabic`)) {
  tmp <- quantities$quant_coffee*i
  pq_coffee_arabic <- cbind(tmp, pq_coffee_arabic)
}

pq_coffee_arabic <- as.data.frame(pq_coffee_arabic)
pq_coffee_arabic <- pq_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#coffee usando o preço do robust
pq_coffe_robust <- NULL

for (i in unique(prices$`Coffee Robust`)) {
  tmp <- quantities$quant_coffee*i
  pq_coffe_robust <- cbind(tmp, pq_coffe_robust)
}

pq_coffe_robust <- as.data.frame(pq_coffe_robust)
pq_coffe_robust <- pq_coffe_robust %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_1  tem poucos
pq_cotton_1 <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities$quant_cotton1*i
  pq_cotton_1 <- cbind(tmp, pq_cotton_1)
}

pq_cotton_1 <- as.data.frame(pq_cotton_1)
pq_cotton_1 <- pq_cotton_1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_2
pq_cotton_2 <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities$quant_cotton2*i
  pq_cotton_2 <- cbind(tmp, pq_cotton_2)
}

pq_cotton_2 <- as.data.frame(pq_cotton_2)
pq_cotton_2 <- pq_cotton_2 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#somar os dois cotton
quantities_cotton_sum <- mutate(quantities, cotton = rowSums(select(quantities, quant_cotton1, quant_cotton2))) %>%
  select(-c(quant_cotton1, quant_cotton2))

pq_cotton <- NULL

for (i in unique(prices$Cotton)) {
  tmp <- quantities_cotton_sum$cotton*i
  pq_cotton <- cbind(tmp, pq_cotton)
}

pq_cotton <- as.data.frame(pq_cotton)
pq_cotton <- pq_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#maize
pq_maize <- NULL

for (i in unique(prices$Maize)) {
  tmp <- quantities$quant_maize*i
  pq_maize <- cbind(tmp, pq_maize)
}

pq_maize <- as.data.frame(pq_maize)
pq_maize <- pq_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
pq_orange <- NULL

for (i in unique(prices$Orange)) {
  tmp <- quantities$quant_orange*i
  pq_orange <- cbind(tmp, pq_orange)
}

pq_orange <- as.data.frame(pq_orange)
pq_orange <- pq_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
pq_sorghum <- NULL

for (i in unique(prices$Sorghum)) {
  tmp <- quantities$quant_sorghum*i
  pq_sorghum <- cbind(tmp, pq_sorghum)
}

pq_sorghum <- as.data.frame(pq_sorghum)
pq_sorghum <- pq_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
pq_soy <- NULL

for (i in unique(prices$Soy)) {
  tmp <- quantities$quant_soybean*i
  pq_soy <- cbind(tmp, pq_soy)
}

pq_soy <- as.data.frame(pq_soy)
pq_soy <- pq_soy %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
pq_tobacco <- NULL

for (i in unique(prices$Tobacco)) {
  tmp <- quantities$quant_tobacco*i
  pq_tobacco <- cbind(tmp, pq_tobacco)
}

pq_tobacco <- as.data.frame(pq_tobacco)
pq_tobacco <- pq_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
pq_sugarcane <- NULL

for (i in unique(prices$Sugar)) {
  tmp <- quantities$quant_sugarcane*i
  pq_sugarcane <- cbind(tmp, pq_sugarcane)
}

pq_sugarcane <- as.data.frame(pq_sugarcane)
pq_sugarcane <- pq_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Rubber
pq_rubber <- NULL

for (i in unique(prices$Rubber)) {
  tmp <- quantities$quant_rubber*i
  pq_rubber <- cbind(tmp, pq_rubber)
}

pq_rubber <- as.data.frame(pq_rubber)
pq_rubber <- pq_rubber %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Tea Indian
pq_indiantea <- NULL

for (i in unique(prices$Tea)) {
  tmp <- quantities$quant_indiantea*i
  pq_indiantea <- cbind(tmp, pq_indiantea)
}

pq_indiantea <- as.data.frame(pq_indiantea)
pq_indiantea <- pq_indiantea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Tea Yerba
pq_yerbamate <- NULL

for (i in unique(prices$Tea)) {
  tmp <- quantities$quant_yerbamate*i
  pq_yerbamate <- cbind(tmp, pq_yerbamate)
}

pq_yerbamate <- as.data.frame(pq_yerbamate)
pq_yerbamate <- pq_yerbamate %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat Hard

pq_wheat_hard <- NULL

for (i in unique(prices$`Wheat H`)) {
  tmp <- quantities$quant_wheat*i
  pq_wheat_hard <- cbind(tmp, pq_wheat_hard)
}

pq_wheat_hard <- as.data.frame(pq_wheat_hard)
pq_wheat_hard <- pq_wheat_hard %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Wheat Sofr
pq_wheat_soft <- NULL

for (i in unique(prices$`Wheat S`)) {
  tmp <- quantities$quant_wheat*i
  pq_wheat_soft <- cbind(tmp, pq_wheat_soft)
}

pq_wheat_soft <- as.data.frame(pq_wheat_soft)
pq_wheat_soft <- pq_wheat_soft %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice 05
pq_rice_05 <- NULL

for (i in unique(prices$`Rice 05`)) {
  tmp <- quantities$quant_rice*i
  pq_rice_05 <- cbind(tmp, pq_rice_05)
}

pq_rice_05 <- as.data.frame(pq_rice_05)
pq_rice_05 <- pq_rice_05 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice A1
pq_rice_a1 <- NULL

for (i in unique(prices$`Rice A1`)) {
  tmp <- quantities$quant_rice*i
  pq_rice_a1 <- cbind(tmp, pq_rice_a1)
}

pq_rice_a1 <- as.data.frame(pq_rice_a1)
pq_rice_a1 <- pq_rice_a1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#unindo tudo
pq_banana <- pq_banana %>% pivot_longer(-municip, names_to = "year", values_to = "banana")
pq_barley <- pq_barley %>% pivot_longer(-municip, names_to = "year", values_to = "barley")
pq_cocoa <- pq_cocoa %>% pivot_longer(-municip, names_to = "year", values_to = "cocoa")
pq_coffee_arabic <- pq_coffee_arabic %>% pivot_longer(-municip, names_to = "year", values_to = "coffee")
pq_cotton <- pq_cotton %>% pivot_longer(-municip, names_to = "year", values_to = "cotton")
pq_indiantea <- pq_indiantea %>% pivot_longer(-municip, names_to = "year", values_to = "indiantea")
pq_maize <- pq_maize %>% pivot_longer(-municip, names_to = "year", values_to = "maize")
pq_orange <- pq_orange %>% pivot_longer(-municip, names_to = "year", values_to = "orange")
pq_rice_05 <- pq_rice_05 %>% pivot_longer(-municip, names_to = "year", values_to = "rice")
pq_rubber <- pq_rubber %>% pivot_longer(-municip, names_to = "year", values_to = "rubber")
pq_sorghum <- pq_sorghum %>% pivot_longer(-municip, names_to = "year", values_to = "sorghum")
pq_soy <- pq_soy %>% pivot_longer(-municip, names_to = "year", values_to = "soybean")
pq_sugarcane <- pq_sugarcane %>% pivot_longer(-municip, names_to = "year", values_to = "sugarcane")
pq_tobacco <- pq_tobacco %>% pivot_longer(-municip, names_to = "year", values_to = "tobacco")
pq_wheat_soft <- pq_wheat_soft %>% pivot_longer(-municip, names_to = "year", values_to = "wheat")
pq_yerbamate <- pq_yerbamate %>% pivot_longer(-municip, names_to = "year", values_to = "yerbamate")




pqzada <- Reduce(inner_join, list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
                                  pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate))


pq_final <- pqzada %>% mutate(sum_pq = select(., banana:yerbamate) %>% 
                                rowSums(na.rm = TRUE)) %>%
  select(municip, year, sum_pq)


pq_final_wider <- pq_final %>% pivot_wider(names_from = "year", values_from = "sum_pq", names_repair = "minimal") %>%
  add_column(cod = cocoa$cod[-1], .before = "municip")

#####
write_xlsx(pq_final_wider, 'C:/Users/Andrei/Desktop/Dissertation/Dados\\pq_deflate.xlsx')

pq_final_longer <- pq_final_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pq")
pq_final_longer_log <- transform(pq_final_longer, pq = log(pq))
pq_final_longer_log <- as_tibble(pq_final_longer_log)

#####
write_xlsx(pq_final_longer, 'C:/Users/Andrei/Desktop/Dissertation/Dados\\pq_deflate_longer.xlsx')
write_xlsx(pq_final_longer_log, 'C:/Users/Andrei/Desktop/Dissertation/Dados\\pq_deflate_longer_log.xlsx')

#salvar pro STATA
write.dta(pq_final_wider, "pq_deflate.dta")
