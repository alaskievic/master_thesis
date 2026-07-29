
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

########## 1. Builds the Commodity Index, i.e, Bartik (shift-share) instrument #

#Loading the datasets
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/prices_real_bartik_2010.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/shares_1995_bartik.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/quantities_1995_bartik.Rdata")


# Subset prices to 2000 forward
prices <- cpi_prices_2010 %>% filter(Years >= 2000)


# Sum up the two types of cotton
quantities_cotton_sum <- mutate(quantities_1995, cotton = rowSums(dplyr::select(quantities_1995, cotton_1, cotton_2))) %>%
  dplyr::select(-c(cotton_1, cotton_2))


########## Multiplies the average quantity of each crop between 1995-1999 by its prices from 2000-2015 ##############################################
########## Stil did not find an easier and less complicated way to do this
#banana
pq_banana <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$banana)*unique(prices$Banana)[i]
  pq_banana <- cbind(pq_banana, tmp)
}

pq_banana <- as.data.frame(pq_banana)
pq_banana <- pq_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
pq_barley <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$barley)*unique(prices$Barley)[i]
  pq_barley <- cbind(pq_barley, tmp)
}

pq_barley <- as.data.frame(pq_barley)
pq_barley <- pq_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
pq_cocoa <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$cocoa)*unique(prices$Cocoa)[i]
  pq_cocoa <- cbind(pq_cocoa, tmp)
}

pq_cocoa <- as.data.frame(pq_cocoa)
pq_cocoa <- pq_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
pq_coffee_arabic <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$coffee)*unique(prices$`Coffee Arabic`)[i]
  pq_coffee_arabic <- cbind(pq_coffee_arabic, tmp)
}

pq_coffee_arabic <- as.data.frame(pq_coffee_arabic)
pq_coffee_arabic <- pq_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#coffee usando o preço do robust
pq_coffe_robust <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$coffee)*unique(prices$`Coffee Robust`)[i]
  pq_coffe_robust <- cbind(pq_coffe_robust, tmp)
}

pq_coffe_robust <- as.data.frame(pq_coffe_robust)
pq_coffe_robust <- pq_coffe_robust %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_1
pq_cotton_1 <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$cotton_1)*unique(prices$Cotton)[i]
  pq_cotton_1 <- cbind(pq_cotton_1, tmp)
}

pq_cotton_1 <- as.data.frame(pq_cotton_1)
pq_cotton_1 <- pq_cotton_1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_2
pq_cotton_2 <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$cotton_2)*unique(prices$Cotton)[i]
  pq_cotton_2 <- cbind(pq_cotton_2, tmp)
}

pq_cotton_2 <- as.data.frame(pq_cotton_2)
pq_cotton_2 <- pq_cotton_2 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_sum
pq_cotton <- NULL

for (i in 1:16) {
  tmp <- (quantities_cotton_sum$cotton)*unique(prices$Cotton)[i]
  pq_cotton <- cbind(pq_cotton, tmp)
}

pq_cotton <- as.data.frame(pq_cotton)
pq_cotton <- pq_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#maize
pq_maize <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$maize)*unique(prices$Maize)[i]
  pq_maize <- cbind(pq_maize, tmp)
}

pq_maize <- as.data.frame(pq_maize)
pq_maize <- pq_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
pq_orange <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$orange)*unique(prices$Orange)[i]
  pq_orange <- cbind(pq_orange, tmp)
}

pq_orange <- as.data.frame(pq_orange)
pq_orange <- pq_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
pq_sorghum <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$sorghum)*unique(prices$Sorghum)[i]
  pq_sorghum <- cbind(pq_sorghum, tmp)
}

pq_sorghum <- as.data.frame(pq_sorghum)
pq_sorghum <- pq_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
pq_soy <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$soybean)*unique(prices$Soy)[i]
  pq_soy <- cbind(pq_soy, tmp)
}

pq_soy <- as.data.frame(pq_soy)
pq_soy <- pq_soy %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
pq_tobacco <- NULL

for (i in 1:16) {
  tmp <- quantities_1995$tobacco*unique(prices$Tobacco)[i]
  pq_tobacco <- cbind(pq_tobacco, tmp)
}

pq_tobacco <- as.data.frame(pq_tobacco)
pq_tobacco <- pq_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
pq_sugarcane <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$sugar_cane)*unique(prices$Sugar)[i]
  pq_sugarcane <- cbind(pq_sugarcane, tmp)
}

pq_sugarcane <- as.data.frame(pq_sugarcane)
pq_sugarcane <- pq_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Rubber
pq_rubber <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$rubber)*unique(prices$Rubber)[i]
  pq_rubber <- cbind(pq_rubber, tmp)
}

pq_rubber <- as.data.frame(pq_rubber)
pq_rubber <- pq_rubber %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Tea Indian
pq_indiantea <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$indiantea)*unique(prices$Tea)[i]
  pq_indiantea <- cbind(pq_indiantea, tmp)
}

pq_indiantea <- as.data.frame(pq_indiantea)
pq_indiantea <- pq_indiantea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Tea Yerba
pq_yerbamate <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$yerba_mate)*unique(prices$Tea)[i]
  pq_yerbamate <- cbind(pq_yerbamate, tmp)
}

pq_yerbamate <- as.data.frame(pq_yerbamate)
pq_yerbamate <- pq_yerbamate %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat Hard
pq_wheat_hard <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$wheat)*unique(prices$`Wheat H`)[i]
  pq_wheat_hard <- cbind(pq_wheat_hard, tmp)
}

pq_wheat_hard <- as.data.frame(pq_wheat_hard)
pq_wheat_hard <- pq_wheat_hard %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Wheat Soft
pq_wheat_soft <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$wheat)*unique(prices$`Wheat S`)[i]
  pq_wheat_soft <- cbind(pq_wheat_soft, tmp)
}

pq_wheat_soft <- as.data.frame(pq_wheat_soft)
pq_wheat_soft <- pq_wheat_soft %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice 05
pq_rice_05 <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$rice)*unique(prices$`Rice 05`)[i]
  pq_rice_05 <- cbind(pq_rice_05, tmp)
}

pq_rice_05 <- as.data.frame(pq_rice_05)
pq_rice_05 <- pq_rice_05 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice A1
pq_rice_a1 <- NULL

for (i in 1:16) {
  tmp <- (quantities_1995$rice)*unique(prices$`Rice A1`)[i]
  pq_rice_a1 <- cbind(pq_rice_a1, tmp)
}

pq_rice_a1 <- as.data.frame(pq_rice_a1)
pq_rice_a1 <- pq_rice_a1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()




######### Tidying it up
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


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
                                  pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate))

# Summing all the values for each crop for all municipality
pq_final <- pq_aux %>% mutate(sum_pq = dplyr::select(., banana:yerbamate) %>% 
                                rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_pq)


pq_final_wider <- pq_final %>% pivot_wider(names_from = "year", values_from = "sum_pq", names_repair = "minimal") %>%
  add_column(cod = quantities_1995$cod, .before = "municip")


# Passing to longer format
pq_final_longer <- pq_final_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pq")


#Saving the dataset
save(pq_final_longer, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_bartik.Rdata")
write_xlsx(pq_final_wider, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_bartik.xlsx')
write_xlsx(pq_final_longer, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_deflate_longer.xlsx')


# Taking logs
pq_final_longer_log <- transform(pq_final_longer, pq = log(pq))
pq_final_longer_log <- as_tibble(pq_final_longer_log)

# Saving the dataset in Excel format
write_xlsx(pq_final_longer_log, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_deflate_longer_log.xlsx')







############## 2. Doing it again for the shares ################

#banana
pq_banana <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$banana)*unique(prices$Banana)[i]
  pq_banana <- cbind(pq_banana, tmp)
}

pq_banana <- as.data.frame(pq_banana)
pq_banana <- pq_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
pq_barley <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$barley)*unique(prices$Barley)[i]
  pq_barley <- cbind(pq_barley, tmp)
}

pq_barley <- as.data.frame(pq_barley)
pq_barley <- pq_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
pq_cocoa <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$cocoa)*unique(prices$Cocoa)[i]
  pq_cocoa <- cbind(pq_cocoa, tmp)
}

pq_cocoa <- as.data.frame(pq_cocoa)
pq_cocoa <- pq_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
pq_coffee_arabic <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$coffee)*unique(prices$`Coffee Arabic`)[i]
  pq_coffee_arabic <- cbind(pq_coffee_arabic, tmp)
}

pq_coffee_arabic <- as.data.frame(pq_coffee_arabic)
pq_coffee_arabic <- pq_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#coffee usando o preço do robust
pq_coffe_robust <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$coffee)*unique(prices$`Coffee Robust`)[i]
  pq_coffe_robust <- cbind(pq_coffe_robust, tmp)
}

pq_coffe_robust <- as.data.frame(pq_coffe_robust)
pq_coffe_robust <- pq_coffe_robust %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# #cotton_1
# pq_cotton_1 <- NULL
# 
# for (i in 1:16) {
#   tmp <- (quantities_1995$cotton_1)*unique(prices$Cotton)[i]
#   pq_cotton_1 <- cbind(pq_cotton_1, tmp)
# }
# 
# pq_cotton_1 <- as.data.frame(pq_cotton_1)
# pq_cotton_1 <- pq_cotton_1 %>% as_tibble(.name_repair = "unique") %>%
#   setNames(prices$Years) %>%
#   add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
#   as_tibble()
# 
# #cotton_2
# pq_cotton_2 <- NULL
# 
# for (i in 1:16) {
#   tmp <- (quantities_1995$cotton_2)*unique(prices$Cotton)[i]
#   pq_cotton_2 <- cbind(pq_cotton_2, tmp)
# }
# 
# pq_cotton_2 <- as.data.frame(pq_cotton_2)
# pq_cotton_2 <- pq_cotton_2 %>% as_tibble(.name_repair = "unique") %>%
#   setNames(prices$Years) %>%
#   add_column(municip = quantities_1995$municip, .before = "2000", .name_repair = "minimal") %>%
#   as_tibble()

#cotton_sum
pq_cotton <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$cotton)*unique(prices$Cotton)[i]
  pq_cotton <- cbind(pq_cotton, tmp)
}

pq_cotton <- as.data.frame(pq_cotton)
pq_cotton <- pq_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#maize
pq_maize <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$maize)*unique(prices$Maize)[i]
  pq_maize <- cbind(pq_maize, tmp)
}

pq_maize <- as.data.frame(pq_maize)
pq_maize <- pq_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
pq_orange <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$orange)*unique(prices$Orange)[i]
  pq_orange <- cbind(pq_orange, tmp)
}

pq_orange <- as.data.frame(pq_orange)
pq_orange <- pq_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
pq_sorghum <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$sorghum)*unique(prices$Sorghum)[i]
  pq_sorghum <- cbind(pq_sorghum, tmp)
}

pq_sorghum <- as.data.frame(pq_sorghum)
pq_sorghum <- pq_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
pq_soy <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$soybean)*unique(prices$Soy)[i]
  pq_soy <- cbind(pq_soy, tmp)
}

pq_soy <- as.data.frame(pq_soy)
pq_soy <- pq_soy %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
pq_tobacco <- NULL

for (i in 1:16) {
  tmp <- shares_1995$tobacco*unique(prices$Tobacco)[i]
  pq_tobacco <- cbind(pq_tobacco, tmp)
}

pq_tobacco <- as.data.frame(pq_tobacco)
pq_tobacco <- pq_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
pq_sugarcane <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$sugar_cane)*unique(prices$Sugar)[i]
  pq_sugarcane <- cbind(pq_sugarcane, tmp)
}

pq_sugarcane <- as.data.frame(pq_sugarcane)
pq_sugarcane <- pq_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Rubber
pq_rubber <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rubber)*unique(prices$Rubber)[i]
  pq_rubber <- cbind(pq_rubber, tmp)
}

pq_rubber <- as.data.frame(pq_rubber)
pq_rubber <- pq_rubber %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Tea Indian
pq_indiantea <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$indiantea)*unique(prices$Tea)[i]
  pq_indiantea <- cbind(pq_indiantea, tmp)
}

pq_indiantea <- as.data.frame(pq_indiantea)
pq_indiantea <- pq_indiantea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Tea Yerba
pq_yerbamate <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$yerba_mate)*unique(prices$Tea)[i]
  pq_yerbamate <- cbind(pq_yerbamate, tmp)
}

pq_yerbamate <- as.data.frame(pq_yerbamate)
pq_yerbamate <- pq_yerbamate %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat Hard
pq_wheat_hard <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$wheat)*unique(prices$`Wheat H`)[i]
  pq_wheat_hard <- cbind(pq_wheat_hard, tmp)
}

pq_wheat_hard <- as.data.frame(pq_wheat_hard)
pq_wheat_hard <- pq_wheat_hard %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Wheat Soft
pq_wheat_soft <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$wheat)*unique(prices$`Wheat S`)[i]
  pq_wheat_soft <- cbind(pq_wheat_soft, tmp)
}

pq_wheat_soft <- as.data.frame(pq_wheat_soft)
pq_wheat_soft <- pq_wheat_soft %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice 05
pq_rice_05 <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rice)*unique(prices$`Rice 05`)[i]
  pq_rice_05 <- cbind(pq_rice_05, tmp)
}

pq_rice_05 <- as.data.frame(pq_rice_05)
pq_rice_05 <- pq_rice_05 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice A1
pq_rice_a1 <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rice)*unique(prices$`Rice A1`)[i]
  pq_rice_a1 <- cbind(pq_rice_a1, tmp)
}

pq_rice_a1 <- as.data.frame(pq_rice_a1)
pq_rice_a1 <- pq_rice_a1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Tidying it up
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


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
                                  pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate))

# Summing all the values for each crop for all municipality
pq_final_shares <- pq_aux %>% mutate(sum_pq = dplyr::select(., banana:yerbamate) %>% 
                                rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_pq)


pq_final_shares_wider <- pq_final_shares %>% pivot_wider(names_from = "year", values_from = "sum_pq", names_repair = "minimal") %>%
  add_column(cod = shares_1995$cod, .before = "municip") %>%
  arrange(cod)

#Saving the dataset
write_xlsx(pq_final_shares_wider, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_shares_wide.xlsx')


# Passing to longer format
pq_final_shares_longer <- pq_final_shares_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pq") %>%
  arrange(cod)

#Saving
save(pq_final_shares_longer, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_shares_bartik.Rdata")


# Taking logs
pq_final_shares_longer_log <- transform(pq_final_shares_longer, pq = log(pq))
pq_final_shares_longer_log <- as_tibble(pq_final_shares_longer_log)

# Saving the dataset in Excel format
write_xlsx(pq_final_shares_longer, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_shares_long.xlsx')
write_xlsx(pq_final_shares_longer_log, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_shares_long_log.xlsx')




######### 3. Doing for shares and log prices to avoid non-linearities in SSIV #
prices[-1] <- log(prices[-1])

pq_banana <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$banana)*unique(prices$Banana)[i]
  pq_banana <- cbind(pq_banana, tmp)
}

pq_banana <- as.data.frame(pq_banana)
pq_banana <- pq_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
pq_barley <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$barley)*unique(prices$Barley)[i]
  pq_barley <- cbind(pq_barley, tmp)
}

pq_barley <- as.data.frame(pq_barley)
pq_barley <- pq_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
pq_cocoa <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$cocoa)*unique(prices$Cocoa)[i]
  pq_cocoa <- cbind(pq_cocoa, tmp)
}

pq_cocoa <- as.data.frame(pq_cocoa)
pq_cocoa <- pq_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
pq_coffee_arabic <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$coffee)*unique(prices$`Coffee Arabic`)[i]
  pq_coffee_arabic <- cbind(pq_coffee_arabic, tmp)
}

pq_coffee_arabic <- as.data.frame(pq_coffee_arabic)
pq_coffee_arabic <- pq_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#coffee usando o preço do robust
pq_coffe_robust <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$coffee)*unique(prices$`Coffee Robust`)[i]
  pq_coffe_robust <- cbind(pq_coffe_robust, tmp)
}

pq_coffe_robust <- as.data.frame(pq_coffe_robust)
pq_coffe_robust <- pq_coffe_robust %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cotton_sum
pq_cotton <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$cotton)*unique(prices$Cotton)[i]
  pq_cotton <- cbind(pq_cotton, tmp)
}

pq_cotton <- as.data.frame(pq_cotton)
pq_cotton <- pq_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#maize
pq_maize <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$maize)*unique(prices$Maize)[i]
  pq_maize <- cbind(pq_maize, tmp)
}

pq_maize <- as.data.frame(pq_maize)
pq_maize <- pq_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
pq_orange <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$orange)*unique(prices$Orange)[i]
  pq_orange <- cbind(pq_orange, tmp)
}

pq_orange <- as.data.frame(pq_orange)
pq_orange <- pq_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
pq_sorghum <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$sorghum)*unique(prices$Sorghum)[i]
  pq_sorghum <- cbind(pq_sorghum, tmp)
}

pq_sorghum <- as.data.frame(pq_sorghum)
pq_sorghum <- pq_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
pq_soy <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$soybean)*unique(prices$Soy)[i]
  pq_soy <- cbind(pq_soy, tmp)
}

pq_soy <- as.data.frame(pq_soy)
pq_soy <- pq_soy %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
pq_tobacco <- NULL

for (i in 1:16) {
  tmp <- shares_1995$tobacco*unique(prices$Tobacco)[i]
  pq_tobacco <- cbind(pq_tobacco, tmp)
}

pq_tobacco <- as.data.frame(pq_tobacco)
pq_tobacco <- pq_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
pq_sugarcane <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$sugar_cane)*unique(prices$Sugar)[i]
  pq_sugarcane <- cbind(pq_sugarcane, tmp)
}

pq_sugarcane <- as.data.frame(pq_sugarcane)
pq_sugarcane <- pq_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Rubber
pq_rubber <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rubber)*unique(prices$Rubber)[i]
  pq_rubber <- cbind(pq_rubber, tmp)
}

pq_rubber <- as.data.frame(pq_rubber)
pq_rubber <- pq_rubber %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Tea Indian
pq_indiantea <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$indiantea)*unique(prices$Tea)[i]
  pq_indiantea <- cbind(pq_indiantea, tmp)
}

pq_indiantea <- as.data.frame(pq_indiantea)
pq_indiantea <- pq_indiantea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Tea Yerba
pq_yerbamate <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$yerba_mate)*unique(prices$Tea)[i]
  pq_yerbamate <- cbind(pq_yerbamate, tmp)
}

pq_yerbamate <- as.data.frame(pq_yerbamate)
pq_yerbamate <- pq_yerbamate %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat Hard
pq_wheat_hard <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$wheat)*unique(prices$`Wheat H`)[i]
  pq_wheat_hard <- cbind(pq_wheat_hard, tmp)
}

pq_wheat_hard <- as.data.frame(pq_wheat_hard)
pq_wheat_hard <- pq_wheat_hard %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Wheat Soft
pq_wheat_soft <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$wheat)*unique(prices$`Wheat S`)[i]
  pq_wheat_soft <- cbind(pq_wheat_soft, tmp)
}

pq_wheat_soft <- as.data.frame(pq_wheat_soft)
pq_wheat_soft <- pq_wheat_soft %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice 05
pq_rice_05 <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rice)*unique(prices$`Rice 05`)[i]
  pq_rice_05 <- cbind(pq_rice_05, tmp)
}

pq_rice_05 <- as.data.frame(pq_rice_05)
pq_rice_05 <- pq_rice_05 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice A1
pq_rice_a1 <- NULL

for (i in 1:16) {
  tmp <- (shares_1995$rice)*unique(prices$`Rice A1`)[i]
  pq_rice_a1 <- cbind(pq_rice_a1, tmp)
}

pq_rice_a1 <- as.data.frame(pq_rice_a1)
pq_rice_a1 <- pq_rice_a1 %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = shares_1995$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Tidying it up
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


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(pq_banana, pq_barley, pq_cocoa, pq_coffee_arabic, pq_cotton, pq_indiantea, pq_maize, pq_orange, pq_rice_05,
                                  pq_rubber, pq_sorghum, pq_soy, pq_sugarcane, pq_tobacco, pq_wheat_soft, pq_yerbamate))

# Summing all the values for each crop for all municipality
pq_final_shares_log <- pq_aux %>% mutate(sum_pq = dplyr::select(., banana:yerbamate) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_pq)


pq_final_shares_wider_log <- pq_final_shares_log  %>% pivot_wider(names_from = "year", values_from = "sum_pq", names_repair = "minimal") %>%
  add_column(cod = shares_1995$cod, .before = "municip") %>%
  arrange(cod)

pq_final_shares_log <- pq_final_shares_wider_log %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pq_sum")

#Saving
save(pq_final_shares_log, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_shares_log.Rdata")

save(pq_final_shares_wider_log, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_shares_wider_log.Rdata")



