# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packages
source("./00_load_packages.R")



######### 1. Read and explore FAO-GAEZ dataset #################################

#Reading all the ASCII grid files
list_files_low = list.files("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/FAO-GAEZ/ASCII Grid Data/Low",
                        full.names = TRUE)

list_files_int = list.files("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/FAO-GAEZ/ASCII Grid Data/Intermediate",
                            full.names = TRUE)

list_files_high = list.files("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/FAO-GAEZ/ASCII Grid Data/High",
                            full.names = TRUE)

low <- map(list_files_low, raster)
int <- map(list_files_int, raster)
high <- map(list_files_high, raster)

#Rename the list elements
names_low <- c("banana_low", "barley_low", "citrus_low", "cocoa_low", "coffee_low", "cotton_low", 
               "maize_low", "oat_low", "rice_low", "rye_low", "sorghum_low", "soybean_low", "sugarcane_low",
               "tea_low", "tobacco_low", "wheat_low")

names_int <- c("banana_int", "barley_int", "citrus_int", "cocoa_int", "coffee_int", "cotton_int", 
               "maize_int", "oat_int", "rice_int", "rye_int", "sorghum_int", "soybean_int", "sugarcane_int",
               "tea_int", "tobacco_int", "wheat_int")

names_high <- c("banana_high", "barley_high", "citrus_high", "cocoa_high", "coffee_high", "cotton_high", 
               "maize_high", "oat_high", "rice_high", "rye_high", "sorghum_high", "soybean_high", "sugarcane_high",
               "tea_high", "tobacco_high", "wheat_high")


names(low) <- names_low
names(int) <- names_int
names(high) <- names_high


#Reads shapefile for municipalities borders
shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Analysis/Shapefiles/br_municipios_2019", "BR_Municipios_2019",
                     stringsAsFactors = F)


#Extract the values
low <- low %>% map(~replace(., is.na(.), 0))
low_brick <- brick(low)
low_mean <- extract(low_brick, shp_ibge, fun = mean, sp = TRUE)
low_sum <- extract(low_brick, shp_ibge, fun = sum, sp = TRUE)
low_mean <- low_mean@data %>% as_tibble()
low_sum <- low_sum@data %>% as_tibble()


int <- int %>% map(~replace(., is.na(.), 0))
int_brick <- brick(int)
int_mean <- extract(int_brick, shp_ibge, fun = mean, sp = TRUE)
int_sum <- extract(int_brick, shp_ibge, fun = sum, sp = TRUE)
int_mean <- int_mean@data %>% as_tibble()
int_sum <- int_sum@data %>% as_tibble()


high <- high %>% map(~replace(., is.na(.), 0))
high_brick <- brick(high)
high_mean <- extract(high_brick, shp_ibge, fun = mean, sp = TRUE)
high_sum <- extract(high_brick, shp_ibge, fun = sum, sp = TRUE)
high_mean <- high_mean@data %>% as_tibble()
high_sum <- high_sum@data %>% as_tibble()


#Writing to Stata
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

tmp <- full_join(low_mean, int_mean)

fao_mean <- full_join(tmp, high_mean) %>%
  rename(cod = "CD_MUN")

write.dta(fao_mean, "fao_mean.dta")




###################
fao_mean <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_mean.dta") %>%
  dplyr::select(-c("oat_low", "oat_int", "oat_high", "rye_low", "rye_int", "rye_high"))


load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_1995.Rdata")


full_fao <- inner_join(shares_fao_1995, fao_mean, by = "cod")


write.dta(full_fao, "full_fao.dta")


######### 2. Getting back from STATA and constructing the index ################

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

prices <- cpi_prices_2010 %>% filter(Years >= 2000)
prices[-1] <- log(prices[-1])

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao.dta")

#banana
fao_banana <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_banana)*unique(prices$Banana)[i]
  fao_banana <- cbind(fao_banana, tmp)
}

fao_banana <- as.data.frame(fao_banana)
fao_banana <- fao_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
fao_barley <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_barley)*unique(prices$Barley)[i]
  fao_barley <- cbind(fao_barley, tmp)
}

fao_barley <- as.data.frame(fao_barley)
fao_barley <- fao_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
fao_cocoa <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_cocoa)*unique(prices$Cocoa)[i]
  fao_cocoa <- cbind(fao_cocoa, tmp)
}

fao_cocoa <- as.data.frame(fao_cocoa)
fao_cocoa <- fao_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
fao_coffee_arabic <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_coffee)*unique(prices$`Coffee Arabic`)[i]
  fao_coffee_arabic <- cbind(fao_coffee_arabic, tmp)
}

fao_coffee_arabic <- as.data.frame(fao_coffee_arabic)
fao_coffee_arabic <- fao_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



# cotton
fao_cotton <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_cotton)*unique(prices$Cotton)[i]
  fao_cotton <- cbind(fao_cotton, tmp)
}

fao_cotton <- as.data.frame(fao_cotton)
fao_cotton <- fao_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# maize
fao_maize <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_maize)*unique(prices$Maize)[i]
  fao_maize <- cbind(fao_maize, tmp)
}

fao_maize <- as.data.frame(fao_maize)
fao_maize <- fao_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#orange
fao_orange <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pq_orange)*unique(prices$Orange)[i]
  fao_orange <- cbind(fao_orange, tmp)
}

fao_orange <- as.data.frame(fao_orange)
fao_orange <- fao_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
fao_sorghum <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_sorghum)*unique(prices$Sorghum)[i]
  fao_sorghum <- cbind(fao_sorghum, tmp)
}

fao_sorghum <- as.data.frame(fao_sorghum)
fao_sorghum <- fao_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
fao_soybean <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_soybean)*unique(prices$Soy)[i]
  fao_soybean <- cbind(fao_soybean, tmp)
}

fao_soybean <- as.data.frame(fao_soybean)
fao_soybean <- fao_soybean %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
fao_tobacco <- NULL

for (i in 1:16) {
  tmp <- fao_pr$pr_tobacco*unique(prices$Tobacco)[i]
  fao_tobacco <- cbind(fao_tobacco, tmp)
}

fao_tobacco <- as.data.frame(fao_tobacco)
fao_tobacco <- fao_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
fao_sugarcane <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_sugarcane)*unique(prices$Sugar)[i]
  fao_sugarcane <- cbind(fao_sugarcane, tmp)
}

fao_sugarcane <- as.data.frame(fao_sugarcane)
fao_sugarcane <- fao_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



#Tea
fao_tea <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_tea)*unique(prices$Tea)[i]
  fao_tea <- cbind(fao_tea, tmp)
}

fao_tea <- as.data.frame(fao_tea)
fao_tea <- fao_tea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat
fao_wheat <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_wheat)*unique(prices$`Wheat S`)[i]
  fao_wheat <- cbind(fao_wheat, tmp)
}

fao_wheat <- as.data.frame(fao_wheat)
fao_wheat <- fao_wheat %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice
fao_rice <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_rice)*unique(prices$`Rice 05`)[i]
  fao_rice <- cbind(fao_rice, tmp)
}

fao_rice <- as.data.frame(fao_rice)
fao_rice <- fao_rice %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()




# Tidying it up
fao_banana <- fao_banana %>% pivot_longer(-municip, names_to = "year", values_to = "fao_banana")
fao_barley <- fao_barley %>% pivot_longer(-municip, names_to = "year", values_to = "fao_barley")
fao_cocoa <- fao_cocoa %>% pivot_longer(-municip, names_to = "year", values_to = "fao_cocoa")
fao_coffee_arabic <- fao_coffee_arabic %>% pivot_longer(-municip, names_to = "year", values_to = "fao_coffee")
fao_cotton <- fao_cotton %>% pivot_longer(-municip, names_to = "year", values_to = "fao_cotton")
fao_maize <- fao_maize %>% pivot_longer(-municip, names_to = "year", values_to = "fao_maize")
fao_orange <- fao_orange %>% pivot_longer(-municip, names_to = "year", values_to = "fao_orange")
fao_rice <- fao_rice %>% pivot_longer(-municip, names_to = "year", values_to = "fao_rice")
fao_sorghum <- fao_sorghum %>% pivot_longer(-municip, names_to = "year", values_to = "fao_sorghum")
fao_soybean <- fao_soybean %>% pivot_longer(-municip, names_to = "year", values_to = "fao_soybean")
fao_sugarcane <- fao_sugarcane %>% pivot_longer(-municip, names_to = "year", values_to = "fao_sugarcane")
fao_tobacco <- fao_tobacco %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tobacco")
fao_wheat <- fao_wheat %>% pivot_longer(-municip, names_to = "year", values_to = "fao_wheat")
fao_tea <- fao_tea %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tea")


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(fao_banana, fao_barley, fao_cocoa, fao_coffee_arabic, fao_cotton,
                                  fao_maize, fao_orange, fao_rice,
                                  fao_sorghum, fao_soybean, fao_sugarcane, fao_tobacco, fao_wheat, fao_tea))


# Summing all the values for each crop for all municipality
fao_final_index <- pq_aux %>% mutate(sum_fao = dplyr::select(., fao_banana:fao_tea) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_fao)


fao_final_index_wider <- fao_final_index %>% pivot_wider(names_from = "year", values_from = "sum_fao", names_repair = "minimal") %>%
  add_column(cod = fao_pr$cod, .before = "municip") %>%
  arrange(cod)


fao_final <- fao_final_index_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "sum_fao") %>%
  mutate(cod = as.integer(cod), year = as.integer(year)) %>%
  arrange(cod)

# Saving the dataset
#write_xlsx(fao_final_index_wider, 'C:/Users/Andrei/Desktop/Dissertation/Analysis/fao_final_index_wider.xlsx')

# Saving
save(fao_final_index, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/fao_final_index_wider.Rdata")
save(fao_final, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")

##########################################################################################

# Baseline for most things
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/controls_baseline.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/pq_shares_log.Rdata")

pq_final_shares_log %<>% mutate(cod = as.integer(cod)) %>% mutate(year = as.integer(year))


baseline <- inner_join(fao_final, controls_baseline, by = "cod")

baseline <- inner_join(baseline, pq_final_shares_log, by = c("cod", "year"))


# Load land gini
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")


land_gini %<>% mutate(year = ifelse(year==1995, 2000, year)) %>%
  mutate(year = ifelse(year==2017, 2015, year))

baseline_gini <- full_join(baseline, land_gini, by = c("cod", "year"))
save(baseline_gini, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_gini.Rdata")






#########################################################################################################

tmp2 <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/municip_pib_final.dta")

fao_pib <- inner_join(fao_final, tmp2, by = c("cod", "year"))


setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(fao_pib, "fao_pib.dta")


#########

tmp3 <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/andrei_tmp.dta")

fao_andrei <- inner_join(fao_final, tmp3, by = c("cod", "year"))


setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(fao_andrei, "fao_andrei.dta")

######### Full Bartik

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/pq_bartik_final.Rdata")


bartik_final <- full_join(fao_final, dplyr::select(pq_bartik_final, -c("municip")), by = c("cod", "year"))


save(bartik_final, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/bartik_final.Rdata")






################## 3. Measure using FAO high inputs ############################

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

prices <- cpi_prices_2010 %>% filter(Years >= 2000)
prices[-1] <- log(prices[-1])

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao_high.dta")

#banana
fao_banana <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_banana_high)*unique(prices$Banana)[i]
  fao_banana <- cbind(fao_banana, tmp)
}

fao_banana <- as.data.frame(fao_banana)
fao_banana <- fao_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#barley
fao_barley <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_barley_high)*unique(prices$Barley)[i]
  fao_barley <- cbind(fao_barley, tmp)
}

fao_barley <- as.data.frame(fao_barley)
fao_barley <- fao_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
fao_cocoa <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_cocoa_high)*unique(prices$Cocoa)[i]
  fao_cocoa <- cbind(fao_cocoa, tmp)
}

fao_cocoa <- as.data.frame(fao_cocoa)
fao_cocoa <- fao_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
fao_coffee_arabic <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_coffee_high)*unique(prices$`Coffee Arabic`)[i]
  fao_coffee_arabic <- cbind(fao_coffee_arabic, tmp)
}

fao_coffee_arabic <- as.data.frame(fao_coffee_arabic)
fao_coffee_arabic <- fao_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



# cotton
fao_cotton <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_cotton_high)*unique(prices$Cotton)[i]
  fao_cotton <- cbind(fao_cotton, tmp)
}

fao_cotton <- as.data.frame(fao_cotton)
fao_cotton <- fao_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# maize
fao_maize <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_maize_high)*unique(prices$Maize)[i]
  fao_maize <- cbind(fao_maize, tmp)
}

fao_maize <- as.data.frame(fao_maize)
fao_maize <- fao_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# orange
fao_orange <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_orange_high)*unique(prices$Orange)[i]
  fao_orange <- cbind(fao_orange, tmp)
}

fao_orange <- as.data.frame(fao_orange)
fao_orange <- fao_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# sorghum
fao_sorghum <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_sorghum_high)*unique(prices$Sorghum)[i]
  fao_sorghum <- cbind(fao_sorghum, tmp)
}

fao_sorghum <- as.data.frame(fao_sorghum)
fao_sorghum <- fao_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Soy
fao_soybean <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_soybean_high)*unique(prices$Soy)[i]
  fao_soybean <- cbind(fao_soybean, tmp)
}

fao_soybean <- as.data.frame(fao_soybean)
fao_soybean <- fao_soybean %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
fao_tobacco <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_tobacco_high)*unique(prices$Tobacco)[i]
  fao_tobacco <- cbind(fao_tobacco, tmp)
}

fao_tobacco <- as.data.frame(fao_tobacco)
fao_tobacco <- fao_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
fao_sugarcane <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_sugarcane_high)*unique(prices$Sugar)[i]
  fao_sugarcane <- cbind(fao_sugarcane, tmp)
}

fao_sugarcane <- as.data.frame(fao_sugarcane)
fao_sugarcane <- fao_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



#Tea
fao_tea <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_tea_high)*unique(prices$Tea)[i]
  fao_tea <- cbind(fao_tea, tmp)
}

fao_tea <- as.data.frame(fao_tea)
fao_tea <- fao_tea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Wheat
fao_wheat <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_wheat_high)*unique(prices$`Wheat S`)[i]
  fao_wheat <- cbind(fao_wheat, tmp)
}

fao_wheat <- as.data.frame(fao_wheat)
fao_wheat <- fao_wheat %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


#Rice
fao_rice <- NULL

for (i in 1:16) {
  tmp <- (fao_pr$pr_rice_high)*unique(prices$`Rice 05`)[i]
  fao_rice <- cbind(fao_rice, tmp)
}

fao_rice <- as.data.frame(fao_rice)
fao_rice <- fao_rice %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()




# Tidying it up
fao_banana <- fao_banana %>% pivot_longer(-municip, names_to = "year", values_to = "fao_banana")
fao_barley <- fao_barley %>% pivot_longer(-municip, names_to = "year", values_to = "fao_barley")
fao_cocoa <- fao_cocoa %>% pivot_longer(-municip, names_to = "year", values_to = "fao_cocoa")
fao_coffee_arabic <- fao_coffee_arabic %>% pivot_longer(-municip, names_to = "year", values_to = "fao_coffee")
fao_cotton <- fao_cotton %>% pivot_longer(-municip, names_to = "year", values_to = "fao_cotton")
fao_maize <- fao_maize %>% pivot_longer(-municip, names_to = "year", values_to = "fao_maize")
fao_orange <- fao_orange %>% pivot_longer(-municip, names_to = "year", values_to = "fao_orange")
fao_rice <- fao_rice %>% pivot_longer(-municip, names_to = "year", values_to = "fao_rice")
fao_sorghum <- fao_sorghum %>% pivot_longer(-municip, names_to = "year", values_to = "fao_sorghum")
fao_soybean <- fao_soybean %>% pivot_longer(-municip, names_to = "year", values_to = "fao_soybean")
fao_sugarcane <- fao_sugarcane %>% pivot_longer(-municip, names_to = "year", values_to = "fao_sugarcane")
fao_tobacco <- fao_tobacco %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tobacco")
fao_wheat <- fao_wheat %>% pivot_longer(-municip, names_to = "year", values_to = "fao_wheat")
fao_tea <- fao_tea %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tea")


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(fao_banana, fao_barley, fao_cocoa, fao_coffee_arabic, fao_cotton,
                                  fao_maize, fao_orange, fao_rice,
                                  fao_sorghum, fao_soybean, fao_sugarcane, fao_tobacco, fao_wheat, fao_tea))


# Summing all the values for each crop for all municipality
fao_final_index <- pq_aux %>% mutate(sum_fao = dplyr::select(., fao_banana:fao_tea) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_fao)


fao_final_index_wider <- fao_final_index %>% pivot_wider(names_from = "year", values_from = "sum_fao", names_repair = "minimal") %>%
  add_column(cod = fao_pr$cod, .before = "municip") %>%
  arrange(cod)


fao_final <- fao_final_index_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "sum_fao_high") %>%
  mutate(cod = as.integer(cod), year = as.integer(year)) %>%
  arrange(cod)


# Saving
save(fao_final_index, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/fao_final_index_wider.Rdata")
save(fao_final, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")










# #Checking
# #maize and soy
# maize_low <- raster ("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/FAO-GAEZ/ASCII Grid Data/Low/maize_low.asc")
# soy_low <- raster ("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/FAO-GAEZ/ASCII Grid Data/Low/soybean_low.asc")
# 
# 
# maize_low[is.na(maize_low[])] <- 0
# soy_low[is.na(soy_low[])] <- 0
# 
# 
# teste_maize_1 <- extract(brick(maize_low), shp_ibge, fun = mean, sp = TRUE)
# 
# teste_maize_1 <- teste_maize_1 %>% as_tibble(teste_maize_1@data)
# 
# 
# teste_maize_2 <- extract(brick(maize_low), shp_ibge, fun = sum, sp = TRUE)
# 
# teste_maize_2 <- teste_maize_2 %>% as_tibble(teste_maize_2@data)
# 
# 
# teste_soy_1 <- extract(brick(soy_low), shp_ibge, fun = mean, sp = TRUE)
# 
# teste_soy_1 <- teste_soy_1 %>% as_tibble(teste_soy_1@data)
# 
# 
# teste_soy_2 <- extract(brick(soy_low), shp_ibge, fun = sum, sp = TRUE)
# 
# teste_soy_2 <- teste_soy_2 %>% as_tibble(teste_soy_2@data)






####################################################################################################################

# #Plotting the raw data
# plot(maize_low, main = "Potential Yields for Maize - Low Inputs")
# plot(soy_low, main = "Potential Yields for Soy - Low Inputs")


# #Reads shapefile for state borders
# estados_shp <- readOGR("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/uf_2019", "BR_UF_2019", stringsAsFactors = F)
# 
# plot(estados_shp,
#      main = "Brazillian State Borders",
#      axes = TRUE,
#      border = "blue")
# 
# 
# #Plot raster file on top of state borders
# 
# ##Cuts the raster file to match the states coordinates
# aux <-  crop(maize_low, estados_shp)
# 
# plot(aux, main = "Maize Potential Yields")
# 
# plot(estados_shp, add = TRUE)
# 
# 
# #Reads shapefile for municipalities borders
# shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)
# 
# ##Plot raster file on top of municipalities borders (demora alguns minutinhos)
# plot(aux, main = "Maize Potential Yields")
# plot(shp_ibge, add = TRUE)
# 
# 
# #Uses velox package to compute the mean and sum value for each pixel inside each municipality
# options(buildtools.check=NULL)
# 
# library(devtools)
# install_github("hunzikp/velox")
# 
# library(velox)
# maize_low_vx <- velox("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/maize_low.asc")
# soy_low_vx <- velox("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/soybean_low.asc")
# 
# teste1 <- maize_low_vx$extract(shp_ibge, fun = mean)
# teste2 <- maize_low_vx$extract(shp_ibge, fun = sum)
# 
# teste3 <- soy_low_vx$extract(shp_ibge, fun = mean)
# teste4 <- soy_low_vx$extract(shp_ibge, fun = sum)
# 
# 
# teste_aa <- soy_low_vx$extract(shp_ibge, fun = function(t) sum(t, na.rm = TRUE))
# 
# 
# #retira valores NA
# 
# maize_low[is.na(maize_low[])] <- 0
# 
# maize_low_vx <- velox(maize_low)
# 
# teste <- maize_low_vx$extract(estados_shp, fun = sum)
# teste2 <- maize_low_vx$extract(estados_shp, fun = mean)
# 
# 
# 
# #Testing projection on the raster file
# 
# shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/FAO-GAEZ/Example/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)
# 
# crs(maize_low) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
# 
# plot(maize_low)
# 
# 
# p = extract(brick(maize_low), shp_ibge, fun = mean, na.rm = TRUE, sp = TRUE)
# 
# p = extract(brick(soy_low), shp_ibge, fun = mean, na.rm = TRUE, sp = TRUE)



