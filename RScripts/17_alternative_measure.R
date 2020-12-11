# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

memory.limit(size = NA)
memory.limit(size = 50000)

########## 1. Construct alternate measure, using the actual quantity shares for each year ##########################


# Set crops names
crops_names <- c("banana", "barley", "cocoa", "coffee", "cotton_1", "cotton_2", "indiantea", "maize", "oatmeal", 
                 "orange", "rice", "rubber", "sorghum", "soybean", "sugar_cane", "tobacco", "wheat", "yerba_mate")


# We need to reset the working directory in order to use the purr::map function below
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados_PAM/Dados_PAM_corr")

file.list <- list.files(pattern='*.xlsx')

# Makes a list of all the excel files
df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



# Cleaning function 
df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", starts_with("20")) %>%
    dplyr::select(-c("2016", "2017", "2018", "municip")) %>%
    mutate(cod = as.integer(cod)) %>%
    pivot_longer(-"cod", names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod)
}


df_list_actual <- map(.x = df_list_map, .f = df_fix)


actual_map <- reduce(df_list_actual, inner_join, by = c("cod", "year"))


quantities_actual <- actual_map %>% setNames(c("cod", "year", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

save(quantities_actual, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual.Rdata")

########## 2. Construct a measure for the share of each commodity in a given location in relation to the total produced quantity in that same location 

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual.Rdata")


cod_mun <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/cod_ibge/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx",
                      col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

cod_mun %<>% rename(cod = "Código Município Completo", municip = "Nome_Município") %>%
  dplyr::select(c("cod", "municip")) %>%
  mutate(cod = as.integer(cod))


quantities_actual %<>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber"))


quantities_actual %<>% mutate(total_quant = rowSums(.[3:16]))
quantities_actual <- inner_join(quantities_actual, cod_mun, by = "cod")


shares_actual <- lmap(quantities_actual[3:17], ~{.x/quantities_actual$total_quant})


shares_actual %<>% mutate(total_quant = rowSums(.[1:14])) %>%
  add_column(cod = quantities_actual$cod, .before = "banana") %>%
  add_column(municip = quantities_actual$municip, .before = "banana") %>%
  arrange %>%
  add_column(year = quantities_actual$year, .before = "banana")


save(shares_actual, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_actual.Rdata")


######### 3. Multiplying by prices and summing up ####################################

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_actual.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

prices <- cpi_prices_2010 %>% filter(Years >= 2000)
prices[-1] <- log(prices[-1])


## Merging
prices %<>% rename(year = "Years") %>% mutate(year = as.integer(year))

shares_actual %<>% mutate(year = as.integer(year))

aux <- inner_join(shares_actual, prices, by = "year")

# Mutate multiplying prices by shares
aux %<>% mutate(act_banana = banana*Banana) %>%
  mutate(act_barley = barley*Barley) %>%
  mutate(act_cocoa = cocoa*Cocoa) %>%
  mutate(act_coffee = coffee*`Coffee Arabic`) %>%
  mutate(act_cotton = cotton*Cotton) %>%
  mutate(act_maize = maize*Maize) %>%
  mutate(act_orange = orange*Orange) %>%
  mutate(act_rice = rice*`Rice 05`) %>%
  mutate(act_sorghum = sorghum*Sorghum) %>%
  mutate(act_soybean = soybean*Soy) %>%
  mutate(act_sugarcane = sugar_cane*Sugar) %>%
  mutate(act_tea = tea*Tea) %>%
  mutate(act_tobacco = tobacco*Tobacco) %>%
  mutate(act_wheat = wheat*`Wheat S`)
  
# Sum across crops to obtain measure by municipality-year

# Nice way to select and sum
actual_measure <- aux %>% mutate(sum_actual = dplyr::select(., act_banana:act_wheat) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(cod, municip, year, sum_actual)



# Merging

popstruc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/popstruc_pres.dta")

popstruc_actual <- inner_join(popstruc, actual_measure, by = c("cod", "year"))

# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(popstruc_actual, "popstruc_pres.dta")






### Adding price shocks
popstruc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/popstruc_pres.dta")

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.RData")









######### 4. AKM Correction ####################################################

### Take out sorghum

fao_mean <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_mean.dta") %>%
  dplyr::select(-c("oat_low", "oat_int", "oat_high", "rye_low", "rye_int", "rye_high", 
                   "sorghum_low", "sorghum_int", "sorghum_high"))

crops_names <- c("banana", "barley", "cocoa", "coffee", "cotton_1", "cotton_2", "indiantea", "maize", "oatmeal", 
                 "orange", "rice", "rubber", "sorghum", "soybean", "sugar_cane", "tobacco", "wheat", "yerba_mate")

setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados_PAM/Dados_PAM_corr")

file.list <- list.files(pattern='*.xlsx')

# Makes a list of all the excel files
df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Sets a function to clean and calculate the mean of each crop quantity for each municipality in the period 1995-1999
df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}




# Apply the function above to all datasets at once using map
df_list_mean <- map(.x = df_list_map, .f = df_fix)


# Merge all the crops files
quantities_map <- reduce(df_list_mean, merge, by = c("cod", "municip"))


# Produce a tidy dataset and takes out oatmeal, since it cannot be matched in the price dataset
quantities_1995 <- quantities_map %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-c("oatmeal", "sorghum")) %>%
  filter(cod >1)


#Save the quantities dataset
#save(quantities_1995_akm, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1995_bartik.Rdata")

quant_fao_1995 <- quantities_1995 %>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:15]))

shares_1995_akm <- lmap(quant_fao_1995[3:16], ~{.x/quant_fao_1995$total_quant})


#Tidying up
shares_1995_akm %<>% mutate(total_quant = rowSums(.[1:13])) %>%
  add_column(cod = quant_fao_1995$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1995$municip, .before = "banana") %>%
  arrange(cod)


#Saving 
#save(shares_1995, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/shares_1995_bartik.Rdata")


# Merging with FAO for Stata



fao_akm <- inner_join(shares_1995_akm, fao_mean, by = "cod")

#Writing to Stata
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(fao_akm, "fao_akm.dta")



### Creating the Measure ######################

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

prices <- cpi_prices_2010 %>% filter(Years >= 2000)
prices[-1] <- log(prices[-1])

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_pr_akm.dta")

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
  tmp <- (fao_pr$pr_orange)*unique(prices$Orange)[i]
  fao_orange <- cbind(fao_orange, tmp)
}

fao_orange <- as.data.frame(fao_orange)
fao_orange <- fao_orange %>% as_tibble(.name_repair = "unique") %>%
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
fao_soybean <- fao_soybean %>% pivot_longer(-municip, names_to = "year", values_to = "fao_soybean")
fao_sugarcane <- fao_sugarcane %>% pivot_longer(-municip, names_to = "year", values_to = "fao_sugarcane")
fao_tobacco <- fao_tobacco %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tobacco")
fao_wheat <- fao_wheat %>% pivot_longer(-municip, names_to = "year", values_to = "fao_wheat")
fao_tea <- fao_tea %>% pivot_longer(-municip, names_to = "year", values_to = "fao_tea")


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(fao_banana, fao_barley, fao_cocoa, fao_coffee_arabic, fao_cotton,
                                  fao_maize, fao_orange, fao_rice,
                                  fao_soybean, fao_sugarcane, fao_tobacco, fao_wheat, fao_tea))


# Summing all the values for each crop for all municipality
fao_final_index <- pq_aux %>% mutate(sum_fao_akm = dplyr::select(., fao_banana:fao_tea) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_fao_akm)


fao_final_index_wider <- fao_final_index %>% pivot_wider(names_from = "year", values_from = "sum_fao_akm", names_repair = "minimal") %>%
  add_column(cod = fao_pr$cod, .before = "municip") %>%
  arrange(cod)


fao_final <- fao_final_index_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "sum_fao_akm") %>%
  mutate(cod = as.integer(cod), year = as.integer(year)) %>%
  arrange(cod)

akm_fao_shares <- fao_pr %>% dplyr::select(c("cod", "municip", "pr_banana", "pr_barley", "pr_orange", 
                                  "pr_cocoa", "pr_coffee", "pr_cotton", "pr_maize",
                                  "pr_rice", "pr_soybean", "pr_sugarcane", "pr_tea", 
                                  "pr_tobacco", "pr_wheat"))

akm_fao_shares %<>% set_names(c("cod", "municip", "akm_banana", "akm_barley", "akm_orange", 
                                "akm_cocoa", "akm_coffee", "akm_cotton", "akm_maize",
                                "akm_rice", "akm_soybean", "akm_sugarcane", "akm_tea", 
                                "akm_tobacco", "akm_wheat")) %>% as_tibble() %>%
  mutate(cod = as.integer(cod))



# Saving the dataset
#write_xlsx(fao_final_index_wider, 'C:/Users/Andrei/Desktop/Dissertation/Analysis/fao_final_index_wider.xlsx')

# Saving
#save(fao_final_index, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/fao_final_index_widerx.Rdata")
#save(fao_final, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")


# Merging with popstruc

fao_final <- inner_join(fao_final, akm_fao_shares, by = "cod")

popstruc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/popstruc_pres.dta")

popstruc_akm <- inner_join(popstruc, fao_final, by = c("cod", "year"))

# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(popstruc_akm , "popstruc_pres.dta")




######### 5. Actual Shares #####################################################





  