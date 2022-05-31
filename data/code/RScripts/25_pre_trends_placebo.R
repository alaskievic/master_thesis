# Load packages
source("./00_load_packages.R")

########## 1. Clean and caculate the mean quantities for each crop in each
##########    municipality between 1995-1999

# Set crops names
crops_names <- c("banana", "barley", "cattle", "cocoa", "coffee", "cotton_1",
                 "cotton_2", "indiantea", "maize", "oatmeal", 
                 "orange", "rice", "rubber", "sorghum", "soybean", "sugar_cane",
                 "tobacco", "wheat", "yerba_mate")

# Read all the files one by one just to store them if necessary
banana <- read_excel(here("data", "raw", "pam", "pam_corr", "banana.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

barley <- read_excel(here("data", "raw", "pam", "pam_corr", "barley.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

cocoa <- read_excel(here("data", "raw", "pam", "pam_corr", "cocoa.xlsx"),
                    sheet = "Tabela", col_names = TRUE,
                    na = c("NA","N/A","", "...", "-", "..", "X"))

coffee <- read_excel(here("data", "raw", "pam", "pam_corr", "coffee.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

cotton_1 <- read_excel(here("data", "raw", "pam", "pam_corr", "cotton_1.xlsx"),
                       sheet = "Tabela", col_names = TRUE,
                       na = c("NA","N/A","", "...", "-", "..", "X"))

cotton_2 <- read_excel(here("data", "raw", "pam", "pam_corr", "cotton_2.xls"),
                       sheet = "Tabela", col_names = TRUE,
                       na = c("NA","N/A","", "...", "-", "..", "X"))

indiantea <- read_excel(here("data", "raw", "pam", "pam_corr", "indiantea.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))

maize <- read_excel(here("data", "raw", "pam", "pam_corr", "maize.xlsx"),
                    sheet = "Tabela", col_names = TRUE,
                    na = c("NA","N/A","", "...", "-", "..", "X"))

oatmeal <- read_excel(here("data", "raw", "pam", "pam_corr", "oatmeal.xlsx"),
                      sheet = "Tabela", col_names = TRUE,
                      na = c("NA","N/A","", "...", "-", "..", "X"))

orange <- read_excel(here("data", "raw", "pam", "pam_corr", "orange.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

rice <- read_excel(here("data", "raw", "pam", "pam_corr", "rice.xlsx"),
                   sheet = "Tabela", col_names = TRUE,
                   na = c("NA","N/A","", "...", "-", "..", "X"))

rubber <- read_excel(here("data", "raw", "pam", "pam_corr", "rubber.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

sorghum <- read_excel(here("data", "raw", "pam", "pam_corr", "sorghums.xlsx"),
                      sheet = "Tabela", col_names = TRUE,
                      na = c("NA","N/A","", "...", "-", "..", "X"))

soybean <- read_excel(here("data", "raw", "pam", "pam_corr", "soybeans.xlsx"),
                                 sheet = "Tabela", col_names = TRUE,
                                 na = c("NA","N/A","", "...", "-", "..", "X"))
                      
sugar_cane <- read_excel(here("data", "raw", "pam", "pam_corr", "sugar_cane.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))

tobacco <- read_excel(here("data", "raw", "pam", "pam_corr", "tobacco.xlsx"),
                      sheet = "Tabela", col_names = TRUE,
                      na = c("NA","N/A","", "...", "-", "..", "X"))

wheat <- read_excel(here("data", "raw", "pam", "pam_corr", "wheat.xlsx"),
                    sheet = "Tabela", col_names = TRUE,
                    na = c("NA","N/A","", "...", "-", "..", "X"))

yerba_mate <- read_excel(here("data", "raw", "pam", "pam_corr", "yerba_mate.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))

cattle <- read_excel(here("data", "raw", "pam", "pam_corr", "cattle.xlsx"),
                     sheet = "Tabela", col_names = TRUE,
                     na = c("NA","N/A","", "...", "-", "..", "X"))

######## Now a more convenient way to read all the files
# We need to reset the working directory in order to use the purr::map function below
setwd(here("data", "raw", "pam", "pam_corr"))

file.list <- list.files(pattern='*.xlsx')

# Makes a list of all the excel files
df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE,
                   na = c("NA","N/A","", "...", "-", "..", "X"))



# Sets a function to clean and calculate the mean of each crop quantity for each
# municipality in the period 1985-1989
df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "1985" ,"1986" ,"1987", "1988", "1989") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}

#Same function for the period 1990-2000
df_fix_2 = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip","1990" ,"1991" ,"1992" ,"1993" ,"1994",
                  "1995" ,"1996" ,"1997", "1998", "1999", "2000") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}




# Apply the function above to all datasets at once using map
df_list_mean_1985 <- map(.x = df_list_map, .f = df_fix)

df_list_mean_1990 <- map(.x = df_list_map, .f = df_fix_2)


# Merge all the crops files
quantities_map_1985 <- reduce(df_list_mean_1985, merge, by = c("cod", "municip"))

quantities_map_1990 <- reduce(df_list_mean_1990, merge, by = c("cod", "municip"))



# Produce a tidy dataset and takes out oatmeal, since it cannot be matched in the price dataset
quantities_1985 <- quantities_map_1985 %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

quantities_1990 <- quantities_map_1990 %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)


# Multiplying cattle by medium weight (255kg) to transform it in tons
quantities_1985 %<>% mutate(cattle = (cattle*255)/1000)

quantities_1990 %<>% mutate(cattle = (cattle*255)/1000)


#Save the quantities dataset
save(quantities_1985, file = here("data", "output", "pam_shares", "quantities_1985_placebo.RData"))

save(quantities_1990, file = here("data", "output", "pam_shares", "quantities_1990_placebo.RData"))


########## 2. Construct a measure for the share of each commodity in a given
##########    location in relation to the total produced quantity in that same location 

load(here("data", "output", "pam_shares", "quantities_1985_placebo.RData"))
load(here("data", "output", "pam_shares", "quantities_1990_placebo.RData"))


# Sum over all quantities
quant_fao_1985 <- quantities_1985 %>% 
  mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))


quant_fao_1990 <- quantities_1990 %>%
  mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))





# Construct shares
shares_1985_placebo <- lmap(quant_fao_1985[3:18], ~{.x/quant_fao_1985$total_quant})
shares_1990_placebo <- lmap(quant_fao_1990[3:18], ~{.x/quant_fao_1990$total_quant})



# Tidying up
shares_1985_placebo %<>%
  add_column(cod = quant_fao_1985$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1985$municip, .before = "banana") %>%
  arrange(cod)  %>% mutate(cod = as.integer(cod))

shares_1990_placebo %<>%
  add_column(cod = quant_fao_1990$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1990$municip, .before = "banana") %>%
  arrange(cod) %>% mutate(cod = as.integer(cod))


#Saving
save(shares_1985_placebo, file = here("data", "output", "pam_shares", "shares_1985_placebo.RData"))

save(shares_1990_placebo, file = here("data", "output", "pam_shares", "shares_1990_placebo.RData"))




######################## 2. Placebo Prices #####################################



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



###### Merging for FAO

full_fao <- read_dta(here("data", "output", "fao_gaez", "full_fao_cat_1995.dta"))

full_fao %<>% dplyr::select(cod, NM_MUN:grass_low)


load(here("data", "output", "pam_shares", "shares_1985_placebo.RData"))


full_fao <- inner_join(shares_1985_placebo, full_fao, by = "cod")


write_dta(full_fao, path = here("data", "output", "fao_gaez", "shares_to_fao_placebo.dta"))






####################### 3. Placebo Exposure Measure ############################
load(here("data", "output", "prices", "prices_real_bartik_2010.Rdata"))

prices <- cpi_prices_2010 %>% filter(Years <= 2000)
prices[-1] <- log(prices[-1])

fao_pr <- read_dta(here("data", "output", "fao_gaez", "fao_pr_placebo_1985.dta"))

#banana
fao_banana <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_banana)*unique(prices$Banana)[i]
  fao_banana <- cbind(fao_banana, tmp)
}

fao_banana <- as.data.frame(fao_banana)
fao_banana <- fao_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#barley
fao_barley <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_barley)*unique(prices$Barley)[i]
  fao_barley <- cbind(fao_barley, tmp)
}

fao_barley <- as.data.frame(fao_barley)
fao_barley <- fao_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#cocoa
fao_cocoa <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_cocoa)*unique(prices$Cocoa)[i]
  fao_cocoa <- cbind(fao_cocoa, tmp)
}

fao_cocoa <- as.data.frame(fao_cocoa)
fao_cocoa <- fao_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#coffee usando preço do arabic
fao_coffee_arabic <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_coffee)*unique(prices$`Coffee Arabic`)[i]
  fao_coffee_arabic <- cbind(fao_coffee_arabic, tmp)
}

fao_coffee_arabic <- as.data.frame(fao_coffee_arabic)
fao_coffee_arabic <- fao_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()



# cotton
fao_cotton <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_cotton)*unique(prices$Cotton)[i]
  fao_cotton <- cbind(fao_cotton, tmp)
}

fao_cotton <- as.data.frame(fao_cotton)
fao_cotton <- fao_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()


# maize
fao_maize <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_maize)*unique(prices$Maize)[i]
  fao_maize <- cbind(fao_maize, tmp)
}

fao_maize <- as.data.frame(fao_maize)
fao_maize <- fao_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#orange
fao_orange <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_orange)*unique(prices$Orange)[i]
  fao_orange <- cbind(fao_orange, tmp)
}

fao_orange <- as.data.frame(fao_orange)
fao_orange <- fao_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#sorghum
fao_sorghum <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_sorghum)*unique(prices$Sorghum)[i]
  fao_sorghum <- cbind(fao_sorghum, tmp)
}

fao_sorghum <- as.data.frame(fao_sorghum)
fao_sorghum <- fao_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#Soy
fao_soybean <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_soybean)*unique(prices$Soy)[i]
  fao_soybean <- cbind(fao_soybean, tmp)
}

fao_soybean <- as.data.frame(fao_soybean)
fao_soybean <- fao_soybean %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()


#Tobacco
fao_tobacco <- NULL

for (i in 1:11) {
  tmp <- fao_pr$pr_tobacco*unique(prices$Tobacco)[i]
  fao_tobacco <- cbind(fao_tobacco, tmp)
}

fao_tobacco <- as.data.frame(fao_tobacco)
fao_tobacco <- fao_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()

#Sugar
fao_sugarcane <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_sugarcane)*unique(prices$Sugar)[i]
  fao_sugarcane <- cbind(fao_sugarcane, tmp)
}

fao_sugarcane <- as.data.frame(fao_sugarcane)
fao_sugarcane <- fao_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()



#Tea
fao_tea <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_tea)*unique(prices$Tea)[i]
  fao_tea <- cbind(fao_tea, tmp)
}

fao_tea <- as.data.frame(fao_tea)
fao_tea <- fao_tea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()


#Wheat
fao_wheat <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_wheat)*unique(prices$`Wheat S`)[i]
  fao_wheat <- cbind(fao_wheat, tmp)
}

fao_wheat <- as.data.frame(fao_wheat)
fao_wheat <- fao_wheat %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
  as_tibble()


#Rice
fao_rice <- NULL

for (i in 1:11) {
  tmp <- (fao_pr$pr_rice)*unique(prices$`Rice 05`)[i]
  fao_rice <- cbind(fao_rice, tmp)
}

fao_rice <- as.data.frame(fao_rice)
fao_rice <- fao_rice %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "1990", .name_repair = "minimal") %>%
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


fao_final_placebo_1985 <- fao_final_index_wider %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "sum_fao") %>%
  mutate(cod = as.integer(cod), year = as.integer(year)) %>%
  arrange(cod)



fao_final_placebo_1985 %<>% rename(sum_fao_placebo_1985 = sum_fao) %>% 
  dplyr::select(-"municip")

# Saving
save(fao_final_placebo_1985, file = here("data", "output", "final", "measure_placebo_1985.RData"))




####################### 4. Mario Exposure Measure ############################
load(here("data", "output", "prices", "prices_real_bartik_2010.Rdata"))

prices <- cpi_prices_2010 %>% filter(Years >= 2000)
prices[-1] <- log(prices[-1])

fao_pr <- read_dta(here("data", "output", "fao_gaez", "fao_pr_cattle_1995.dta"))

# Banana
fao_banana <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_banana)*unique(prices$Banana)[i]
  fao_banana <- cbind(fao_banana, tmp)
}

fao_banana <- as.data.frame(fao_banana)
fao_banana <- fao_banana %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Beef (Cattle)
fao_cattle <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_cattle)*unique(prices$Beef)[i]
  fao_cattle <- cbind(fao_cattle, tmp)
}

fao_cattle <- as.data.frame(fao_cattle)
fao_cattle <- fao_cattle %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Barley
fao_barley <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_barley)*unique(prices$Barley)[i]
  fao_barley <- cbind(fao_barley, tmp)
}

fao_barley <- as.data.frame(fao_barley)
fao_barley <- fao_barley %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Cocoa
fao_cocoa <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_cocoa)*unique(prices$Cocoa)[i]
  fao_cocoa <- cbind(fao_cocoa, tmp)
}

fao_cocoa <- as.data.frame(fao_cocoa)
fao_cocoa <- fao_cocoa %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Coffee usando preço do arabic
fao_coffee_arabic <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_coffee)*unique(prices$`Coffee Arabic`)[i]
  fao_coffee_arabic <- cbind(fao_coffee_arabic, tmp)
}

fao_coffee_arabic <- as.data.frame(fao_coffee_arabic)
fao_coffee_arabic <- fao_coffee_arabic %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



# Cotton
fao_cotton <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_cotton)*unique(prices$Cotton)[i]
  fao_cotton <- cbind(fao_cotton, tmp)
}

fao_cotton <- as.data.frame(fao_cotton)
fao_cotton <- fao_cotton %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Maize
fao_maize <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_maize)*unique(prices$Maize)[i]
  fao_maize <- cbind(fao_maize, tmp)
}

fao_maize <- as.data.frame(fao_maize)
fao_maize <- fao_maize %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Orange
fao_orange <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_orange)*unique(prices$Orange)[i]
  fao_orange <- cbind(fao_orange, tmp)
}

fao_orange <- as.data.frame(fao_orange)
fao_orange <- fao_orange %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()



# Soy
fao_soybean <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_soybean)*unique(prices$Soy)[i]
  fao_soybean <- cbind(fao_soybean, tmp)
}

fao_soybean <- as.data.frame(fao_soybean)
fao_soybean <- fao_soybean %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Tobacco
fao_tobacco <- NULL

for (i in 1:20) {
  tmp <- fao_pr$pr_tobacco*unique(prices$Tobacco)[i]
  fao_tobacco <- cbind(fao_tobacco, tmp)
}

fao_tobacco <- as.data.frame(fao_tobacco)
fao_tobacco <- fao_tobacco %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()

# Sugar
fao_sugarcane <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_sugarcane)*unique(prices$Sugar)[i]
  fao_sugarcane <- cbind(fao_sugarcane, tmp)
}

fao_sugarcane <- as.data.frame(fao_sugarcane)
fao_sugarcane <- fao_sugarcane %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Sugar
fao_sorghum <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_sorghum)*unique(prices$Sorghum)[i]
  fao_sorghum <- cbind(fao_sorghum, tmp)
}

fao_sorghum <- as.data.frame(fao_sorghum)
fao_sorghum <- fao_sorghum %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Tea
fao_tea <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_tea)*unique(prices$Tea)[i]
  fao_tea <- cbind(fao_tea, tmp)
}

fao_tea <- as.data.frame(fao_tea)
fao_tea <- fao_tea %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Wheat
fao_wheat <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_wheat)*unique(prices$`Wheat S`)[i]
  fao_wheat <- cbind(fao_wheat, tmp)
}

fao_wheat <- as.data.frame(fao_wheat)
fao_wheat <- fao_wheat %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()


# Rice
fao_rice <- NULL

for (i in 1:20) {
  tmp <- (fao_pr$pr_rice)*unique(prices$`Rice 05`)[i]
  fao_rice <- cbind(fao_rice, tmp)
}

fao_rice <- as.data.frame(fao_rice)
fao_rice <- fao_rice %>% as_tibble(.name_repair = "unique") %>%
  setNames(prices$Years) %>%
  add_column(municip = fao_pr$municip, .before = "2000", .name_repair = "minimal") %>%
  as_tibble()




# Tidying it up
fao_banana  %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_banana")
fao_barley %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_barley")
fao_cattle %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_cattle")
fao_cocoa %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_cocoa")
fao_coffee_arabic %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_coffee")
fao_cotton %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_cotton")
fao_maize %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_maize")
fao_orange %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_orange")
fao_rice %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_rice")
fao_soybean %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_soybean")
fao_sorghum %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_sorghum")
fao_sugarcane %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_sugarcane")
fao_tobacco %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_tobacco")
fao_wheat %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_wheat")
fao_tea %<>% pivot_longer(-municip, names_to = "year", values_to = "fao_tea")


# Joining all in one dataset
pq_aux <- Reduce(inner_join, list(fao_banana, fao_barley, fao_cattle, fao_cocoa,
                                  fao_coffee_arabic, fao_cotton,
                                  fao_maize, fao_orange, fao_rice,
                                  fao_soybean, fao_sorghum, fao_sugarcane,
                                  fao_tobacco, fao_wheat, fao_tea))


# Summing all the values for each crop for all municipality
fao_final_index <- pq_aux %>% mutate(sum_fao_cattle_1995 = dplyr::select(., fao_banana:fao_tea) %>% 
                                       rowSums(na.rm = TRUE)) %>%
  dplyr::select(municip, year, sum_fao_cattle_1995)


fao_final_index_wider <- fao_final_index %>% pivot_wider(names_from = "year",
                                                         values_from = "sum_fao_cattle_1995",
                                                         names_repair = "minimal") %>%
  add_column(cod = fao_pr$cod, .before = "municip") %>%
  arrange(cod)


fao_final_cattle_1995 <- fao_final_index_wider %>% pivot_longer(-c("cod", "municip"),
                                                                names_to = "year",
                                                                values_to = "sum_fao_cattle_1995") %>%
  mutate(cod = as.integer(cod), year = as.integer(year)) %>%
  arrange(cod)

# Saving
save(fao_final_cattle_1995, file = here("data", "output", "final", "mario_measure.RData"))
