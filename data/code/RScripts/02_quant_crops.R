# Load packages
source("./00_load_packages.R")

#### 1. Clean and caculate the mean quantities for each crop in each
#### municipality between 1995-1999. To be used as the "share" in the Bartik Measure

# Set crops names
crops_names <- c("banana", "barley", "cattle", "cocoa", "coffee", "cotton_1",
                 "cotton_2", "indiantea", "maize", "oatmeal", "orange", "rice",
                 "rubber", "sorghum", "soybean", "sugar_cane", "tobacco", "wheat",
                 "yerba_mate")

# Read all the files one by one just to store them if necessary
banana    <- read_excel(here("data", "raw", "pam", "pam_corr", "banana.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
barley    <- read_excel(here("data", "raw", "pam", "pam_corr", "barley.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
cocoa     <- read_excel(here("data", "raw", "pam", "pam_corr", "cocoa.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
coffee    <- read_excel(here("data", "raw", "pam", "pam_corr", "cofee.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
cotton_1  <- read_excel(here("data", "raw", "pam", "pam_corr", "cotton_1.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
cotton_2  <- read_excel(here("data", "raw", "pam", "pam_corr", "cotton_2.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
indiantea <- read_excel(here("data", "raw", "pam", "pam_corr", "indiantea.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
maize     <- read_excel(here("data", "raw", "pam", "pam_corr", "maize.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
oatmeal   <- read_excel(here("data", "raw", "pam", "pam_corr", "oatmeal.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
orange    <- read_excel(here("data", "raw", "pam", "pam_corr", "orange.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
rice      <- read_excel(here("data", "raw", "pam", "pam_corr", "rice.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
rubber    <- read_excel(here("data", "raw", "pam", "pam_corr", "rubber.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
sorghum   <- read_excel(here("data", "raw", "pam", "pam_corr", "sorghum.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
soybean   <- read_excel(here("data", "raw", "pam", "pam_corr", "soybean.xlsx"),
                        sheet = "Tabela", col_names = TRUE,
                        na = c("NA","N/A","", "...", "-", "..", "X"))
sugar_cane <- read_excel(here("data", "raw", "pam", "pam_corr", "sugar_cane.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))
tobacco    <- read_excel(here("data", "raw", "pam", "pam_corr", "tobacco.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))
wheat      <- read_excel(here("data", "raw", "pam", "pam_corr", "wheat.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))
yerba_mate <- read_excel(here("data", "raw", "pam", "pam_corr", "yerba_mate.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))

cattle     <- read_excel(here("data", "raw", "pam", "pam_corr", "cattle.xlsx"),
                         sheet = "Tabela", col_names = TRUE,
                         na = c("NA","N/A","", "...", "-", "..", "X"))

## Now a more convinient way to read all the files

# We need to reset the working directory in order to use the purr::map function below
setwd(here("data", "raw", "pam", "pam_corr"))

file.list <- list.files(pattern='*.xlsx')

# Makes a list of all the excel files
df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE,
                   na = c("NA","N/A","", "...", "-", "..", "X"))

# Sets a function to clean and calculate the mean of each crop quantity for each
# municipality in the period 1995-1999
df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}

#Same function for the period 1990-1999

df_fix_2 = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip","1990" ,"1991" ,"1992" ,"1993","1994", "1995",
                  "1996","1997", "1998","1999") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}


# Now for actual meaasure
df_fix_actual = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "2000" ,"2001" , "2002" , "2003", "2004",
                  "2005" ,"2006" ,"2007", "2008", "2009", "2010") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}

# Apply the function above to all datasets at once using map
df_list_mean <- map(.x = df_list_map, .f = df_fix)

df_list_mean_1990 <- map(.x = df_list_map, .f = df_fix_2)

df_list_actual <- map(.x = df_list_map, .f = df_fix_actual)


# Merge all the crops files
quantities_map <- reduce(df_list_mean, merge, by = c("cod", "municip"))

quantities_map_2 <- reduce(df_list_mean_1990, merge, by = c("cod", "municip"))

quantities_map_actual <- reduce(df_list_actual, merge, by = c("cod", "municip"))

# Produce a tidy dataset and takes out oatmeal, since it cannot be matched in the price dataset
quantities_1995 <- quantities_map %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

quantities_1990 <- quantities_map_2 %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

quantities_actual <- quantities_map_actual %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

# Multiplying cattle by medium weight (255kg) to transform it in tons
quantities_1995 %<>% mutate(cattle = (cattle*255)/1000)

quantities_actual %<>% mutate(cattle = (cattle*255)/1000)

quantities_1990 %<>% mutate(cattle = (cattle*255)/1000)

#Save the quantities dataset
save(quantities_1995, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1995_bartik.Rdata")

save(quantities_actual, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual.Rdata")

save(quantities_1990, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1990_bartik.Rdata")


########## 2. Construct a measure for the share of each commodity in a given location in relation to the total produced quantity in that same location 

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1995_bartik.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1990_bartik.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual.Rdata")

# Sum over all quantities

quant_fao_1995 <- quantities_1995 %>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))


quant_fao_actual <- quantities_1995 %>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))



quantities_1995 <- quantities_1995 %>% mutate(total_quant = rowSums(.[3:19])) %>%
  mutate(cotton = cotton_1 + cotton_2) %>%
  dplyr::select(-c("cotton_1", "cotton_2")) %>%
  relocate(cotton, .before = "indiantea")



  
# Constrcut shares
shares_1995 <- lmap(quantities_1995[3:18], ~{.x/quantities_1995$total_quant})


shares_fao_actual <- lmap(quant_fao_actual[3:18], ~{.x/quant_fao_1995$total_quant})
shares_fao_1995 <- lmap(quant_fao_1995[3:18], ~{.x/quant_fao_1995$total_quant})


# Tidying up
shares_1995 <- shares_1995 %>% mutate(total_quant = rowSums(.[1:16])) %>%
  add_column(cod = quantities_1995$cod, .before = "banana") %>%
  add_column(municip = quantities_1995$municip, .before = "banana") %>%
  arrange(cod)

shares_fao_actual %<>%
  add_column(cod = quant_fao_1995$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1995$municip, .before = "banana") %>%
  arrange(cod)

shares_fao_1995 <- shares_fao_1995 %>%
  add_column(cod = quant_fao_1995$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1995$municip, .before = "banana") %>%
  arrange(cod)
  

#Saving
save(shares_fao_1995, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_1995.Rdata")
save(shares_fao_actual, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_actual.Rdata")


save(shares_fao_1990, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_1990.Rdata")
save(shares_1995, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_1995_bartik.Rdata")




######### 3. Now adding Cattle heads (livestock) ###############################
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados_PAM/Dados_PAM_corr")

crops_names <- c("banana", "barley", "cattle", "cocoa", "coffee", "cotton_1",
                 "cotton_2", "indiantea", "maize", "oatmeal", 
                 "orange", "rice", "rubber", "sorghum", "soybean", "sugar_cane",
                 "tobacco", "wheat", "yerba_mate")

file.list <- list.files(pattern='*.xlsx')

# Makes a list of all the excel files
df_list_map <- map(file.list, read_excel, sheet = "Tabela", col_names = TRUE,
                   na = c("NA","N/A","", "...", "-", "..", "X"))

# Sets a function to clean and calculate the mean of each crop quantity for each
# municipality in the period 1995-1999

df_fix = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "1995" ,"1996" ,"1997", "1998", "1999") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}


# Now for actual meaasure
df_fix_actual = function(x){
  x %>% rename("cod" = "Cód.", municip = "Brasil e Município") %>%
    dplyr::select("cod", "municip", "2000" ,"2001" , "2002" , "2003", "2004",
                  "2005" ,"2006" ,"2007", "2008", "2009", "2010") %>%
    pivot_longer(-c("cod", "municip"),names_to = "year", values_to = "quant") %>%
    replace_na(list(quant = 0)) %>%
    arrange(cod) %>%
    group_by(cod, municip) %>%
    summarize(quant_mean = mean(quant))
}

# Apply the function above to all datasets at once using map
df_list_mean <- map(.x = df_list_map, .f = df_fix)
df_list_actual <- map(.x = df_list_map, .f = df_fix_actual)


# Merge all the crops files
quantities_map <- reduce(df_list_mean, merge, by = c("cod", "municip"))
quantities_map_actual <- reduce(df_list_actual, merge, by = c("cod", "municip"))

# Produce a tidy dataset and takes out oatmeal, since it cannot be matched in the price dataset
quantities_1995 <- quantities_map %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

quantities_actual <- quantities_map_actual %>% setNames(c("cod", "municip", crops_names)) %>%
  as_tibble() %>%
  dplyr::select(-oatmeal) %>%
  filter(cod >1)

# Multiplying cattle by medium weight (255kg) to transform it in tons
quantities_1995 %<>% mutate(cattle = (cattle*255)/1000)

quantities_actual %<>% mutate(cattle = (cattle*255)/1000)

#Save the quantities dataset
save(quantities_1995, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1995_cattle.Rdata")

save(quantities_actual, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual_cattle.Rdata")


######### Shares #######
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_1995_cattle.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/quantities_actual_cattle.Rdata")

# Sum over all quantities

quant_fao_1995 <- quantities_1995 %>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))


quant_fao_actual <- quantities_actual %>% mutate(cotton = cotton_1 + cotton_2, tea = indiantea + yerba_mate) %>%
  dplyr::select(-c("cotton_1", "cotton_2", "indiantea", "yerba_mate", "rubber")) %>%
  mutate(total_quant = rowSums(.[3:17]))



# Constrcut shares
shares_fao_1995 <- lmap(quant_fao_1995[3:18], ~{.x/quant_fao_1995$total_quant})
shares_fao_actual <- lmap(quant_fao_actual[3:18], ~{.x/quant_fao_actual$total_quant})


# Tidying up
shares_fao_actual %<>%
  add_column(cod = quant_fao_actual$cod, .before = "banana") %>%
  add_column(municip = quant_fao_actual$municip, .before = "banana") %>%
  arrange(cod)

shares_fao_1995 %<>%
  add_column(cod = quant_fao_1995$cod, .before = "banana") %>%
  add_column(municip = quant_fao_1995$municip, .before = "banana") %>%
  arrange(cod)


#Saving
save(shares_fao_1995,
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_1995_cattle.Rdata")

save(shares_fao_actual,
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_actual_cattle.Rdata")







