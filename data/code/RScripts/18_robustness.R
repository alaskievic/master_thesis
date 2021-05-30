# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")


######### 1. Farm Productivity Measure #########################################

### Reading 1995
# Value of production

tab_338 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela338.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_500 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela500.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_513 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela513.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_527 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela527.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_534 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela534.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_551 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_prod/tabela551.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Total Area

tab_314 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/total_area/tabela314.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers

tab_321 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/n_workers/tabela321.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



# Cleaning and merging

tab_338 %<>% set_names(c("cod", "municip", "x", "val_pec")) %>%
  slice(-5571) %>% dplyr::select(- "x") %>% mutate(cod = as.integer(cod))


tab_500 %<>% set_names(c("cod", "municip", "x", "val_temp")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_513 %<>% set_names(c("cod", "municip", "x", "val_perm")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_527 %<>% set_names(c("cod", "municip", "x", "val_hort")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_534 %<>% set_names(c("cod", "municip", "x", "val_silv")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_551 %<>% set_names(c("cod", "municip", "x", "val_veg")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


agriprod_1995 <- Reduce(inner_join, list(tab_338, tab_500,
                                         tab_513, tab_527, tab_534,
                                         tab_551))

agriprod_1995[is.na(agriprod_1995)] = 0

agriprod_1995 %<>% mutate(totval = rowSums(.[3:8]))



tab_314 %<>% set_names(c("cod", "municip", "x", "total_area")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_321 %<>% set_names(c("cod", "municip", "x", "n_workers")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


agri_1995 <- inner_join(agriprod_1995, tab_314, by = "cod")

agri_1995 <- inner_join(agri_1995, tab_321, by = "cod")

agri_1995 %<>% add_column(year = rep("1995", length(agri_1995$cod)), .before = "municip")



### Reading 2006
# Value of production
tab_782 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela782.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_815 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela815.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_816 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela816.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_818 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela818.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_937 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela937.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_943 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela943.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1177 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela1177.xlsx", 
                       skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1178 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela1178.xlsx", 
                       skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1823 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_prod/tabela1823.xlsx", 
                       skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Total Area

tab_787 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/total_area/tabela787.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers

tab_956 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/n_workers/tabela956.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))




tab_782 %<>% set_names(c("cod", "municip", "x", "y", "z", "a", "b", "c", "val_bov")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y", "z", "a", "b", "c")) %>% mutate(cod = as.integer(cod))


tab_815 %<>% set_names(c("cod", "municip", "x", "y", "z", "val_silv")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "z", "municip")) %>% mutate(cod = as.integer(cod))

tab_816 %<>% set_names(c("cod", "municip", "x", "y", "z", "val_veg")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "z", "municip")) %>% mutate(cod = as.integer(cod))

tab_818 %<>% set_names(c("cod", "municip", "x", "y", "val_hort")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

tab_937 %<>% set_names(c("cod", "municip", "x", "y", "val_suin")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

tab_943 %<>% set_names(c("cod", "municip", "x", "y", "val_aves")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

tab_1177 %<>% set_names(c("cod", "municip", "x", "y", "val_perm_a")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

tab_1178 %<>% set_names(c("cod", "municip", "x", "y", "val_perm_b")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

tab_1823 %<>% set_names(c("cod", "municip", "x", "val_temp")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "municip")) %>% mutate(cod = as.integer(cod))




agriprod_2006 <- Reduce(inner_join, list(tab_782, tab_815, tab_816, tab_818, 
                                         tab_937, tab_943, tab_1177, tab_1178, 
                                         tab_1823))


agriprod_2006[is.na(agriprod_2006)] = 0

agriprod_2006 %<>% mutate(totval = rowSums(.[3:11]))


tab_787 %<>% set_names(c("cod", "municip", "x", "y", "total_area")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_956 %<>% set_names(c("cod", "municip", "x", "y", "n_workers")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))



agri_2006 <- inner_join(agriprod_2006, tab_787, by = "cod")

agri_2006 <- inner_join(agri_2006, tab_956, by = "cod")


agri_2006 %<>% add_column(year = rep("2006", length(agri_2006$cod)), .before = "municip")



### Reading 2017
# Value of production

tab_6910_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                         skip = 5, sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                         skip = 5, sheet = "Tabela 2", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_c <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                         skip = 5, sheet = "Tabela 3", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_d <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                         skip = 5, sheet = "Tabela 4", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


tab_6910 <- inner_join(tab_6910_a, tab_6910_b, by = "...1")
tab_6910 <- inner_join(tab_6910, tab_6910_c, by = "...1")
tab_6910 <- inner_join(tab_6910, tab_6910_d, by = "...1")

tab_6910 %<>% mutate(Total.x = replace_na(Total.x, 0))  %>% mutate(Total.y = replace_na(Total.y, 0)) %>%
  mutate(Total.x.x = replace_na(Total.x.x, 0))  %>% mutate(Total.y.y = replace_na(Total.y.y, 0))


tab_6910 %<>% mutate(val_bov = Total.x + Total.y + Total.x.x + Total.y.y) %>%
  dplyr::select(c("...1", "...2.x", val_bov)) %>% rename(cod = "...1", municip = "...2.x")



tab_6927 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6927.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6940.xlsx", 
                         skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6940.xlsx", 
                         skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940 <- inner_join(tab_6940_a, tab_6940_b, by = "...1")

tab_6940 %<>% mutate(Total.x = replace_na(Total.x, 0))  %>% mutate(Total.y = replace_na(Total.y, 0))

tab_6940 %<>% mutate(val_aves = Total.x + Total.y) %>%
  dplyr::select(c("...1", "...2.x", val_aves)) %>% rename(cod = "...1", municip = "...2.x")



tab_6947 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6947.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6949 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6949.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6953 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6953.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6955 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6955.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6957 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_prod/tabela6957.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



# Total Area
tab_6878 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/total_area/tabela6878.xlsx", 
                       skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers
tab_6889 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/n_workers/tabela6889.xlsx", 
                       skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Cleaning and merging
tab_6910 %<>% slice(-5571) %>% mutate(cod = as.integer(cod))


tab_6927 %<>% set_names(c("cod", "municip", "x", "val_suin")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_6940 %<>% slice(-5571) %>% mutate(cod = as.integer(cod))


tab_6947 %<>% set_names(c("cod", "municip", "x", "y", "val_silv")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6949 %<>% set_names(c("cod", "municip", "x", "y",  "val_veg")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6953 %<>% set_names(c("cod", "municip", "x", "y", "val_horti")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6955 %<>% set_names(c("cod", "municip", "x", "y", "val_perm")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6957 %<>% set_names(c("cod", "municip", "x", "y", "val_temp")) %>%
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))





agriprod_2017 <- Reduce(inner_join, list(tab_6910, tab_6927, tab_6940, tab_6947, 
                                         tab_6949, tab_6953, tab_6955, tab_6957))


agriprod_2017[is.na(agriprod_2017)] = 0

agriprod_2017 %<>% mutate(totval = rowSums(.[3:10]))



tab_6878 %<>% set_names(c("cod", "municip", "x", "y", "total_area")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6889 %<>% set_names(c("cod", "municip", "x", "y", "n_workers")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))




agri_2017 <- inner_join(agriprod_2017, tab_6878, by = "cod")

agri_2017 <- inner_join(agri_2017, tab_6889, by = "cod")


agri_2017 %<>% add_column(year = rep("2017", length(agri_2017$cod)), .before = "municip")


aux_1995 <- dplyr::select(agri_1995, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area"))


aux_2006 <- dplyr::select(agri_2006, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area"))


aux_2017 <- dplyr::select(agri_2017, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area"))


### Deflating

ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca_1995 <- ipca %>% filter(Date == 1995)
ipca_2006 <- ipca %>% filter(Date == 2006)
ipca_2017 <- ipca %>% filter(Date == 2017)


aux_1995_def <- aux_1995 %>% mutate(totval = totval/(ipca_1995$Index_2/100))

aux_2006_def <- aux_2006 %>% mutate(totval = totval/(ipca_2006$Index_2/100))

aux_2017_def <- aux_2017 %>% mutate(totval = totval/(ipca_2017$Index_2/100))

# Value of production is in mil reais for 2006 and 2017 but not for 1995

aux_1995_def %<>% mutate(totval = totval/1000)

prod <- bind_rows(aux_1995_def, aux_2006_def)
prod <- bind_rows(prod, aux_2017_def)

prod %<>% mutate(linten = n_workers/total_area) %>%
  mutate(val_outpw = totval/n_workers) %>%
  mutate(val_outpa = totval/total_area)

save(prod, 
file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agri_controls.RData")


######### 2. FAO-GAEZ first principal components  ##############################

# First we normalize by the maximum in each observation

fao <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/fao_pr_cattle_1995.dta")

fao_low <- fao %>% dplyr::select(cod, municip, ends_with("_low"))

fao_int <- fao %>% dplyr::select(cod, municip, ends_with("_int"))

fao_high <- fao %>% dplyr::select(cod, municip, ends_with("_high"))

norm <- function(x) {x/max(x)}

fao_low_norm <- map_dfc(fao_low[3:17], norm) %>%
  add_column(fao_low$cod, fao_low$municip,
             .before = "banana_low") %>%
  rename(cod = "fao_low$cod", municip = "fao_low$municip")

fao_int_norm <- map_dfc(fao_int[3:16], norm) %>%
  add_column(fao_int$cod, fao_int$municip,
             .before = "banana_int") %>%
  rename(cod = "fao_int$cod", municip = "fao_int$municip")

fao_high_norm <- map_dfc(fao_high[3:17], norm) %>%
  add_column(fao_high$cod, fao_high$municip,
             .before = "banana_high") %>%
  rename(cod = "fao_high$cod", municip = "fao_high$municip")

### Calculatin PCA

fao_pca_low <- prcomp(fao_low_norm[3:17], center = TRUE, scale = TRUE)

low <- fao_pca_low$x %>% as_tibble() %>% dplyr::select(PC1) %>%
  add_column(fao_low$cod, fao_low$municip,
             .before = "PC1") %>%
  rename(cod = "fao_low$cod", municip = "fao_low$municip", pc1_low = "PC1")



fao_pca_int <- prcomp(fao_int_norm[3:16], center = TRUE, scale = TRUE)

int <- fao_pca_int$x %>% as_tibble() %>% dplyr::select(PC1) %>%
  add_column(fao_int$cod, fao_int$municip,
             .before = "PC1") %>%
  rename(cod = "fao_int$cod", municip = "fao_int$municip", pc1_int = "PC1")



fao_pca_high <- prcomp(fao_high_norm[3:17], center = TRUE, scale = TRUE)

high <- fao_pca_high$x %>% as_tibble() %>% dplyr::select(PC1) %>%
  add_column(fao_high$cod, fao_high$municip,
             .before = "PC1") %>%
  rename(cod = "fao_high$cod", municip = "fao_high$municip", pc1_high = "PC1")

first_pca <- inner_join(low, dplyr::select(int, -municip), by = "cod")
first_pca <- inner_join(first_pca, dplyr::select(high, -municip), by = "cod")

save(first_pca, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/first_pca.RData")


