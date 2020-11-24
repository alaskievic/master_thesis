# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

######### 1. Reads and cleans data from Agricultural Census  ############################################################


######### Reading 1995

# Value of production

tab_338 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela338.xlsx", 
                               skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_500 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela500.xlsx", 
                     skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_513 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela513.xlsx", 
                     skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_527 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela527.xlsx", 
                     skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_534 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela534.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_551 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_prod/tabela551.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Total Area

tab_314 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/total_area/tabela314.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers

tab_321 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/n_workers/tabela321.xlsx", 
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

# Putting in mil reais
agriprod_1995 %<>% mutate(totval = totval/1000)


tab_314 %<>% set_names(c("cod", "municip", "x", "total_area")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


tab_321 %<>% set_names(c("cod", "municip", "x", "n_workers")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))


agri_1995 <- inner_join(agriprod_1995, tab_314, by = "cod")

agri_1995 <- inner_join(agri_1995, tab_321, by = "cod")

agri_1995 %<>% mutate(val_outpw = totval/n_workers) %>% 
  mutate(l_inten = n_workers/total_area)


agri_1995 %<>% add_column(year = rep("1995", length(agri_1995$cod)), .before = "municip")


          
                                 
######### Reading 2006

# Value of production

tab_782 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela782.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_815 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela815.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_816 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela816.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_818 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela818.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_937 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela937.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_943 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela943.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1177 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela1177.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1178 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela1178.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_1823 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_prod/tabela1823.xlsx", 
                      skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Total Area

tab_787 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/total_area/tabela787.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers

tab_956 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/n_workers/tabela956.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Tractors and Machines

tab_860_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/machines/tabela860.xlsx", 
                      skip = 6, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_860_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/machines/tabela860.xlsx", 
                        skip = 6, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_861 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/machines/tabela861.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


tab_860_a %<>% set_names(c("cod", "municip", "x", "y", "nf_tract", "nf_tract_99", "nf_tract_101")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y")) %>% mutate(cod = as.integer(cod))

tab_860_b %<>% set_names(c("cod", "municip", "x", "y", "n_tract", "n_tract_99", "n_tract_101")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_860 <- inner_join(tab_860_a, tab_860_b, by = "cod")


tab_860 %<>% mutate(n_tract = replace_na(n_tract, 0))


tab_861 %<>% set_names(c("cod", "municip", "x", "y", "total", "seme", "colhe", "adub")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y")) %>% mutate(cod = as.integer(cod))

tab_861 %<>% mutate(seme = replace_na(seme, 0)) %>% mutate(colhe = replace_na(colhe, 0)) %>%
  mutate(adub = replace_na(adub, 0))

tab_861 %<>% mutate(n_maq = seme + colhe + adub + tab_860$n_tract)


# Cleaning and merging

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



agri_2006 %<>% mutate(val_outpw = totval/n_workers) %>% 
  mutate(l_inten = n_workers/total_area)


agri_2006 %<>% add_column(year = rep("2006", length(agri_2006$cod)), .before = "municip")



# Merging with machines


agri_2006 <- inner_join(agri_2006, dplyr::select(tab_860, -"municip"), by = "cod")

agri_2006 <- inner_join(agri_2006, dplyr::select(tab_861, -"municip"), by = "cod")



######### Reading 2017


# Value of production

tab_6910_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                      skip = 5, sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                       skip = 5, sheet = "Tabela 2", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_c <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                       skip = 5, sheet = "Tabela 3", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6910_d <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6910.xlsx", 
                       skip = 5, sheet = "Tabela 4", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


tab_6910 <- inner_join(tab_6910_a, tab_6910_b, by = "...1")
tab_6910 <- inner_join(tab_6910, tab_6910_c, by = "...1")
tab_6910 <- inner_join(tab_6910, tab_6910_d, by = "...1")

tab_6910 %<>% mutate(Total.x = replace_na(Total.x, 0))  %>% mutate(Total.y = replace_na(Total.y, 0)) %>%
  mutate(Total.x.x = replace_na(Total.x.x, 0))  %>% mutate(Total.y.y = replace_na(Total.y.y, 0))


tab_6910 %<>% mutate(val_bov = Total.x + Total.y + Total.x.x + Total.y.y) %>%
  dplyr::select(c("...1", "...2.x", val_bov)) %>% rename(cod = "...1", municip = "...2.x")



tab_6927 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6927.xlsx", 
                         skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6940.xlsx", 
                       skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6940.xlsx", 
                         skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6940 <- inner_join(tab_6940_a, tab_6940_b, by = "...1")

tab_6940 %<>% mutate(Total.x = replace_na(Total.x, 0))  %>% mutate(Total.y = replace_na(Total.y, 0))

tab_6940 %<>% mutate(val_aves = Total.x + Total.y) %>%
  dplyr::select(c("...1", "...2.x", val_aves)) %>% rename(cod = "...1", municip = "...2.x")



tab_6947 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6947.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6949 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6949.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6953 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6953.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6955 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6955.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6957 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_prod/tabela6957.xlsx", 
                       skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



# Total Area

tab_6878 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/total_area/tabela6878.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Workers

tab_6889 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/n_workers/tabela6889.xlsx", 
                      skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Number of Tractors and Machines

tab_6869_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/machines/tabela6869.xlsx", 
                        skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6869_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/machines/tabela6869.xlsx", 
                        skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6872 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/machines/tabela6872.xlsx", 
                      skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



tab_6869_a %<>% set_names(c("cod", "municip", "x", "y", "nf_tract", "nf_tract_99", "nf_tract_101")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y")) %>% mutate(cod = as.integer(cod))

tab_6869_b %<>% set_names(c("cod", "municip", "x", "y", "n_tract", "n_tract_99", "n_tract_101")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


tab_6869 <- inner_join(tab_6869_a, tab_6869_b, by = "cod")

tab_6872 %<>% set_names(c("cod", "municip", "x", "y", "n_maq")) %>% 
  slice(-5571) %>% dplyr::select(-c("x", "y")) %>% mutate(cod = as.integer(cod))



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



agri_2017 %<>% mutate(val_outpw = totval/n_workers) %>% 
  mutate(l_inten = n_workers/total_area)

agri_2017 %<>% add_column(year = rep("2017", length(agri_2017$cod)), .before = "municip")


# Merging with machines


agri_2017 <- inner_join(agri_2017, dplyr::select(tab_6869, -"municip"), by = "cod")

agri_2017 <- inner_join(agri_2017, dplyr::select(tab_6872, -"municip"), by = "cod")











#### Putting it all together in long format

aux_1995 <- dplyr::select(agri_1995, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area", "val_outpw", "l_inten"))


aux_2006 <- dplyr::select(agri_2006, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area", "val_outpw", "l_inten", "nf_tract", 
                                       "nf_tract_99", "nf_tract_101", "n_tract", 
                                       "n_tract_99", "n_tract_101", "n_maq"))


aux_2017 <- dplyr::select(agri_2017, c("cod", "year", "municip", "totval", "n_workers", 
                                       "total_area", "val_outpw", "l_inten", "nf_tract", 
                                       "nf_tract_99", "nf_tract_101", "n_tract", 
                                       "n_tract_99", "n_tract_101", "n_maq"))


### Deflating

ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca_1995 <- ipca %>% filter(Date == 1995)
ipca_2006 <- ipca %>% filter(Date == 2006)
ipca_2017 <- ipca %>% filter(Date == 2017)


aux_1995_def <- aux_1995 %>% mutate(totval = totval/(ipca_1995$Index_2/100)) %>% 
  mutate(val_outpw = val_outpw/(ipca_1995$Index_2/100))

aux_2006_def <- aux_2006 %>% mutate(totval = totval/(ipca_2006$Index_2/100)) %>% 
  mutate(val_outpw = val_outpw/(ipca_2006$Index_2/100))

aux_2017_def <- aux_2017 %>% mutate(totval = totval/(ipca_2017$Index_2/100)) %>% 
  mutate(val_outpw = val_outpw/(ipca_2017$Index_2/100))


agro_struc <- bind_rows(aux_1995, aux_2006)
agro_struc <- bind_rows(agro_struc, aux_2017)

save(agro_struc, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/agro_struc.Rdata")


####### Joining with controls and Bartik

agro_struc %<>% mutate(year = as.integer(year))

# Adding population
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pop_sidra.Rdata")

pop_1996 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/populacao_total.xls", 
                        sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

pop_1996 %<>% rename(cod = "Codigo", municip = "Município") %>%
  dplyr:: select(c("cod", "municip", "1996"))

pop_1996 %<>% add_column(year = rep("1995", length(pop_1996$cod)), .before = "municip")

pop_1996 %<>% rename(pesotot = "1996") %>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

pop_sidra %<>% filter(year == 2006| year == 2015) %>%
  mutate(cod = as.integer(cod)) %>% mutate(year = as.integer(year))

pop_sidra %<>% mutate(year = ifelse(year == 2015, 2017, 2006))

pop_aux <- bind_rows(pop_1996, pop_sidra)


agro_struc <- inner_join(agro_struc, dplyr::select(pop_aux, -"municip"), by = c("cod", "year"))





# Adding Controls
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")

agro_struc <- inner_join(agro_struc, dplyr::select(controls, -"municip"), by = "cod")


agro_struc %<>% mutate(pop_dens = pesotot/geo_area_2010)


# Adding Bartik
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/bartik_final.Rdata")

bartik_final_1 <- bartik_final %>% filter(year == 2000| year == 2010 | year == 2015)

bartik_final_1 %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2010, 2006, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))


bartik_final_2 <- bartik_final %>% filter(year == 2000| year == 2006|  year == 2015)

bartik_final_2 %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))


agro_struc_1 <- inner_join(agro_struc, dplyr::select(bartik_final_1, -"municip"),
                                by = c("cod", "year"))

agro_struc_2 <- inner_join(agro_struc, dplyr::select(bartik_final_2, -"municip"),
                           by = c("cod", "year"))


# Writing to STATA

setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(agro_struc_1, "agro_struc1.dta")
write.dta(agro_struc_2, "agro_struc2.dta")


######### 2. Check Census for useful data for Structural Transformation ########################################


atlas_mun <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Atlas PNUD/atlas2013_dadosbrutos_pt.xlsx",
                        sheet = "MUN 91-00-10", col_names = TRUE)


atlas_mun %<>% dplyr::select(c("ANO", "Codmun7", "Município",  "RDPC", "P_AGRO", "P_SERV", "P_COM",
                               "pesoRUR", "P_CONSTR", "P_EXTR", "RDPCT", "EMP", "P_TRANSF",
                               "pesourb", "pesotot", "POP"))

atlas_mun[atlas_mun == "-" ] <- NA

atlas_mun %<>% rename(year = "ANO", cod = "Codmun7", municip = "Município")

pop_struc <- inner_join(atlas_mun, dplyr::select(controls, -"municip"), by = "cod")

pop_struc %<>% mutate(P_AGRO = as.double(P_AGRO)) %>%  mutate(P_SERV = as.double(P_SERV)) %>%
  mutate(P_COM = as.double(P_COM)) %>% mutate(P_CONSTR = as.double(P_CONSTR)) %>%
  mutate(P_EXTR = as.double(P_EXTR)) %>% mutate(EMP = as.double(EMP)) %>%
  mutate(P_TRANSF = as.double(P_TRANSF))


load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/bartik_final.Rdata")

bartik_final <- bartik_final %>% filter(year == 2000| year == 2010)


pop_struc_final <- inner_join(pop_struc, bartik_final, by = c("cod", "year"))

pop_struc_final %<>% rename(municip = "municip.x")

pop_struc_final2 <- bind_rows(pop_struc_final, filter(pop_struc, year==1991))

setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(pop_struc_final, "pop_struc.dta")
write.dta(pop_struc_final2, "pop_struc_controls.dta")




##### AGRO STRUC 3 WITH  ######

pop_aux_3 <- filter(atlas_mun, year == 1991) %>% mutate(year = ifelse(year == 1991, 1995, year))
agro_struc_3 <- inner_join(agro_struc_1, pop_aux_3, by = "cod")

setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(agro_struc_3, "agro_struc3.dta")




agro_struc_4 <- inner_join(agro_struc_2, pop_aux_3, by = "cod")

setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(agro_struc_4, "agro_struc4.dta")
