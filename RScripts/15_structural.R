# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

######### 1. Reads and cleans data from Agricultural Census  ###################


######### Reading 1995

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


# Number of Tractors and Machines

tab_860_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/machines/tabela860.xlsx", 
                      skip = 6, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_860_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/machines/tabela860.xlsx", 
                        skip = 6, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_861 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/machines/tabela861.xlsx", 
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


# Number of Tractors and Machines
tab_6869_a <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/machines/tabela6869.xlsx", 
                        skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6869_b <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/machines/tabela6869.xlsx", 
                        skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

tab_6872 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/machines/tabela6872.xlsx", 
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
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls", 
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


agro_struc <- bind_rows(aux_1995_def, aux_2006_def)
agro_struc <- bind_rows(agro_struc, aux_2017_def)

save(agro_struc, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")



##### Joining data for group areas and use of fertilizers ######################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")

## 2006
# Read data
group_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/area_group/area_grupo_2006.xlsx", 
                         skip = 4, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

adubo_tipo_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/fertilizer/adubo_tipo_2006.xlsx", 
                              skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


agrotox_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/fertilizer/agrotox_2006.xlsx", 
                              skip = 4, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


agrotox_equip_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/fertilizer/agrotox_equip_2006.xlsx", 
                              skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Cleaning

group_2006 %<>% set_names(c("cod", "municip", "x", "total_area", "temp", "hort", "perm", "sement", 
                            "pecu", "flor1", "flor2", "pesca", "aqui")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>% mutate(cod = as.integer(cod))

group_2006[is.na(group_2006)] = 0

# Por enquanto vamos usar apenas a área usada, já esá no dataset anterior


adubo_tipo_2006 %<>% set_names(c("cod", "municip", "x", "y", "total_adubo", "adubo_yes", "quimn",
                                 "quimnn", "esterco", "verde", "vin", "humus", "bio", "inoc", 
                                 "compost", "outros", "nus", "adubo_n")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))

adubo_tipo_2006[is.na(adubo_tipo_2006)] = 0

# We have a problem here withe the shares in each type
adubo_tipo_2006 %<>% mutate(shares_adubo = ((adubo_yes + nus)/total_adubo), 
                            quimio_share = ((quimn + quimnn)/total_adubo),
                            org_share = ((esterco + verde + vin + humus + bio + inoc + compost + outros)/total_adubo), 
                            shares_nadubo = adubo_n/total_adubo) %>%
  dplyr::select(cod, adubo_yes, shares_adubo)


agrotox_2006  %<>% set_names(c("cod", "municip", "x", "y", "total_agrotox", "agrotox_yes", "agrotox_n")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>% mutate(cod = as.integer(cod))


agrotox_2006[is.na(agrotox_2006)] = 0

agrotox_2006 %<>% mutate(share_agrotox = agrotox_yes/total_agrotox) %>%
  dplyr::select(cod, agrotox_yes, share_agrotox)



## 2017

group_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/area_group/area_grupo_2017.xlsx", 
                         skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

adubo_tipo_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/fertilizer/adubo_tipo_2017.xlsx", 
                         skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


agrotox_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/fertilizer/agrotox_2017.xlsx", 
                           skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Cleaning 
adubo_tipo_2017 %<>% set_names(c("cod", "municip", "x", "y", "z", "total_adubo", "adubo_yes", "quimio",
                                 "org", "quimiorg", "adubo_n", "a", "b")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "a", "b", "municip")) %>% mutate(cod = as.integer(cod))


adubo_tipo_2017[is.na(adubo_tipo_2017)] = 0

adubo_tipo_2017 %<>% mutate(shares_adubo = adubo_yes/total_adubo) %>%
  dplyr::select(cod, adubo_yes, shares_adubo)


agrotox_2017  %<>% set_names(c("cod", "municip", "x", "y", "z", "total_agrotox", "agrotox_yes", "agrotox_n")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "z", "municip")) %>% mutate(cod = as.integer(cod))

agrotox_2017[is.na(agrotox_2017)] = 0


agrotox_2017 %<>% mutate(share_agrotox = agrotox_yes/total_agrotox) %>%
  dplyr::select(cod, agrotox_yes, share_agrotox)


# Joining
fertilizer_2006 <- inner_join(adubo_tipo_2006, agrotox_2006, by = "cod") 
fertilizer_2017 <- inner_join(adubo_tipo_2017, agrotox_2017, by = "cod") 

fertilizer_2006 %<>% mutate(year = 2006)
fertilizer_2017 %<>% mutate(year = 2017)

fertilizer <- bind_rows(fertilizer_2006, fertilizer_2017)


# Cleaning the agricultural census dataset and adding intensities
agro_struc %<>% mutate(year = as.integer(year))

agro_struc<- full_join(agro_struc, fertilizer, by = (c("cod", "year")))

agro_struc %<>% mutate(ltract_inten = log(n_tract/total_area), lmaq_inten = log(n_maq/total_area))


# Saving
save(agro_struc, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")



##### Seeds, property and agroindustry  ########################################

### Seeds
# 2006
seeds_value_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/seeds/semente_2006.xlsx", 
                         skip = 6, sheet = 3, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
  
seeds_area_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/seeds/semente_2006.xlsx", 
                         skip = 6, sheet = 4, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# In mil reais
seeds_value_2006 %<>% set_names(c("cod", "municip", "x", "y", "z", "totval_seeds", "cert", "comum",
                                 "trans_value", "nsab")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "z", "municip",
                                     "cert", "comum", "nsab")) %>%
  mutate(cod = as.integer(cod))


seeds_area_2006 %<>% set_names(c("cod", "municip", "x", "y", "z", "totarea_seeds", "cert", "comum",
                                  "trans_area", "nsab")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "z", "municip",
                                     "cert", "comum", "nsab")) %>%
  mutate(cod = as.integer(cod))



seeds_2006 <- full_join(seeds_value_2006, seeds_area_2006, by = "cod")


# 2017
seeds_value_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/seeds/semente_2017.xlsx", 
                               skip = 5, sheet = 4, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

seeds_area_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/seeds/semente_2017.xlsx", 
                              skip = 5, sheet = 5, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


seeds_value_2017 %<>% set_names(c("cod", "municip", "tipo", "x", "total"))
seeds_area_2017 %<>% set_names(c("cod", "municip", "tipo", "x", "total"))


# Fills NA values only in the municip column
seeds_value_2017[1] <- na.locf(seeds_value_2017[1], fromLast = FALSE)

seeds_value_2017[2] <- na.locf(seeds_value_2017[2], fromLast = FALSE)

seeds_value_2017 %<>% dplyr::select(-c("municip", "x")) %>%
  pivot_wider(values_from = "total", names_from = "tipo")

seeds_value_2017[is.na(seeds_value_2017)] = 0

seeds_value_2017 %<>% set_names(c("cod", "totval_seeds", "com1", "com2", "cert", 
                                  "trans_value", "na")) %>%
  dplyr::select(-c("com1", "com2", "cert", "na")) %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))


# Now for the area
seeds_area_2017[1] <- na.locf(seeds_area_2017[1], fromLast = FALSE)

seeds_area_2017[2] <- na.locf(seeds_area_2017[2], fromLast = FALSE)

seeds_area_2017 %<>% dplyr::select(-c("municip", "x")) %>%
  pivot_wider(values_from = "total", names_from = "tipo")

seeds_area_2017[is.na(seeds_area_2017)] = 0

seeds_area_2017 %<>% set_names(c("cod", "totarea_seeds", "com1", "com2", "cert", 
                                  "trans_area", "na")) %>%
  dplyr::select(-c("com1", "com2", "cert", "na")) %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))



## Deflating
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca_2006 <- ipca %>% filter(Date == 2006)
ipca_2017 <- ipca %>% filter(Date == 2017)


seeds_value_2006 %<>% mutate(trans_value = trans_value/(ipca_2006$Index_2/100)) %>%
  mutate(totval_seeds = totval_seeds/(ipca_2006$Index_2/100))

seeds_value_2017 %<>% mutate(trans_value = trans_value/(ipca_2017$Index_2/100)) %>%
  mutate(totval_seeds = totval_seeds/(ipca_2017$Index_2/100))


# Adding years and joining
seeds_value_2006 %<>% mutate(year = 2006)
seeds_value_2017 %<>% mutate(year = 2017)
seeds_area_2006 %<>% mutate(year = 2006)
seeds_area_2017 %<>% mutate(year = 2017)

seeds_value <- bind_rows(seeds_value_2006, seeds_value_2017)
seeds_area <- bind_rows(seeds_area_2006, seeds_area_2017)
seeds <- full_join(seeds_value, seeds_area, by = c("cod", "year")) %>%
  mutate(year = as.integer(year))


### Agroindustry
agroind_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/agroindustry/agroind_2006.xlsx", 
                                               skip = 5, sheet = "Tabela 4", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

agroind_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/agroindustry/agroind_2017.xlsx", 
                           skip = 5, sheet = 3, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# In mil reais
agroind_2006 %<>% set_names(c("cod", "municip", "x", "totval_agroind")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "municip")) %>%
  mutate(cod = as.integer(cod))


agroind_2017 %<>% set_names(c("cod", "municip", "x", "y", "totval_agroind")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip")) %>%
  mutate(cod = as.integer(cod))


agroind_2006[is.na(agroind_2006)] = 0
agroind_2017[is.na(agroind_2017)] = 0


# Deflating
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca_2006 <- ipca %>% filter(Date == 2006)
ipca_2017 <- ipca %>% filter(Date == 2017)


agroind_2006 %<>% mutate(totval_agroind = totval_agroind/(ipca_2006$Index_2/100))

agroind_2017 %<>% mutate(totval_agroind = totval_agroind/(ipca_2017$Index_2/100))



# Adding years and joining
agroind_2006 %<>% mutate(year = 2006)
agroind_2017 %<>% mutate(year = 2017)

agroind_val <- bind_rows(agroind_2006, agroind_2017)


### Type of Property
# 2006
prop_num_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/property/proprietario_2006.xlsx", 
                           skip = 6, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

prop_area_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/property/proprietario_2006.xlsx", 
                           skip = 6, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



prop_num_2006 %<>% set_names(c("cod", "municip", "x","y", "tot_num", "prop_num",
                               "assent", "arrend", "parceiro", "ocup", "narea")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip", "parceiro", "ocup", 
                                     "narea", "assent", "arrend")) %>%
  mutate(cod = as.integer(cod))


prop_area_2006 %<>% set_names(c("cod", "municip", "x","y", "tot_area", "prop_area",
                               "assent", "arrend", "parceiro", "ocup", "narea")) %>%
  slice(-5571) %>% dplyr::select(- c("x", "y", "municip", "parceiro", "ocup", 
                                     "narea", "assent", "arrend")) %>%
  mutate(cod = as.integer(cod))


prop_num_2006[is.na(prop_num_2006)] = 0
prop_area_2006[is.na(prop_area_2006)] = 0


# 2017
prop_num_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/property/proprietario_2017.xlsx", 
                            skip = 5, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

prop_area_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/property/proprietario_2017.xlsx", 
                             skip = 5, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



prop_num_2017 %<>% set_names(c("cod", "municip", "tipo", "x", "total"))
prop_area_2017 %<>% set_names(c("cod", "municip", "tipo", "x", "total"))


# Fills NA values only in the municip column
prop_num_2017[1] <- na.locf(prop_num_2017[1], fromLast = FALSE)

prop_num_2017[2] <- na.locf(prop_num_2017[2], fromLast = FALSE)

prop_num_2017 %<>% dplyr::select(-c("municip", "x")) %>%
  pivot_wider(values_from = "total", names_from = "tipo")

prop_num_2017[is.na(prop_num_2017)] = 0

prop_num_2017 %<>% set_names(c("cod", "tot_num", "prop_num", "con", "arrend", 
                                  "parc", "com", "ocup", "narea", "na")) %>%
  dplyr::select(-c("con", "arrend", "parc", "com", "ocup", "narea", "na")) %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))


# The same for area
prop_area_2017[1] <- na.locf(prop_area_2017[1], fromLast = FALSE)

prop_area_2017[2] <- na.locf(prop_area_2017[2], fromLast = FALSE)

prop_area_2017 %<>% dplyr::select(-c("municip", "x")) %>%
  pivot_wider(values_from = "total", names_from = "tipo")

prop_area_2017[is.na(prop_area_2017)] = 0

prop_area_2017 %<>% set_names(c("cod", "tot_area", "prop_area", "con", "arrend", 
                               "parc", "com", "ocup", "narea", "na")) %>%
  dplyr::select(-c("con", "arrend", "parc", "com", "ocup", "narea", "na")) %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))


# Adding years and joining
prop_num_2006 %<>% mutate(year = 2006)
prop_num_2017 %<>% mutate(year = 2017)
prop_area_2006 %<>% mutate(year = 2006)
prop_area_2017 %<>% mutate(year = 2017)

prop_num <- bind_rows(prop_num_2006, prop_num_2017)
prop_area <- bind_rows(prop_area_2006, prop_area_2017)
prop <- full_join(prop_num,prop_area, by = c("cod", "year")) %>%
  mutate(year = as.integer(year))



##### Joining with full dataset #####
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")

agro_struc <- full_join(agro_struc, seeds, by = c("cod", "year"))
agro_struc <- full_join(agro_struc, prop, by = c("cod", "year"))
agro_struc <- full_join(agro_struc, agroind_val, by = c("cod", "year"))

# Saving
save(agro_struc, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")



######### 2. Check Census for useful data for Structural Transformation ########

### Getting employment shares and wages

## 2000
wages_2000 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Demo/2000/Wages/wages_2000.xlsx", 
                         skip = 4, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

colnames(wages_2000) <- c("cod", "municip", "sector", "wage")

wages_2000 %<>% slice(-105831)

wages_2000 %<>% transform(municip = na.locf(municip, fromLast = FALSE))

wages_2000 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()

wages_2000 <- pivot_wider(wages_2000, names_from = "sector", values_from = "wage")

colnames(wages_2000) <- c("cod", "municip", "total", "agro", "pesca", "indust_ex", "indust_trans",
                          "distri","constru", "comrep", "transp", "aloj", "finan", 
                          "imob", "adm", "educ", "saude", "outros", "servdom", "int", "mal")


# Selecting only the desired ones
wages_2000 %<>% dplyr::select(c("cod", "municip", "agro", "pesca", "indust_ex", "indust_trans"))

# Calculating wages
wages_2000 %<>% rename(w_trans = "indust_trans") %>%
  mutate(w_agro = rowMeans(.[,3:4], na.rm=TRUE)) %>%
  mutate(w_indust = rowMeans(.[,5:6], na.rm=TRUE))


# Adding years
wages_2000 %<>% mutate(year = 2000)

# Deflating
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca_2000 <- ipca %>% filter(Date == 2000)

deflate <- function(x) x/(ipca_2000$Index_2/100)

wages_2000_real <- wages_2000 %>% mutate_at(c("agro", "pesca", "indust_ex", 
                                              "w_trans", "w_agro", "w_indust"), deflate)

wages_2000_real %<>% dplyr::select(-"pesca")

## 2010
wages_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Demo/2010/Wages/wages_2010.xlsx", 
                         skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

colnames(wages_2010) <- c("cod", "municip", "sector", "wage")

wages_2010 %<>% slice(-128111)

wages_2010 %<>% transform(municip = na.locf(municip, fromLast = FALSE))

wages_2010 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()

wages_2010 <- pivot_wider(wages_2010, names_from = "sector", values_from = "wage")

colnames(wages_2010) <- c("cod", "municip", "total", "w_agro", "indust_ex", "indust_trans",
                          "elec","agua", "constru", "com", "transp", "aloj", "info", "finan", 
                          "imob", "prof", "adm", "admpub", "educ", "saude", "art", "outros",
                          "dom", "int", "mal")


# Selecting only the desired ones
wages_2010 %<>% dplyr::select(c("cod", "municip", "w_agro", "indust_ex", "indust_trans"))


# Calculating wages
wages_2010 %<>% rename(w_trans = "indust_trans") %>%
  mutate(w_indust = rowMeans(.[,4:5], na.rm=TRUE))


# Adding years
wages_2010 %<>% mutate(year = 2010)


# Joining for wages
sector_wages <- bind_rows(wages_2000_real, wages_2010)



atlas_shares <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Atlas PNUD/atlas2013_dadosbrutos_pt.xlsx",
                           sheet = "MUN 91-00-10", col_names = TRUE)


atlas_shares %<>% dplyr::select(c("ANO", "Codmun7", "Município", "P_AGRO", "P_SERV", "P_COM",
                                  "P_CONSTR", "P_EXTR",  "P_TRANSF"))

atlas_shares[atlas_shares == "-" ] <- NA

atlas_shares %<>% rename(year = "ANO", cod = "Codmun7", municip = "Município")


atlas_shares %<>% mutate(P_AGRO = as.double(P_AGRO)) %>%  mutate(P_SERV = as.double(P_SERV)) %>%
  mutate(P_COM = as.double(P_COM)) %>% mutate(P_CONSTR = as.double(P_CONSTR)) %>%
  mutate(P_EXTR = as.double(P_EXTR)) %>%
  mutate(P_TRANSF = as.double(P_TRANSF))

atlas_shares %<>% filter(year != 1991)

atlas_shares %<>% mutate(P_INDUST = P_EXTR + P_TRANSF)


sector_wages %<>% mutate(cod = as.integer(cod))

popstruc_pres <- inner_join(sector_wages, atlas_shares, by = c("cod", "year"))

# Saving
save(popstruc_pres, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/pop_struc.RData")






