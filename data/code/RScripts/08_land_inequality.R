# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")


##### 1. Reads Censo Agro data on area and numbers of agricultural #############
######### Establishments by groups of total area

# Reads data from Censo Agro IBGE
num_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/tabela6880_num_def_2017.xlsx",
                         skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

area_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/tabela6880_area_def_2017.xlsx",
                        skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

num_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/tabela837_num_def_2006.xlsx",
                       skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


area_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/tabela837_area_def_2006.xlsx",
                        skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Tabela 2 shows percentages; did some beforehand changes on the excel files
num_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/tabela312_num_def_1995.xlsx",
                       sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
  
area_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/tabela315_area_def_1995.xlsx",
                      sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

  
# Cleaning 2017 data
num_2017 <- num_2017 %>% rename(municip = "Município, em ordem de código de UF e código de Município", group = "Grupos de área total" , 
                                num = "Ano x Tipologia x Grupos de atividade econômica", cod = "Cód.") %>%
  dplyr::select(c(cod, municip, group, num))

num_2017 <- num_2017 %>% slice(4:111403)
num_2017 <- num_2017 %>% transform(municip = na.locf(municip, fromLast = FALSE))
num_2017 <- num_2017 %>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()

area_2017 <- area_2017 %>% rename(municip = "Município, em ordem de código de UF e código de Município", group = "Grupos de área total" , 
                                area = "Ano x Tipologia x Grupos de atividade econômica", cod = "Cód.") %>%
  dplyr::select(c(cod, municip, group, area))

area_2017 <- area_2017 %>% slice(4:111403)
area_2017 <- area_2017 %>% transform(municip = na.locf(municip, fromLast = FALSE))
area_2017 <- area_2017 %>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()

# 2006
num_2006 <- num_2006 %>% rename(municip = "Município, em ordem de código de UF e código de Município", group = "Grupos de área total" , 
                                num = "Ano x Grupos de atividade econômica x Condição do produtor em relação às terras", cod = "Cód.") %>%
  dplyr::select(c(cod, municip, group, num))

num_2006 <- num_2006 %>% slice(4:105833)
num_2006 <- num_2006 %>% transform(municip = na.locf(municip, fromLast = FALSE))
num_2006 <- num_2006 %>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()

area_2006 <- area_2006 %>% 
  rename(municip = "Município, em ordem de código de UF e código de Município",
         group = "Grupos de área total" , 
         area = "Ano x Grupos de atividade econômica x Condição do produtor em relação às terras", cod = "Cód.") %>%
  dplyr::select(c(cod, municip, group, area))

area_2006 <- area_2006 %>% slice(4:105833)
area_2006 <- area_2006 %>% transform(municip = na.locf(municip, fromLast = FALSE))
area_2006 <- area_2006 %>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()


# 1995
num_1995 <- num_1995 %>%
  rename(municip = "Município, em ordem de código de UF e nome de Município",
         cod = "Cód.") %>%
  arrange(cod)


area_1995 <- area_1995 %>% 
  rename(municip = "Município, em ordem de código de UF e código de Município",
         cod = "Cód.") %>%
  arrange(cod)


##### 2. Calculate some inequality measures ####################################

# Percentage of total area appropriate by farm with more than 1.000 hectares

# 1995
num_app_95 <- num_1995 %>%
  dplyr::select(c("cod", "Total", "1.000 a menos de 2.000 ha", "2.000 a menos de 5.000 ha", 
                  "5.000 a menos de 10.000 ha", "10.000 a menos de 100.000 ha",
                  "100.000 ha e mais"))

area_app_95 <- area_1995 %>% 
  dplyr::select(c("cod", "Total", "1.000 a menos de 2.000 ha", "2.000 a menos de 5.000 ha", 
                  "5.000 a menos de 10.000 ha", "10.000 a menos de 100.000 ha",
                  "100.000 ha e mais"))

colnames(num_app_95) <- c("cod", "total_num", "area_1", "area_2", "area_3",
                          "area_4", "area_5")
colnames(area_app_95) <- c("cod", "total_area", "area_1", "area_2", "area_3",
                           "area_4", "area_5")

num_app_95[is.na(num_app_95)] = 0
area_app_95[is.na(area_app_95)] = 0

num_app_95 %<>% mutate(napp = (area_1 + area_2 + area_3 + area_4 + area_5)/total_num) %>% 
  dplyr::select(cod, total_num, napp)

area_app_95 %<>% mutate(arapp = (area_1 + area_2 + area_3 + area_4 + area_5)/total_area) %>%
  dplyr::select(cod, total_area, arapp)

app_95 <- full_join(num_app_95, area_app_95, by = "cod")
app_95 %<>% mutate(year = 1995) %>% mutate(cod = as.integer(cod))


# 2006
num_app_06 <- num_2006 %>% filter (group == "Total" | group =="De 2.500 ha e mais" |
                                     group == "De 1.000 a menos de 2.500 ha") %>%
  pivot_wider(names_from = "group", values_from = "num") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 ha e mais` = as.integer(`De 2.500 ha e mais`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(cod = as.integer(cod))

area_app_06 <- area_2006 %>% filter (group == "Total" | group =="De 2.500 ha e mais" |
                                       group == "De 1.000 a menos de 2.500 ha") %>%
  pivot_wider(names_from = "group", values_from = "area")  %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 ha e mais` = as.integer(`De 2.500 ha e mais`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(cod = as.integer(cod))

colnames(num_app_06) <- c("cod", "municip", "total_num", "area_1", "area_2")
colnames(area_app_06) <- c("cod", "municip", "total_area", "area_1", "area_2")

num_app_06[is.na(num_app_06)] = 0
area_app_06[is.na(area_app_06)] = 0

num_app_06 %<>% mutate(napp = (area_1 + area_2)/total_num) %>% 
  dplyr::select(cod, total_num, napp)

area_app_06 %<>% mutate(arapp = (area_1 + area_2)/total_area) %>%
  dplyr::select(cod, total_area, arapp)


app_06 <- full_join(num_app_06, area_app_06, by = "cod")
app_06 %<>% mutate(year = 2006) %>% mutate(cod = as.integer(cod))

# 2017
num_app_17 <- num_2017 %>% filter (group == "Total" | group == "De 1.000 a menos de 2.500 ha" |
                                   group == "De 2.500 a menos de 10.000 ha" |
                                   group == "De 10.000 ha e mais")  %>%
  pivot_wider(names_from = "group", values_from = "num") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(`De 2.500 a menos de 10.000 ha` = as.integer(`De 2.500 a menos de 10.000 ha`)) %>%
  mutate(`De 10.000 ha e mais` = as.integer(`De 10.000 ha e mais`)) %>%
  mutate(cod = as.integer(cod))


area_app_17 <- area_2017 %>% filter (group == "Total" | group == "De 1.000 a menos de 2.500 ha" |
                                     group == "De 2.500 a menos de 10.000 ha" |
                                     group == "De 10.000 ha e mais")  %>%
  pivot_wider(names_from = "group", values_from = "area") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(`De 2.500 a menos de 10.000 ha` = as.integer(`De 2.500 a menos de 10.000 ha`)) %>%
  mutate(`De 10.000 ha e mais` = as.integer(`De 10.000 ha e mais`)) %>%
  mutate(cod = as.integer(cod))

colnames(num_app_17) <- c("cod", "municip", "total_num", "area_1",
                          "area_2", "area_3")
colnames(area_app_17) <- c("cod", "municip", "total_area", "area_1",
                           "area_2", "area_3")

num_app_17[is.na(num_app_17)] = 0
area_app_17[is.na(area_app_17)] = 0

num_app_17 %<>% mutate(napp = (area_1 + area_2 + area_3)/total_num) %>% 
  dplyr::select(cod, total_num, napp)

area_app_17 %<>% mutate(arapp = (area_1 + area_2 + area_3)/total_area) %>%
  dplyr::select(cod, total_area, arapp)



app_17 <- full_join(num_app_17, area_app_17, by = "cod")
app_17 %<>% mutate(year = 2017) %>% mutate(cod = as.integer(cod))

# Joining
land_app <- bind_rows(app_95, app_06)
land_app <- bind_rows(land_app, app_17)

land_app$napp[is.nan(land_app$napp)]<-0
land_app$arapp[is.nan(land_app$arapp)]<-0

# Saving
save(land_app, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/land_app.Rdata")

######### 3. Sets up the data to be used in Stata ##############################

# 2017
num_2017 <- num_2017 %>% mutate(num = as.numeric(num))
area_2017 <- area_2017 %>% mutate(area = as.numeric(area))

for(i in 1:length(num_2017$num)){if(is.na(num_2017$num[i])){num_2017$num[i]=0}}
for(i in 1:length(area_2017$area)){if(is.na(area_2017$area[i])){area_2017$area[i]=0}}

num_2017 <- num_2017 %>% filter(group != "Total")
area_2017 <- area_2017 %>% filter(group != "Total")
agro_2017 <- full_join(num_2017, area_2017, by = c("cod", "group", "municip"))


# 2006
num_2006 <- num_2006 %>% mutate(num = as.numeric(num))
area_2006 <- area_2006 %>% mutate(area = as.numeric(area))

for(i in 1:length(num_2006$num)){if(is.na(num_2006$num[i])){num_2006$num[i]=0}}
for(i in 1:length(area_2006$area)){if(is.na(area_2006$area[i])){area_2006$area[i]=0}}

num_2006 <- num_2006 %>% filter(group != "Total")
area_2006 <- area_2006 %>% filter(group != "Total")
agro_2006 <- full_join(num_2006, area_2006, by = c("cod", "group", "municip"))

# 1995
# Changing to long format
num_1995_long <- pivot_longer(num_1995, -c("cod", "municip"), values_to = "num",
                              names_to = "group")
area_1995_long <- pivot_longer(area_1995, -c("cod", "municip"), values_to = "area",
                               names_to = "group")

for(i in 1:length(num_1995_long$num)){if(is.na(num_1995_long$num[i])){num_1995_long$num[i]=0}}
for(i in 1:length(area_1995_long$area)){if(is.na(area_1995_long$area[i])){area_1995_long$area[i]=0}}

agro_1995 <- full_join(num_1995_long, area_1995_long, by = c("cod", "group", "municip"))
agro_1995 <- agro_1995 %>% filter(group != "Total") %>%
  filter(group != "Sem declaração")

# Saving the dataset in Stata format
write.dta(agro_2006, "agro_2006.dta")
write.dta(agro_2017, "agro_2017.dta")
write.dta(agro_1995, "agro_1995.dta")


##### 4. Reads area by group and type of property for land inequality ##########

# 1995
# agroupa_arrend_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/ineq_group/area_arrend_groupa_1995.xlsx",
#                        skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
# 
# agroupa_ocup_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/ineq_group/area_ocup_groupa_1995.xlsx",
#                                   skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
# 
# agroupa_parc_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/ineq_group/area_parc_groupa_1995.xlsx",
#                                   skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

agroupa_prop_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/ineq_group/area_prop_groupa_1995.xlsx",
                                  skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ngroupa_prop_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/1995/ineq_group/num_prop_groupa_1995.xlsx",
                                skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


agroupa_prop_1995 %<>% dplyr::select(cod = "Cód.", totarea_prop = "Total", 
                                       ineq1 = "1.000 a menos de 2.000 ha",
                                       ineq2 = "2.000 a menos de 5.000 ha", 
                                       ineq3 = "5.000 a menos de 10.000 ha", 
                                       ineq4 = "10.000 a menos de 100.000 ha", 
                                       ineq5 = "100.000 ha e mais") %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))

ngroupa_prop_1995 %<>% dplyr::select(cod = "Cód.", totnum_prop = "Total", 
                                     ineq1 = "1.000 a menos de 2.000 ha",
                                     ineq2 = "2.000 a menos de 5.000 ha", 
                                     ineq3 = "5.000 a menos de 10.000 ha", 
                                     ineq4 = "10.000 a menos de 100.000 ha", 
                                     ineq5 = "100.000 ha e mais") %>%
  slice(-5571) %>% mutate(cod = as.integer(cod))


agroupa_prop_1995[is.na(agroupa_prop_1995)] = 0
ngroupa_prop_1995[is.na(ngroupa_prop_1995)] = 0

agroupa_prop_1995 %<>% mutate(proparapp = ineq1 + ineq2 + ineq3 + ineq4 + ineq5) %>%
  dplyr::select(cod, totarea_prop, proparapp)

ngroupa_prop_1995 %<>% mutate(propnapp = ineq1 + ineq2 + ineq3 + ineq4 + ineq5) %>%
  dplyr::select(cod, totnum_prop, propnapp)

#Merging
groupa_prop_1995 <- full_join(agroupa_prop_1995, ngroupa_prop_1995, by = "cod")

groupa_prop_1995 %<>% mutate(year = 1995)



#2006
agroupa_prop_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/ineq_group/area_prop_groupa_2006.xlsx",
                                skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ngroupa_prop_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2006/ineq_group/num_prop_groupa_2006.xlsx",
                                skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

colnames(agroupa_prop_2006) <- c("cod", "municip", "x", "group", "proparapp")
colnames(ngroupa_prop_2006) <- c("cod", "municip", "group", "x", "propnapp")


agroupa_prop_2006 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  filter (group == "Total" | group =="De 2.500 ha e mais" |
            group == "De 1.000 a menos de 2.500 ha") %>%
  dplyr::select(cod, group, proparapp) %>%
  pivot_wider(names_from = "group", values_from = "proparapp") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 ha e mais` = as.integer(`De 2.500 ha e mais`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(cod = as.integer(cod))
  
ngroupa_prop_2006 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  filter (group == "Total" | group =="De 2.500 ha e mais" |
            group == "De 1.000 a menos de 2.500 ha") %>%
  dplyr::select(cod, group, propnapp) %>%
  pivot_wider(names_from = "group", values_from = "propnapp") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 ha e mais` = as.integer(`De 2.500 ha e mais`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(cod = as.integer(cod))

agroupa_prop_2006[is.na(agroupa_prop_2006)] = 0
ngroupa_prop_2006[is.na(ngroupa_prop_2006)] = 0

colnames(agroupa_prop_2006) <- c("cod", "totarea_prop", "ineq1", "ineq2")
colnames(ngroupa_prop_2006) <- c("cod", "totnum_prop", "ineq1", "ineq2")

agroupa_prop_2006 %<>% mutate(proparapp = ineq1 + ineq2) %>%
  dplyr::select(cod, totarea_prop, proparapp)

ngroupa_prop_2006 %<>% mutate(propnapp = ineq1 + ineq2) %>%
  dplyr::select(cod, totnum_prop, propnapp)

#Merging
groupa_prop_2006 <- full_join(agroupa_prop_2006, ngroupa_prop_2006, by = "cod")
groupa_prop_2006 %<>% mutate(year=2006)


#2017
##This is for legal condition of the lands
agroupa_prop_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/ineq_group/area_prop_groupa_2017.xlsx",
                                skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ngroupa_prop_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Agro/2017/ineq_group/num_prop_groupa_2017.xlsx",
                                skip = 6, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



colnames(agroupa_prop_2017) <- c("cod", "municip", "x", "group", "proparapp")
colnames(ngroupa_prop_2017) <- c("cod", "municip", "x", "y", "group", "propnapp")


agroupa_prop_2017 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  filter (group == "Total" | group =="De 2.500 a menos de 10.000 ha" |
            group == "De 1.000 a menos de 2.500 ha" | 
            group == "De 10.000 ha e mais") %>%
  dplyr::select(cod, group, proparapp) %>%
  pivot_wider(names_from = "group", values_from = "proparapp") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 a menos de 10.000 ha` = as.integer(`De 2.500 a menos de 10.000 ha`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(`De 10.000 ha e mais` = as.integer(`De 10.000 ha e mais`)) %>%
  mutate(cod = as.integer(cod))


ngroupa_prop_2017 %<>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  filter (group == "Total" | group =="De 2.500 a menos de 10.000 ha" |
            group == "De 1.000 a menos de 2.500 ha" |
            group == "De 10.000 ha e mais") %>%
  dplyr::select(cod, group, propnapp) %>%
  pivot_wider(names_from = "group", values_from = "propnapp") %>%
  mutate(Total = as.integer(Total)) %>%
  mutate(`De 2.500 a menos de 10.000 ha` = as.integer(`De 2.500 a menos de 10.000 ha`)) %>%
  mutate(`De 1.000 a menos de 2.500 ha` = as.integer(`De 1.000 a menos de 2.500 ha`)) %>%
  mutate(`De 10.000 ha e mais` = as.integer(`De 10.000 ha e mais`)) %>%
  mutate(cod = as.integer(cod))

agroupa_prop_2017[is.na(agroupa_prop_2017)] = 0
ngroupa_prop_2017[is.na(ngroupa_prop_2017)] = 0

colnames(agroupa_prop_2017) <- c("cod", "totarea_prop", "ineq1", "ineq2", "ineq3")
colnames(ngroupa_prop_2017) <- c("cod", "totnum_prop", "ineq1", "ineq2", "ineq3")

agroupa_prop_2017 %<>% mutate(proparapp = ineq1 + ineq2 + ineq3) %>%
  dplyr::select(cod, totarea_prop, proparapp)

ngroupa_prop_2017 %<>% mutate(propnapp = ineq1 + ineq2 + ineq3) %>%
  dplyr::select(cod, totnum_prop, propnapp)

#Merging
groupa_prop_2017 <- full_join(agroupa_prop_2017, ngroupa_prop_2017, by = "cod")
groupa_prop_2017 %<>% mutate(year=2017)



#Final Merge
groupa_prop <- bind_rows(groupa_prop_1995, groupa_prop_2006)
groupa_prop <- bind_rows(groupa_prop, groupa_prop_2017)


save(groupa_prop, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/groupa_prop.Rdata")

