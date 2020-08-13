# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 1. Reads Censo Agro data on area and numbers of agricultural establishments by groups of total area ###################################################

# Reads data from Censo Agro IBGE
num_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/tabela6880_num_def_2017.xlsx",
                         skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

area_2017 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2017/tabela6880_area_def_2017.xlsx",
                        skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

num_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/tabela837_num_def_2006.xlsx",
                       skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


area_2006 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/2006/tabela837_area_def_2006.xlsx",
                        skip = 2, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Tabela 2 shows percentages; did some beforehand changes on the excel files
num_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/tabela312_num_def_1995.xlsx",
                       sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
  
area_1995 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Censo Agro/1995/tabela315_area_def_1995.xlsx",
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

area_2006 <- area_2006 %>% rename(municip = "Município, em ordem de código de UF e código de Município", group = "Grupos de área total" , 
                                  area = "Ano x Grupos de atividade econômica x Condição do produtor em relação às terras", cod = "Cód.") %>%
  dplyr::select(c(cod, municip, group, area))

area_2006 <- area_2006 %>% slice(4:105833)
area_2006 <- area_2006 %>% transform(municip = na.locf(municip, fromLast = FALSE))
area_2006 <- area_2006 %>% transform(cod = na.locf(cod, fromLast = FALSE)) %>%
  arrange(cod) %>%
  as_tibble()


#num_2017_prod <- num_2017 %>% filter(group != c("Produtor sem área")

# 1995
num_1995 <- num_1995 %>% rename(municip = "Município, em ordem de código de UF e nome de Município", cod = "Cód.") %>%
  arrange(cod)


area_1995 <- area_1995 %>% rename(municip = "Município, em ordem de código de UF e código de Município", cod = "Cód.") %>%
  arrange(cod)




######### 2. Sets up the data to be used in Stata ############################################################################################################

num_2006 <- num_2006 %>% mutate(num = as.numeric(num))
area_2006 <- area_2006 %>% mutate(area = as.numeric(area))


for(i in 1:length(num_2006$num)){if(is.na(num_2006$num[i])){num_2006$num[i]=0}}
for(i in 1:length(area_2006$area)){if(is.na(area_2006$area[i])){area_2006$area[i]=0}}

num_2006 <- num_2006 %>% as.numeric(num)



mean_2006 <- mean_2006 %>% mutate(mean = area_2006$area/num_2006$num) %>%
  dplyr::select(-num)

for(i in 1:length(mean_2006$mean)){if(is.nan(mean_2006$mean[i])){mean_2006$mean[i]=0}}



# Saving the dataset in Stata format
write.dta(num_2006, "num_2006_agro.dta")
write.dta(area_2006, "area_2006_agro.dta")


num_2006 <- num_2006 %>% filter(group != "Total")
area_2006 <- area_2006 %>% filter(group != "Total")
agro_2006 <- full_join(num_2006, area_2006, by = c("cod", "group", "municip"))


write.dta(agro_2006, "agro_2006.dta")







