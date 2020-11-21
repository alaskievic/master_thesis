# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")



######### 1. Reads data for temperature and rainfall. #############################################################


raintemp<- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/Daniel_raintemp/base_clima_br_1950_2017_13.dta")

raintemp %<>% as_tibble() %>% rename(cod2 = "codigo_IBGE") %>%
  dplyr::select(c("cod2", "uf", "ano", "lat", "longit", "v_rain", "v_temp"))

raintemp %<>% rename (year = ano)

raintemp %<>% group_by(cod2) %>% mutate(rain = sum(v_rain, na.rm = TRUE)) %>%
  mutate(temp = sum(v_temp, na.rm = TRUE)) %>% ungroup() %>% filter(year == 2010) %>%
  dplyr::select(-"year") %>% mutate(cod2=as.integer(cod2)) %>%
  mutate(temp_daniel = temp/68) %>% mutate(rain_daniel = rain/68) %>%
  ungroup()


## Load previous controls
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")

controls %<>% group_by(cod) %>% 
  mutate(rain_sum = sum(rain_primavera + rain_inverno + rain_verao + rain_outono, na.rm = TRUE)) %>% 
  mutate(temp_sum = sum(temp_primavera + temp_inverno + temp_verao + temp_outono, na.rm = TRUE)) %>%
  mutate(rain_ipea = rain_sum/4) %>% mutate(temp_ipea = temp_sum/4) %>%
  ungroup()

  
controls %<>% dplyr::select(-c("temp_outono", "temp_primavera", "temp_verao", "temp_inverno", 
                              "rain_primavera", "rain_inverno", "rain_verao", "rain_outono",
                              "rain_sum", "temp_sum"))

controls$cod2 <- stri_sub(controls$cod, 1, -2)

controls %<>% mutate(cod2 = as.integer(cod2))

# Merging
controls_final <- inner_join(controls, raintemp, by = "cod2")


controls_final %<>% mutate(rain_ipea = rain_ipea*12) %>% 
  mutate(rain_ipea  = ifelse(rain_ipea == 0, NA, rain_ipea)) %>%
  mutate(rain_daniel  = ifelse(rain_daniel == 0, NA, rain_daniel)) %>%
  mutate(temp_ipea  = ifelse(temp_ipea == 0, NA, temp_ipea)) %>%
  mutate(temp_daniel  = ifelse(temp_daniel == 0, NA, temp_daniel))


controls_final$rain_ipea <- ifelse(is.na(controls_final$rain_ipea), controls_final$rain_daniel, controls_final$rain_ipea)
controls_final$rain_daniel <- ifelse(is.na(controls_final$rain_daniel), controls_final$rain_ipea, controls_final$rain_daniel)
controls_final$temp_ipea <- ifelse(is.na(controls_final$temp_ipea), controls_final$temp_daniel, controls_final$temp_ipea)
controls_final$temp_daniel <- ifelse(is.na(controls_final$temp_daniel), controls_final$temp_ipea, controls_final$temp_daniel)

controls_final %<>% dplyr::select(-c("v_rain", "v_temp", "rain", "temp"))


#### Addign shares
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/shares_fao_1995.Rdata")

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/full_fao.dta")

# select only both shares
fao_pr %<>% dplyr::select(-c(21:62)) %>% as_tibble() %>% mutate(cod = as.integer(cod))

# Join all
controls_baseline <- inner_join(controls_final, fao_pr, by = "cod")


##### Saving
save(controls_baseline, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/controls_baseline.Rdata")














