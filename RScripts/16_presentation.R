# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")



######### 1. Reads data for temperature and rainfall. #############################################################


raintemp<- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/IPEA/Controles/Daniel_raintemp/base_clima_br_1950_2017_13.dta")

raintemp %<>% as_tibble() %>% rename(cod2 = "codigo_IBGE") %>%
  dplyr::select(c("cod2", "uf", "ano", "lat", "longit", "v_rain", "v_temp"))

raintemp %<>% rename (year = ano)

raintemp %<>% group_by(cod2) %>% mutate(rain = sum(v_rain, na.rm = TRUE)) %>%
  mutate(temp = sum(v_temp, na.rm = TRUE)) %>% ungroup() %>% filter(year == 2010) %>%
  dplyr::select(-"year") %>% mutate(cod2=as.integer(cod2)) %>%
  mutate(temp_daniel = temp/68) %>% mutate(rain_daniel = rain/68) %>%
  ungroup()


## Load previous controls
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/controls.RData")

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
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/shares_fao_1995.Rdata")

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/full_fao.dta")

# select only both shares
fao_pr %<>% dplyr::select(-c(21:62)) %>% as_tibble() %>% mutate(cod = as.integer(cod))

# Join all
controls_baseline <- inner_join(controls_final, fao_pr, by = "cod")


##### Saving
save(controls_baseline, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/controls_baseline.Rdata")



######### 2. Adding rural and urban population, literacy rate, income and population density in 1991 ###########

# Load again controls
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/controls_baseline.Rdata")


# Read Atlas Mun and take the variables
atlas_1991 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Atlas PNUD/atlas2013_dadosbrutos_pt.xlsx",
                        sheet = "MUN 91-00-10", col_names = TRUE)


atlas_1991 %<>% dplyr::select(c("ANO", "Codmun7", "Município",  "RDPC", "T_ANALF15M",
                               "pesoRUR", "T_ANALF15M", "RDPCT",
                               "pesourb", "pesotot"))

atlas_1991[atlas_1991 == "-" ] <- NA

atlas_1991 %<>% rename(year = "ANO", cod = "Codmun7", municip = "Município", 
                      income_1991 = "RDPC", income0_1991 = "RDPCT", analf_1991 = "T_ANALF15M", 
                      pesorur_1991 = "pesoRUR", pesourb_1991 = "pesourb", 
                      pesotot_1991 = "pesotot")


### Filter for year 1991
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_gini.Rdata")

atlas_1991 %<>% filter(year == 1991 ) %>% dplyr::select(-"year")


# Merging with baseline measures
controls_baseline <- inner_join(controls_baseline, atlas_1991, by = "cod")

controls_baseline %<>% mutate(pop_dens_1991 = pesotot_1991/geo_area_2000)

baseline_full <- inner_join(baseline_gini, atlas_1991, by = "cod")
baseline_full %<>% mutate(pop_dens_1991 = pesotot_1991/geo_area_2000)


save(baseline_full, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")
save(controls_baseline, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/controls_baseline.Rdata")



# Load population
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/pop_sidra.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")

baseline_full <- inner_join(baseline_full, pop_sidra, by = c("cod", "year"))
save(baseline_full, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")


## Assigning land inequality
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")

aux_land1 <- land_gini %>% mutate(year = ifelse(year == 1995, 2000, year)) %>%
  mutate(year = ifelse(year == 2006, 2010, year)) %>% filter(year != 2017) %>%
  rename(gini_land1 = "gini_land")


aux_land2 <- land_gini %>% mutate(year = ifelse(year == 2006, 2000, year)) %>%
  mutate(year = ifelse(year == 2017, 2010, year)) %>% filter(year != 1995) %>%
  rename(gini_land2 = "gini_land")

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")

baseline_full<- full_join(baseline_full, aux_land1, by = c("cod", "year"))
baseline_full <- full_join(baseline_full, aux_land2, by = c("cod", "year"))

save(baseline_full, file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")



######### 3. Constructing revenues and expenditures dataset ##################################################

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/exp_real_long.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/rev_final.Rdata")

rev_final <- rev_final %>% mutate(cod = as.integer(cod), year = as.integer(year)) %>% arrange(cod)
exp_real_long <- exp_real_long %>% mutate(cod = as.integer(cod), year = as.integer(year)) %>% arrange(cod)

pres_fin <- inner_join(baseline_full, rev_final, by = c("cod", "year"))


pres_fin <- inner_join(pres_fin, exp_real_long, by = c("cod", "year"))

# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(pres_fin, "pres_fin.dta")


######### 4. Constructing municipalities GDP dataset ##################################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/municip_pib_real.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")

pres_pib <- inner_join(baseline_full, municip_pib_real, by = c("cod", "year"))


# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")

write.dta(pres_pib , "pres_pib .dta")


######### 5. Constructing censo pop structural transformation dataset ##########

load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")


### Getting employment shares ans wages

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


## Now getting employment shares from PNUD
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


# Uniting all for POP STRUCTURAL
sector_wages %<>% mutate(cod = as.integer(cod))

popstruc_pres <- inner_join(sector_wages, atlas_shares, by = c("cod", "year"))

popstruc_pres <- inner_join(popstruc_pres, baseline_full, by = c("cod", "year"))

# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")
write.dta(popstruc_pres, "popstruc_pres.dta")



######### 6. Constructing censo agro structural transformation dataset ##################################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")

agro_struc %<>% mutate(cod = as.integer(cod)) %>% mutate(year = as.integer(year))

agro_struc <- inner_join(agro_struc, land_gini, by = c("cod", "year"))


baseline_full_1 <- baseline_full %>% filter(year == 2000| year == 2010 | year == 2015)

baseline_full_1 %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2010, 2006, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))


baseline_full_2 <-baseline_full %>% filter(year == 2000| year == 2006|  year == 2015)

baseline_full_2 %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))


agrostruc_baseline <- full_join(agro_struc, baseline_full_1 , by = c("cod", "year"))

agrostruc_pres2 <- full_join(agro_struc, baseline_full_2, by = c("cod", "year"))



# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")
write.dta(agrostruc_baseline, "agrostruc_baseline.dta")
write.dta(agrostruc_pres2, "agrostruc_pres2.dta")


######### 7.ITR Agreement ######################################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/itr_conv_final.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/baseline_full.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")


baseline_full %<>% filter(year==2000 | year==2010| year==2015)

measure_itr <- baseline_full %>% dplyr::select(c("cod", "municip.x", "year", "sum_fao", "pq_sum"))

measure_itr <- pivot_wider(measure_itr, names_from = "year", values_from = c("sum_fao", "pq_sum"))

gini_itr <- pivot_wider(land_gini, names_from = "year", values_from = "gini_land")

gini_itr %<>% set_names(c("cod", "municip", "landgini_1995", "landgini_2006", "landgini_2017"))


baseline_full %<>% filter(year==2000) %>% dplyr::select(-c("sum_fao", "pq_sum"))

itr_conv_final <- itr_conv_final[1:19]

itr_pres <- inner_join(itr_conv_final, measure_itr, by = "cod")

itr_pres <- inner_join(itr_pres, baseline_full, by = "cod")

itr_pres <- inner_join(itr_pres, gini_itr, by = "cod")

setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")
write.dta(itr_pres, "itr_pres.dta")


######### 8. Some Graphs #######################################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agro_struc.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")

agro_struc %<>% mutate(year = as.integer(year))

aux_graph1 <- inner_join(agro_struc, land_gini, by = c("cod", "year"))

# Agr Prod X Land Gini
aux_graph1 %<>% mutate(log_valw = log(val_outpw)) %<>% mutate(log_val = log(totval/total_area)) %>%
  mutate(log_linten = log(l_inten))

aux_graph1 %<>% filter(year == 2017) %>%
  filter(gini_land !=0) %>%
  filter(log_val !=0 & log_val != -Inf) %>%
  filter(total_area !=0) 



agrprod_ineq <- ggplot(aux_graph1, aes(x = gini_land, y = log_val)) + 
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
               formula = y~x, 
               parse = TRUE) +
  theme_bw(base_size = 13) +
  labs(x = "Landholding Gini Coefficient", y = "Log Agricultural Output per Ha") +
  ggtitle("Agricultural output and land distribution")

agrprod_ineq


linten_ineq <- ggplot(aux_graph1, aes(x = gini_land, y = log_linten)) + 
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
               formula = y~x, 
               parse = TRUE) +
  theme_bw(base_size = 13) +
  labs(x = "Landholding Gini Coefficient", y = "Log Labor Intensity in Agriculture") +
  ggtitle("Labor intensity in agriculture and land distribution")

linten_ineq

  
# ggsave(filename = "com_prices.png", plot = graph_7, path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")


setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")

ggsave("agrprod_ineq.png")
ggsave("linten_ineq.png")


pop_graph <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/popstruc_pres.dta")

pop_graph %<>% mutate(log_wagro = log(w_agro))


w_ineq <- ggplot(pop_graph, aes(x = gini_land1, y = log_wagro)) + 
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
               formula = y~x, 
               parse = TRUE) +
  theme_bw(base_size = 13) +
  labs(x = "Landholding Gini Coefficient", y = "Log Labor Intensity in Agriculture") +
  ggtitle("Labor intensity in agriculture and land distribution")

w_ineq


emp_ineq <- ggplot(pop_graph, aes(x = gini_land1, y = P_AGRO)) + 
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
               formula = y~x, 
               parse = TRUE) +
  theme_bw(base_size = 13) +
  labs(x = "Landholding Gini Coefficient", y = "Log Labor Intensity in Agriculture") +
  ggtitle("Labor intensity in agriculture and land distribution")

emp_ineq 


######### Add Shares of Women that are PEA #####################################

# Muitos NA não dá certo
pea <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/IPEA/Controles/pea_demo.xlsx", 
                               skip = 5, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


pea %<>% slice(-5571) %>%
  dplyr::select(1:13) %>% set_names(c("cod", "municip", "x", "y", 
                                             "tot", "men", "women", "tot_pea",
                                             "men_pea", "women_pea", "tot_npea",
                                             "men_npea", "women_npea")) %>%
  mutate(share_womenpea_1991 = women_pea/tot_pea) %>%
  dplyr::select(cod, municip, share_womenpea_1991)


teste <- filter(pea, is.na(pea$share_womenpea_1991))

######### Add Micro, Mesoregion and Sates ######################################

microreg <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/cod_ibge/mun_microreg.xlsx", 
                      sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

mesoreg <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/cod_ibge/mun_mesoreg.xlsx", 
                      sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


state <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/cod_ibge/mun_state.xlsx", 
                      sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


microreg %<>% set_names(c("codmicro", "microreg", "cod", "municip"))
mesoreg %<>% set_names(c("codmeso", "mesoreg", "cod", "municip"))
state %<>% set_names(c("codstate", "state", "cod", "municip"))

# Count unique entries to check
teste <- microreg %>% distinct(codmicro, .keep_all = TRUE)
teste2 <- mesoreg %>% distinct(codmeso, .keep_all = TRUE)
teste3 <- state %>% distinct(codstate, .keep_all = TRUE)
# All ok!

# Merge
mun_codes <- inner_join(microreg, mesoreg, by = c("cod", "municip"))
mun_codes <- inner_join(mun_codes, state, by = c("cod", "municip"))

# Merge with final datasets

pop_struc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/popstruc_pres.dta")

agro_struc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/agro_struc_baseline.dta")


pop_struc <- inner_join(pop_struc, mun_codes, by = "cod")
agro_struc <- inner_join(agro_struc, mun_codes, by = "cod")


# Saving
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles")
write.dta(pop_struc, "pop_struc.dta")
write.dta(agro_struc, "agro_struc_baseline.dta")



######### 10. Mapping ##########################################################

# Reads IBGE's 2019 shapefile containing 5570 municipalities
shp_ibge <-  readOGR("C:/Users/Andrei/Desktop/Dissertation/Analysis/Shapefiles/br_municipios_2019", "BR_Municipios_2019", stringsAsFactors = F)

# Reads shapefiles for state borders
shp_ufs <- readOGR("C:/Users/Andrei/Desktop/Dissertation/Analysis/Shapefiles/uf_2019", "BR_UF_2019", stringsAsFactors = F)


# Get unique values for each municipality
popstruc <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc.dta")

popstruc %<>% as_tibble() %>% dplyr::select(cod, year, municip, pq_sum, sum_fao, 
                                            sum_faoc95, sum_faocact) %>% 
  arrange(cod)


popstruc %<>% group_by(cod) %>% mutate(fao_diffc95 = sum_faoc95 - shift(sum_faoc95)) %>%
  mutate(fao_diffcact = sum_faocact - shift(sum_faocact))


popstruc_2000 <- filter(popstruc, year ==2000)
popstruc_2010 <- filter(popstruc, year ==2010)


# Merging with IBGE data
names(shp_ibge@data)[1] = "cod"
pq_aux_2000 <- merge(shp_ibge,popstruc_2000 , all.x = TRUE)
pq_aux_2010 <- merge(shp_ibge,popstruc_2010 , all.x = TRUE)


# Plotting Difference
map_diffc95  <- tm_shape(pq_aux_2010) +
  tm_polygons(col = "fao_diffc95",  style = "quantile", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Difference in Pre-Shares\nExposure 2000-2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, #capped?
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

map_diffc95

tmap_save(map_diffc95,
          "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures/map_diffc95.png")



map_diffcact  <- tm_shape(pq_aux_2010) +
  tm_polygons(col = "fao_diffcact",  style = "quantile", palette = "YlOrRd", border.col = "black", border.alpha = .3, showNA = TRUE, 
              textNA="No Data",
              title = "Difference in Actual Shares\nExposure 2000-2010") +
  tm_shape(shp_ufs) +
  tm_borders(lwd = 1.5, col = "black", alpha = .5) +
  tm_layout(legend.text.size=1.25,
            legend.title.size=1.55,
            legend.position = c("left","bottom"), 
            legend.height=1.0, 
            frame = FALSE) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 1) 

map_diffcact


tmap_save(map_diffcact,
          "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures/map_diffcact.png")
