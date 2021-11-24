#Load packages
source("./00_load_packages.R")

######### 1. Consolidating Explanatory Datasets ################################

### International Prices in 2010 values
cpi_prices_2010 <- load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/prices_real_bartik_2010.Rdata")

save(cpi_prices_2010,
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/prices.Rdata")

### Commodity Boom Exposure Measures
## Merging all measures
# FAO with cattle
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/final_measures.Rdata")

# No Cattle - Shares
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/pq_shares_log.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/fao_final.Rdata")

# No Cattle - Quantities
fao_final %<>% mutate(cod = as.integer(cod))
pq_final_shares_log %<>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

final_measures <- full_join(final_measures, fao_final, by = c("cod", "year"))
final_measures <- full_join(final_measures, pq_final_shares_log, by = c("cod", "year"))

final_measures %<>% rename(municip = municip.x) %>%
  dplyr::select(-ends_with(".x")) %>% dplyr::select(-ends_with(".y"))


save(final_measures, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/final_measures.Rdata")


### All shares: both from PAM and FAO_GAEZ predicted(pr_) using pre-period averages
# (1990-1995) and actual averages (2000-2010) WITH cattle
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/akm_shares.Rdata")


save(akm_shares, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/shares.Rdata")


### Baseline Controls
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/controls_baseline.Rdata")

# Taking out shares
controls_baseline %<>% dplyr::select(-starts_with("pr_")) %>%
  dplyr::select(-c("pq_orange", "municip.x", "municip.y", "banana",
                   "barley", "cocoa", "coffee", "maize", "orange", "rice", 
                   "sorghum", "soybean", "sugar_cane", "tobacco", "wheat", 
                   "cotton", "total_quant", "tea", "municip", "uf", "SIGLA_UF"))

# Adding productivity controls
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/agri_controls.RData")

prod %<>% filter(year == 1995) %>% rename(totval_1995 = totval,
                                          n_workers_1995 = n_workers, 
                                          total_area_1995 = total_area, 
                                          linten_1995 = linten, 
                                          val_outpw_1995 = val_outpw, 
                                          val_outpa_1995 = val_outpa)


# Adding FAO-GAEZ principal components
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/first_pca.RData")



# Adding region codes from IBGE
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/mun_codes.RData")



## Merging
controls <- full_join(controls_baseline, mun_codes, by = "cod")
controls <- full_join(controls, first_pca, by = "cod")
controls <- full_join(controls, prod, by = "cod")

controls %<>% dplyr::select(-c("municip.x", "municip.y", "year"))


save(controls, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/controls.Rdata")
  
### Land Gini
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts/land_gini.RData")

save(land_gini, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/land_gini.Rdata")


######### 2. Agricultural Structural Change Dataset ############################
load(here("data", "output", "final",  "agro_struc.Rdata"))
load(here("data", "output", "final",  "final_measures.Rdata"))
load(here("data", "output", "final",  "shares.Rdata"))
load(here("data", "output", "final",  "controls.Rdata"))
load(here("data", "output", "final",  "land_gini.Rdata"))
load(here("data", "output", "final",  "land_app.Rdata"))
load(here("data", "output", "final",  "groupa_prop.Rdata"))
load(here("data", "output", "final",  "final_measures_faohigh.Rdata"))
load(here("data", "output", "final",  "amc_full.Rdata"))
load(here("data", "output", "final",  "openness.Rdata"))
load(here("data", "output", "final",  "residence_agro.Rdata"))
load(here("data", "output", "final",  "measure_placebo_1985.RData"))

pop_struc <- read_dta(file = here("analysis", "code", "stata", "pop_struc_r.dta"))



fao_final_placebo_1985 %<>% filter(year == 1990| year == 2000) %>%
  mutate(year = ifelse(year == 1990, 2006, 2017))


final_measures %<>% filter(year == 2000| year == 2010 | year == 2015)

final_measures %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2010, 2006, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))

fao_final_cattle_1995 %<>% filter(year == 2000| year == 2010 | year == 2015)

fao_final_cattle_1995 %<>% mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2010, 2006, year)) %>%
  mutate(year = ifelse(year == 2015, 2017, year))


openness %<>% filter(year == 2000| year == 2010) %>%
  mutate(year = ifelse(year == 2000, 1995, year)) %>%
  mutate(year = ifelse(year == 2010, 2006, year)) %>%
  dplyr::select(-"municip")


pop_struc %<>% mutate (year = ifelse(year == 2000, 2006, 2017)) %>%
  dplyr::select(cod, year, P_AGRO, P_SERV, P_INDUST, w_serv, w_agro, 
                w_indust, pesotot, pesourb)

agro_struc <- full_join(agro_struc, dplyr::select(final_measures, -"municip"), 
                        by = c("cod", "year")) %>%
  full_join(., akm_shares, by = "cod") %>%
  full_join(., dplyr::select(controls, -"municip"), by = "cod") %>%
  full_join(., dplyr::select(fao_final_cattle_1995, -"municip"), by = c("cod", "year")) %>%
  full_join(., dplyr::select(land_gini, -"municip"), by = c("cod", "year")) %>%
  full_join(., dplyr::select(land_app, -"total_area"), by = c("cod", "year")) %>%
  full_join(., groupa_prop, by = c("cod", "year")) %>%
  full_join(., amc_full, by = c("cod", "year")) %>%
  full_join(., pop_struc, by = c("cod", "year")) %>%
  full_join(., openness, by = c("cod", "year")) %>%
  full_join(., residence_agro, by = c("cod", "year")) %>%
  full_join(., fao_final_placebo_1985, by = c("cod", "year"))

agro_struc %<>% dplyr::select(-c("municip.x", "municip.y"))

# Saving
write_dta(agro_struc, path = here("analysis", "code", "stata", "agro_struc.dta"))


################# 3. Population Structural Change Dataset ######################
load(here("pop_struc.RData"))
load(here("data", "output", "final", "final_measures.RData"))
load(here("data", "output", "final", "shares.Rdata"))
load(here("data", "output", "final", "controls.Rdata"))
load(here("data", "output", "final", "ocup_sidra.RData"))
load(here("data", "output", "final", "pop_tot.RData"))
load(here("data", "output", "final", "final_measures_faohigh.Rdata"))
load(here("data", "output", "final", "amc_full.Rdata"))
load(here("data", "output", "final", "past_share.Rdata"))
load(here("data", "output", "final", "openness.Rdata"))
load(here("data", "output", "final", "measure_placebo_1985.RData"))
load(here("data", "output", "final","residence_agro.Rdata"))




residence_agro %<>% mutate(year = ifelse(year == 2006, 2000, 2010))


final_measures %<>% filter(year <= 2010) %>% group_by(cod) %>%
  mutate(sum_faoc95_avg = mean(sum_fao_cattle_1995)) %>%
  mutate(sum_faocact_avg = mean(sum_fao_cattle_actual)) %>%
  filter(year == 2000| year == 2010)


pop_sidra %<>% filter(year == 2000| year == 2010)

fao_final_cattle_1995 %<>% filter(year == 2000| year == 2010)

openness %<>% filter(year == 2000| year == 2010) %>% dplyr::select(-"municip")

fao_final_placebo_1985 %<>% filter(year == 1990| year == 2000) %>%
  mutate(year = ifelse(year == 1990, 2000, 2010))


# Joining
pop_struc <- full_join(popstruc_pres, dplyr::select(final_measures, -"municip"), 
                        by = c("cod", "year")) %>%
  full_join(., dplyr::select(fao_final_cattle_1995, -"municip"), 
            by = c("cod", "year")) %>%
  full_join(., akm_shares, 
            by = c("cod")) %>%
  full_join(., dplyr::select(controls, -"municip"), 
            by = c("cod")) %>%
  full_join(., ocup_sidra, 
            by = c("cod", "year")) %>%
  full_join(., dplyr::select(pop_sidra, -"municip"), 
            by = c("cod", "year")) %>%
  full_join(., amc_full,   by   = c("cod", "year")) %>%
  full_join(., past_share, by   = c("cod", "year")) %>%
  full_join(., openness,   by   = c("cod", "year")) %>%
  full_join(., fao_final_placebo_1985,   by   = c("cod", "year")) %>%
  full_join(., residence_agro,   by   = c("cod", "year"))


pop_struc %<>% dplyr::select(-c("municip.y.x", "municip.y.y", 
                                "municip.x.y")) %>%
  rename(municip = municip.x.x)

# Saving
write_dta(pop_struc, path = here("analysis", "code", "stata", "pop_struc.dta"))


######### 4. Municipal GDPs ####################################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/municip_pib_real.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/final_measures.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/shares.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/controls.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/pop_tot.Rdata")

municip_pib_real <- full_join(municip_pib_real, dplyr::select(final_measures, -"municip"), 
                       by = c("cod", "year"))

municip_pib_real <- full_join(municip_pib_real, akm_shares, 
                       by = c("cod"))

municip_pib_real <- full_join(municip_pib_real, dplyr::select(controls, -"municip"), 
                       by = c("cod"))

municip_pib_real <- full_join(municip_pib_real, dplyr::select(pop_sidra, -"municip"), 
                              by = c("cod", "year"))


municip_pib_real %<>% dplyr::select(-c("municip.x", "municip.y"))



# Saving
write_dta(municip_pib_real,
          path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/municip_gdp.dta")


################################### 5. RAIS ####################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/rais_shares_2.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/final_measures.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/shares.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/controls.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/pop_tot.Rdata")

rais_shares_2 %<>% filter(year >= 2000 & year <= 2015)

rais <- full_join(rais_shares_2, dplyr::select(final_measures, -"municip"), 
                              by = c("cod", "year"))

rais <- full_join(rais, akm_shares, 
                              by = c("cod"))

rais <- full_join(rais, dplyr::select(controls, -"municip"), 
                              by = c("cod"))

rais <- full_join(rais, dplyr::select(pop_sidra, -"municip"), 
                              by = c("cod", "year"))


rais %<>% dplyr::select(-c("municip.x", "municip.y"))

# Saving
write_dta(rais,
          path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/rais.dta")
