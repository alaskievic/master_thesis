#Load packages
source("./00_load_packages.R")

######### 1. Using IPUMS Data to Construct Employment Shares for Brazil ########


### 1960 ###

# Reads ipums data
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_1960.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_1960.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

# Reads as an survey object (using individual weights)
brazil %<>% as_survey_design(weights = PERWT)

# Cleans ipums labels
brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))

# Restrict the dataset to employed individuals
brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)


# Displays harmonized occupational sectors
ipums_val_labels(brazil$variables$INDGEN)

# Cleans ipums labels
#brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))

 
# brazil %<>%
#   mutate(INDGEN = as_factor(ifelse(INDGEN == "Agriculture, fishing, and forestry",
#                                    1, 0)))

# Calculate employment shares for each sector
occ_1960 <- table(brazil$variables$INDGEN)
share_1960 <- prop.table(occ_1960) %>% as.data.frame() %>% as_tibble()




### 1970 ###
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_1970.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_1970.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))

brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1970 <- table(brazil$variables$INDGEN)
share_1970 <- prop.table(occ_1970) %>% as.data.frame() %>% as_tibble()


### 1980 ###
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_1980.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_1980.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1980 <- table(brazil$variables$INDGEN)
share_1980 <- prop.table(occ_1980) %>% as.data.frame() %>% as_tibble()



### 1991 ###
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_1991.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_1991.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1991 <- table(brazil$variables$INDGEN)
share_1991 <- prop.table(occ_1991) %>% as.data.frame() %>% as_tibble()



### 2000 ###
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_2000.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_2000.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_2000 <- table(brazil$variables$INDGEN)
share_2000 <- prop.table(occ_2000) %>% as.data.frame() %>% as_tibble()



### 2010 ###
brazil <- read_ipums_micro(ddi = here("data", "raw", "ipums", "CSV", "brazil_2010.xml"), 
                           data_file = here("data", "raw", "ipums", "CSV", "brazil_2010.csv"),
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))

occ_2010 <- table(brazil$variables$INDGEN)
share_2010 <- prop.table(occ_2010) %>% as.data.frame() %>%as_tibble()


### Joining ###
share_1960 %<>% mutate(Var1 = c("Agriculture, fishing, and forestry", 
                                  "Mining and extraction", 
                                  "Manufacturing",
                                  "Electricity, gas, water and waste management",
                                  "Construction", "Wholesale and retail trade", 
                                  "Hotels and restaurants", 
                                  "Transportation, storage, and communications", 
                                  "Financial services and insurance",
                                  "Public administration and defense",
                                  "Business services and real estate", 
                                  "Education", "Health and social work", 
                                  "Other services", "Private household services", 
                                  "Unknown"))

share_1970 %<>% slice(-1)

share_1960 %<>% slice_head(n=15) %>% rename(sh_1960 = "Freq")
share_1970 %<>% slice_head(n=15) %>% rename(sh_1970 = "Freq")
share_1980 %<>% slice_head(n=15) %>% rename(sh_1980 = "Freq")
share_1991 %<>% slice_head(n=15) %>% rename(sh_1991 = "Freq")
share_2000 %<>% slice_head(n=15) %>% rename(sh_2000 = "Freq")
share_2010 %<>% slice_head(n=15) %>% rename(sh_2010 = "Freq")


share_final <- full_join(share_2000, share_2010, by = "Var1")
share_final <- full_join(share_final, share_1991, by = "Var1")
share_final <- full_join(share_final, share_1980, by = "Var1")
share_final <- full_join(share_final, share_1970, by = "Var1")
share_final <- full_join(share_final, share_1960, by = "Var1")


share_final %<>% set_names(c("sector", "2000", "2010", "1991", "1980", "1970",
                             "1960"))

share_final %<>% pivot_longer(-"sector", names_to = "year", values_to = "shares")

share_final %<>% arrange(year)

save(share_final, 
     file = "C:/Users/Andrei/Desktop/share_final.RData")


################ 2. Making a Graph #############################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/share_final.RData")

# Summing up service sectors
share_final %<>% pivot_wider(names_from = "sector", values_from = "shares")

share_final %<>% rename(Agriculture = "Agriculture, fishing, and forestry", 
                        Industry = "Manufacturing") %>%
  mutate(year = as.integer(year))


share_final %<>% group_by(year) %>%
  mutate(Manufacturing = Industry + `Mining and extraction`) %>%
  mutate("Manufacturing + Construction" = Manufacturing + `Construction`) %>%
  mutate(Services = `Wholesale and retail trade` + `Hotels and restaurants` + 
           `Transportation, storage, and communications` + 
           `Financial services and insurance` + 
           `Public administration and defense` + 
           `Business services and real estate` + 
           `Education` + `Health and social work` + `Other services` + 
           `Private household services`) %>%
  mutate(Services2 = Services + `Electricity, gas, water and waste management`)

share_final %<>% pivot_longer(-"year", names_to = "sectors",
                              values_to = "shares")

share_final %<>% filter(sectors == "Agriculture"| sectors == "Manufacturing"|
                           sectors == "Manufacturing + Construction" |
                          sectors == "Services") %>%
  rename(Sector = sectors)

 
# devtools::install_github('Mikata-Project/ggthemr')
# library(ggthemr)
# ggthemr('fresh')




sh_graph_1 <- ggplot(share_final, aes(x=year)) +
  geom_line(data = share_final, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing",
                                "Manufact. + Construction", "Services"))+
  geom_point(data = share_final, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Employment Shares in Brazil (IPUMS Census Data)") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())


sh_graph_1

ggsave(filename = "empshares_alt.png", plot = sh_graph_1, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")


# No Title
share_final_2 <- share_final %>% filter(Sector != "Manufacturing + Construction") 

sh_graph <- ggplot(share_final_2, aes(x=year)) +
  geom_line(data = share_final_2, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = share_final_2, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=16), legend.position = c(0.85, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())

sh_graph

ggsave(filename = "empshares.png", plot = sh_graph, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")




################ 3. Historical Structural Change Graph #########################


### 1872 ###
ocup_1872 <- read_dta(file = here("data", "raw", "historical",
                                  "ocup_state_total_1872.dta"))

ocup_gender_1872 <- read_dta(file = here("data", "raw", "historical",
                                         "ocup_state_men_1872.dta"))

ocup_1872 %<>% filter (state_1872 == "ImperiodoBrazil") %>% 
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic,
         share_service = service_emp/total_ocup_nodomestic,
         share_other   = others_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1872, total_pop, total_ocup, total_ocup_nodomestic,
                starts_with("share"))


ocup_gender_1872 %<>% filter (state_1872 == "ImperiodoBrazil") %>% 
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic, 
         share_service = service_emp/total_ocup_nodomestic,
         share_other   = others_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1872, total_pop, total_ocup, total_ocup_nodomestic,
                starts_with("share"))



### 1900 ###
ocup_1900 <- read_excel(path = here("data", "raw", "historical", "ocup_1900.xlsx"))

ocup_gender_1900 <- ocup_1900 %>% filter(Sex == "Homens") %>%
  group_by("Nationality") %>% summarise(across(`Agricola`:`TOTAL`, sum)) %>%
  mutate(year = 1900, state_1900 = "Brazil")
  

ocup_1900 %<>% filter(Nationality == "Somma") %>%
  rename(total_pop = "TOTAL") %>% 
  mutate(total_ocup = sum(c_across("Agricola": "Mal especificadas"), na.rm = T)) %>%
  mutate(total_ocup_nodomestic = total_ocup - `Serviço Doméstico`) %>%
  mutate(share_agri = (`Agricola` + `Pastoril` + `Pesca e caça`)/total_ocup_nodomestic) %>%
  mutate(share_manufac = (`Minas` + `Borracha` + `Diversas` +
                          `Manufactureira`)/total_ocup_nodomestic) %>%
  mutate(share_other =   (`Religiosas` + `Mal especificadas`)/total_ocup_nodomestic) %>%
  mutate(share_service = (`Liberais` + `Artes e oficios` + 
                          `Administração` + `Força Pública` + `Comércio` + 
                          `Capitalistas` + `Transporte`)/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1900, total_pop, total_ocup, total_ocup_nodomestic, 
                starts_with("share"))


ocup_gender_1900 %<>% rename(total_pop = "TOTAL") %>% 
  mutate(total_ocup = sum(c_across("Agricola": "Mal especificadas"), na.rm = T)) %>%
  mutate(total_ocup_nodomestic = total_ocup - `Serviço Doméstico`) %>%
  mutate(share_agri = (`Agricola` + `Pastoril` + `Pesca e caça`)/total_ocup_nodomestic) %>%
  mutate(share_manufac = (`Minas` + `Borracha` + `Diversas` +
                            `Manufactureira`)/total_ocup_nodomestic) %>%
  mutate(share_other =   (`Religiosas` + `Mal especificadas`)/total_ocup_nodomestic) %>%
  mutate(share_service = (`Liberais` + `Artes e oficios` + 
                          `Administração` + `Força Pública` + `Comércio` + 
                          `Capitalistas` + `Transporte`)/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1900, total_pop, total_ocup, total_ocup_nodomestic, 
                starts_with("share"))


### 1920 ###
ocup_1920 <- read_dta(file = here("data", "raw", "historical",
                                  "ocup_state_total_1920.dta")) %>%
  group_by("codstate1920") %>% summarise(across(agri_emp:total_pop, sum)) %>%
  mutate(year = 1920, state_1920 = "Brazil")

ocup_gender_1920 <- read_dta(file = here("data", "raw", "historical",
                                         "ocup_state_men_1920.dta")) %>%
  group_by("codstate1920") %>% summarise(across(agri_emp:total_pop, sum)) %>%
  mutate(year = 1920, state_1920 = "Brazil")

ocup_1920 %<>%
  mutate(share_agri = agri_emp/total_ocup,
         share_manufac = manufac_emp/total_ocup,
         share_service = service_emp/total_ocup,
         share_other   = others_emp/total_ocup) %>%
  dplyr::select(year, state_1920, total_pop, total_ocup, starts_with("share"))

ocup_gender_1920 %<>%
  mutate(share_agri = agri_emp/total_ocup,
         share_manufac = manufac_emp/total_ocup,
         share_service = service_emp/total_ocup,
         share_other   = others_emp/total_ocup) %>%
  dplyr::select(year, state_1920, total_pop, total_ocup, starts_with("share"))


### 1940 ###
ocup_1940 <- read_dta(file = here("data", "raw", "historical",
                                  "ocup_state_total_1940.dta"))%>%
  group_by("codstate1940") %>% summarise(across(agri_emp:service_emp, sum)) %>%
  mutate(year = 1940, state_1940 = "Brazil")

ocup_gender_1940 <- read_dta(file = here("data", "raw", "historical",
                                         "ocup_state_men_1940.dta")) %>%
  group_by("codstate1940") %>% summarise(across(agri_emp:service_emp, sum)) %>%
  mutate(year = 1940, state_1940 = "Brazil")


ocup_1940 %<>%
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic,
         share_service = service_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1940, total_ocup_nodomestic,
                starts_with("share"))

ocup_gender_1940 %<>%
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic,
         share_service = service_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1940, total_ocup_nodomestic,
                starts_with("share"))


## 1950 ##
ocup_1950 <- read_dta(file = here("data", "raw", "historical",
                                  "ocup_state_total_1950.dta")) %>%
  group_by("codstate1950") %>% summarise(across(total_pop_10_plus:service_emp, sum)) %>%
  mutate(year = 1950, state_1950 = "Brazil")

ocup_gender_1950 <- read_dta(file = here("data", "raw", "historical",
                                         "ocup_state_men_1950.dta")) %>%
  group_by("codstate1950") %>% summarise(across(total_men_10_plus:service_emp, sum)) %>%
  mutate(year = 1950, state_1950 = "Brazil")


ocup_1950 %<>%
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic,
         share_service = service_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1950, total_ocup_nodomestic,
                starts_with("share"))

ocup_gender_1950 %<>%
  mutate(share_agri = agri_emp/total_ocup_nodomestic,
         share_manufac = manufac_emp/total_ocup_nodomestic,
         share_service = service_emp/total_ocup_nodomestic) %>%
  dplyr::select(year, state_1950, total_ocup_nodomestic,
                starts_with("share"))


### Joining All ###
ocup_hist <- bind_rows(ocup_1872, ocup_1900, ocup_1920, ocup_1940, ocup_1950) %>%
  dplyr::select(year, share_agri, share_manufac, share_service)

ocup_hist %<>% pivot_longer(!year, values_to = "shares", names_to = "Sector")

ocup_hist_gender <- bind_rows(ocup_gender_1872, ocup_gender_1900, 
                              ocup_gender_1920, ocup_gender_1940, 
                              ocup_gender_1950)  %>%
  dplyr::select(year, share_agri, share_manufac, share_service)

ocup_hist_gender %<>% pivot_longer(!year, values_to = "shares", names_to = "Sector")


sh_historical <- ggplot(ocup_hist, aes(x=year)) +
  geom_line(data = ocup_hist, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = ocup_hist, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Historical Employment Shares in Brazil") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())

sh_historical


sh_historical_men <- ggplot(ocup_hist_gender, aes(x=year)) +
  geom_line(data = ocup_hist_gender, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = ocup_hist_gender, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Historical Employment Shares in Brazil (Male Employment)") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.80, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())

sh_historical_men



### Appending to Modern Census Data ###
load(here("data", "output", "final", "share_final.RData"))


share_final %<>% pivot_wider(names_from = "sector", values_from = "shares")

share_final %<>% rename(share_agri = "Agriculture, fishing, and forestry", 
                        Industry = "Manufacturing") %>%
  mutate(year = as.integer(year))


share_final %<>% group_by(year) %>%
  mutate(share_manufac = Industry + `Mining and extraction`) %>%
  mutate(share_service = `Wholesale and retail trade` + `Hotels and restaurants` + 
           `Transportation, storage, and communications` + 
           `Financial services and insurance` + 
           `Public administration and defense` + 
           `Business services and real estate` + 
           `Education` + `Health and social work` + `Other services` + 
           `Private household services`) %>%
  mutate(Services2 = share_service + `Electricity, gas, water and waste management`)

share_final %<>% pivot_longer(-"year", names_to = "sectors",
                              values_to = "shares")

share_final %<>% filter(sectors == "share_agri"| sectors == "share_manufac"|
                          sectors == "share_service") %>%
  rename(Sector = sectors)


ocup_final <- bind_rows(ocup_hist, share_final)


sh_final <- ggplot(ocup_final, aes(x=year)) +
  geom_line(data = ocup_final, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = ocup_final, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Historical Employment Shares in Brazil") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.20, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank()) + 
  scale_x_continuous(breaks = c(1872, 1900, 1920, 1940, 1950, 1960, 1970, 1980, 
                                1991, 2000, 2010)) + 
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8))

sh_final



ocup_final_gender <- bind_rows(ocup_hist_gender, share_final)


sh_final_gender <- ggplot(ocup_final_gender, aes(x=year)) +
  geom_line(data = ocup_final_gender, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = ocup_final_gender, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Historical Employment Shares in Brazil") +
  theme_bw(base_size = 16) +
  theme(legend.text=element_text(size=14), legend.position = c(0.20, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank()) + 
  scale_x_continuous(breaks = c(1872, 1900, 1920, 1940, 1950, 1960, 1970, 1980, 
                                1991, 2000, 2010)) + 
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8))

sh_final_gender
