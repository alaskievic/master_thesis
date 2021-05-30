# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 0. Loading Files ##################################################################################################################################
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/exp_real_long.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/rev_final.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/atlas_mun.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/land_gini.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2002.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2009.RData")



######### 1. Exports data to Stata for summary statistics and regressions making ###############################################################################
#Consolidating Bartik measures data
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_bartik.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/pq_shares_bartik.Rdata")

pq_final_longer <- pq_final_longer %>% rename(pq_quant = "pq")
pq_final_shares_longer <- pq_final_shares_longer %>% rename(pq_shares = "pq")


pq_bartik_final <- full_join(pq_final_longer, dplyr::select(pq_final_shares_longer, -"municip"), by = c("cod", "year")) %>%
  mutate(cod = as.integer(cod)) %>% mutate(year = as.integer(year))

#Saving
write_xlsx(pq_bartik_final, 'C:/Users/Andrei/Desktop/Dissertation/Dados/pq_bartik_final.xlsx')

save(pq_bartik_final, file = 'C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pq_bartik_final.Rdata')

######## Reading Bartik data

pq_bartik <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/bartik_residuals.dta")


# Ranks the residuals in quintiles
pq_bartik <- as_tibble(pq_bartik)
pq_bartik <- pq_bartik %>%  mutate(resid_quant_rank_1 = cut(100 * percent_rank(pq_quant_resid), seq(0, 100, len = 11),
                                          include.lowest = TRUE)) %>%
  mutate(resid_quant_rank_2 = cut(100 * percent_rank(pq_quant_resid), seq(0, 100, len = 5),
                            include.lowest = TRUE))

pq_bartik <- pq_bartik %>%  mutate(resid_shares_rank_1 = cut(100 * percent_rank(pq_shares_resid), seq(0, 100, len = 11),
                                                          include.lowest = TRUE)) %>%
  mutate(resid_shares_rank_2 = cut(100 * percent_rank(pq_shares_resid), seq(0, 100, len = 5),
                                  include.lowest = TRUE))

pq_bartik <- pq_bartik %>%  mutate(resid_log_quant_rank_1 = cut(100 * percent_rank(log_pq_quant_resid), seq(0, 100, len = 11),
                                                          include.lowest = TRUE)) %>%
  mutate(resid_log_quant_rank_2 = cut(100 * percent_rank(log_pq_quant_resid), seq(0, 100, len = 5),
                                  include.lowest = TRUE))

pq_bartik <- pq_bartik %>%  mutate(resid_log_shares_rank_1 = cut(100 * percent_rank(log_pq_shares_resid), seq(0, 100, len = 11),
                                                          include.lowest = TRUE)) %>%
  mutate(resid_log_shares_rank_2 = cut(100 * percent_rank(log_pq_shares_resid), seq(0, 100, len = 5),
                                  include.lowest = TRUE))

# Generate dummies
pq_bartik <- pq_bartik %>% mutate(quant_dummy_10 = ifelse(resid_quant_rank_1 == "(90,100]", 1, 0)) %>%
  mutate(quant_dummy_25 = ifelse(resid_quant_rank_2 == "(75,100]", 1, 0))

pq_bartik <- pq_bartik %>% mutate(shares_dummy_10 = ifelse(resid_shares_rank_1 == "(90,100]", 1, 0)) %>%
  mutate(shares_dummy_25 = ifelse(resid_shares_rank_2 == "(75,100]", 1, 0))

pq_bartik <- pq_bartik %>% mutate(log_quant_dummy_10 = ifelse(resid_log_quant_rank_1 == "(90,100]", 1, 0)) %>%
  mutate(log_quant_dummy_25 = ifelse(resid_log_quant_rank_2 == "(75,100]", 1, 0))

pq_bartik <- pq_bartik %>% mutate(log_shares_dummy_10 = ifelse(resid_log_shares_rank_1 == "(90,100]", 1, 0)) %>%
  mutate(log_shares_dummy_25 = ifelse(resid_log_shares_rank_2 == "(75,100]", 1, 0))

# Filter years
pq_bartik <- pq_bartik %>% filter(year >= 2000, year <= 2010)


######### 1.1 Revenues and Expenditures data #############
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")


# Per capita for descriptive statistics
pop <- atlas_mun %>% dplyr::select(c("ANO", "Codmun7", "Município", "pesotot")) %>%
  filter(ANO != 1991) %>%
  rename(cod = "Codmun7", year = "ANO")

rev_pc <- filter(rev_final, year == 2000 | year==2010) %>%
  mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

exp_pc <- filter(exp_real_long, year==2000 | year == 2010) %>%
  mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

rev_pc <- full_join(pop, rev_pc, by = c("cod", "year"))

exp_rev <- full_join(rev_pc, exp_pc, by = c("cod", "year"))

exp_rev <- exp_rev %>% dplyr::select(-c("municip.y", "Município", "municip"))

exp_rev <- exp_rev %>% rename(municip = "municip.x")

pc <- function(x) x/(exp_rev$pesotot)

teste1 <- mutate_all(exp_rev[7:28], pc) %>%
  add_column(cod = exp_rev$cod, year = exp_rev$year, .before = "exp_itr") %>%
  replace(is.na(.), 0) %>%
  as_tibble()


exp_rev_controls_pc <- full_join(controls, teste1, by = "cod")

summ <- inner_join(pop, exp_rev_controls_pc, by = c("cod", "year"))

#Saving in Stata
write.dta(summ, "summ.dta")


# Reading population
pop <- atlas_mun %>% dplyr::select(c("ANO", "Codmun7", "Município", "pesotot")) %>%
  filter(ANO != 1991) %>%
  rename(cod = "Codmun7", year = "ANO")

pop_sidra <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/pop_sidra.xlsx",
                        skip = 3, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

pop_2007 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/populacao_total.xls",
                        col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X")) %>%
  rename(cod = "Codigo", municip = "Município")  %>%
  dplyr::select(c("cod", "municip", "2007")) %>%
  mutate(cod = as.integer(cod))


pop_sidra <- pop_sidra %>% rename(cod = "Cód.", municip = "Município") %>%
  slice(-5571) %>%
  mutate(cod = as.integer(cod))

pop_sidra <- inner_join(pop_sidra, dplyr::select(pop_2007, c("cod", "2007")), by = "cod")

pop <- pivot_wider(pop, values_from = "pesotot", names_from = "year")

pop_sidra <- inner_join(pop_sidra, dplyr::select(pop, c("cod", "2000", "2010")), by = "cod")

pop_sidra <- pivot_longer(pop_sidra, -c("cod", "municip"), values_to = "pesotot", names_to = "year") %>%
  arrange(year) %>%
  arrange(cod) %>%
  mutate(year = as.integer(year))

# Saving
save(pop_sidra, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pop_sidra.Rdata")

pop_sidra <- pop_sidra %>% filter(year >=2000, year <=2010)


rev_pc <- rev_final %>%
  mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

exp_pc <- exp_real_long %>%
  mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

rev_pc <- inner_join(pop_sidra, rev_pc, by = c("cod", "year")) %>%
  dplyr::select(-c("municip.x", "municip2", "UF", "municip.y"))

exp_rev <- inner_join(rev_pc, dplyr::select(exp_pc, -"municip"), by = c("cod", "year")) %>%
  rename(rev_itr = "exp_itr", transf_estad = "trans_estad")


pc <- function(x) x/(exp_rev$pesotot)

exp_rev_pc <- mutate_all(exp_rev[5:26], pc) %>%
  add_column(cod = exp_rev$cod, year = exp_rev$year, municip = exp_rev$municip, pop = exp_rev$pesotot, .before = "rev_itr") %>%
  as_tibble()


# Reads Municipalities GDP data
municip_pib_2002 <-read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/municip_pib_ibge_antigo.xlsx", 
                         skip = 3, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2010 <-read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/municip_pib_ibge.xlsx", 
                         skip = 3, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2002  <- municip_pib_2002 %>% rename(cod = "Cód.", municip = "Município") %>%
  dplyr::select(c("cod", "municip", "2000", "2001"))

municip_pib_2010 <- municip_pib_2010 %>% rename(cod = "Cód.", municip = "Município")


municip_pib <- full_join (municip_pib_2010, municip_pib_2002, by = "cod")

municip_pib <- municip_pib %>%
  dplyr::select(-c("municip.y")) %>%
  rename(municip = "municip.x") %>%
  dplyr::select(c("cod", "municip", "2000", "2001"), everything()) %>%
  dplyr::select(c("cod", "municip"), `2000`:`2010`)

municip_pib <- pivot_longer(municip_pib, -c("cod", "municip"), names_to = "year", values_to = "municip_pib")



#Deflate
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 2000 &  Date <= 2010)

deflate <- function(x) x/(ipca$Index_2/100)

municip_pib_real <- mutate_all(municip_pib[4], deflate) %>%
  add_column(cod = municip_pib$cod, municip = municip_pib$municip, year = municip_pib$year, .before = "municip_pib")

municip_pib_real <- municip_pib_real %>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))


                         
# For first regresions
exp_bartik <- inner_join(pq_bartik, dplyr::select(controls, -"municip"), by = "cod")
exp_bartik <- inner_join(exp_bartik, dplyr::select(exp_rev_pc, -"municip"), by = c("cod", "year"))
exp_bartik <- inner_join(exp_bartik, dplyr::select(municip_pib_real, -"municip"), by = c("cod", "year"))

# Saving to Stata
write.dta(exp_bartik, "exp_bartik.dta")


# Municipalities not in PAM
a <- anti_join(pop_sidra, exp_bartik, by = c("cod", "year"))

######### 1.2 Gini Indexes ##################################################################################################################################

income_gini <- atlas_mun %>% dplyr::select(c("ANO", "Codmun7", "Município", "GINI")) %>%
  rename(cod = "Codmun7", year = "ANO")



######### 1.2 Demographic Census and Land Gini #######################################################################################################################
######### Joining dataset with demographic census and land gini between 2000 and 2010

## setting years
land_gini_1 <- land_gini %>% filter(year != 2017) %>% mutate(year = ifelse(year == 1995, 2000, 2010))
exp_bartik_tmp <- exp_bartik %>% filter(year == 2000 | year ==2010)
atlas_mun <- atlas_mun %>% filter(ANO != 1991) %>%
  rename(cod = "Codmun7", year = "ANO", municip = "Município")

# Joining
tmp_1 <- full_join(dplyr::select(land_gini_1, -"municip"), atlas_mun, by = c("cod", "year"))
andrei_data_1 <- full_join(dplyr::select(tmp_1, -"municip"), exp_bartik_tmp, by = c("cod", "year"))

write.dta(andrei_data_1, "andrei_data_1.dta")

### Changing the assignment for the land gini years

land_gini_2 <- land_gini %>% filter(year != 1995) %>% mutate(year = ifelse(year == 2006, 2000, 2010))

tmp_2 <- full_join(dplyr::select(land_gini_2, -"municip"), atlas_mun, by = c("cod", "year"))
andrei_data_2 <- full_join(dplyr::select(tmp_2, -"municip"), exp_bartik_tmp, by = c("cod", "year"))


write.dta(andrei_data_2, "andrei_data_2.dta")

######### 2. AMS Data #########################################################################################################################
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2002.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2009.RData")



#Tidying up
ams_2002 <- ams_2002 %>% mutate(year = rep(2000,length(cod))) %>%
  relocate(year, .before = "estab_total")

#Check
ams_2009_check <- ams_2009 %>% mutate(year = rep(2010,length(cod))) %>%
  relocate(c("cod", "municip2","year", "Nome_UF", "UF"), .before = "municip") %>%
  rename(municip = "municip2", municip2 = "municip", atend_total_sus = "atend_total_su") %>%
  dplyr::select(-c("UF.x", "UF.y")) %>%
  arrange(cod)

ams_2009 <- ams_2009 %>% mutate(year = rep(2010,length(cod))) %>%
  relocate(c("cod", "municip2","year", "Nome_UF", "UF"), .before = "municip") %>%
  rename(municip = "municip2", municip2 = "municip", atend_total_sus = "atend_total_su") %>%
  dplyr::select(-c("UF.x", "UF.y", "Nome_UF", "UF", "municip2")) %>%
  transform(cod = as.integer(cod)) %>%
  arrange(cod)

#Merging
ams_2002_join <- dplyr::select(ams_2002, colnames(ams_2009))
  
colnames <- colnames(ams_2009)

ams <- full_join(ams_2002_join, ams_2009, by = c(colnames))

#Full joining with andrei_1

andrei_tmp <- full_join(ams, dplyr::select(andrei_data_1, -"municip"), by = c("cod", "year")) %>%
  arrange(cod)

#Saving
write.dta(andrei_tmp, "andrei_tmp.dta")
  





