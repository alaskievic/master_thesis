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


######### 1. Exports data to Stata for summary statistics and regressions making ###############################################################################
## Reading Bartik data

pq_bartik <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/com_municip.dta")


# Ranks the residuals in quintiles

pq_bartik <- as_tibble(pq_bartik)
pq_bartik <- pq_bartik %>%  mutate(resid_rank = cut(100 * percent_rank(resid), seq(0, 100, len = 11),
                                          include.lowest = TRUE)) %>%
  mutate(resid_rank_2 = cut(100 * percent_rank(resid), seq(0, 100, len = 5),
                            include.lowest = TRUE))
# Generate dummies
pq_bartik <- pq_bartik %>% mutate(dummy_10 = ifelse(resid_rank == "(90,100]", 1, 0)) %>%
  mutate(dummy_25 = ifelse(resid_rank_2 == "(75,100]", 1, 0))


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


#Reads Municipalities GDP data

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
deflate <- function(x) x/(ipca$Index_2/100)

municip_pib_real <- mutate_all(municip_pib[4], deflate) %>%
  add_column(cod = municip_pib$cod, municip = municip_pib$municip, year = municip_pib$year, .before = "municip_pib")

municip_pib_real <- municip_pib_real %>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))


                         
# For first regresions
exp_bartik <- inner_join(pq_bartik, dplyr::select(controls, -"municip"), by = "cod")
exp_bartik <- inner_join(exp_bartik, dplyr::select(exp_rev_pc, -"municip"), by = c("cod", "year"))
exp_bartik <- inner_join(exp_bartik, dplyr::select(municip_pib_real, -"municip"), by = c("cod", "year"))

# Save

write.dta(exp_bartik, "exp_bartik.dta")


# Municipalities not in PAM
a <- anti_join(pop_sidra, exp_bartik, by = c("cod", "year"))

######### 1.2 Gini Indexes ##################################################################################################################################

income_gini <- atlas_mun %>% dplyr::select(c("ANO", "Codmun7", "Município", "GINI"))
  rename(cod = "Codmun7", year = "ANO")


tmp <- full_join(gini_land_1995, gini_land_2006, by = "cod")
land_gini <- full_join(tmp, gini_land_2017, by = "cod")


write.dta(income_gini, "income_gini.dta")
write.dta(land_gini, "land_gini.dta")



######### 1.3 Demographic Census ############################################################################################################################


#write.dta(atlas_mun, "atlas_mun.dta")





