# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

######### 1. Reads and cleans Municipalities GDP by sector  ############################################################

#GDPs
municip_pib_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_1.xlsx", 
                              skip = 3, sheet = "Produto Interno Bruto a preç...", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2002 <- municip_pib_2002 %>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip", "1999", "2000", "2001")) %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)


municip_pib_agro_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_1.xlsx", 
                                 skip = 3, sheet = "Valor adicionado bruto a pre...", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_agro_2002 <- municip_pib_agro_2002 %>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip",  "1999", "2000", "2001")) %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)



municip_pib_indust_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_2.xlsx", 
                                   skip = 3, sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_indust_2002 <- municip_pib_indust_2002 %>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip", "1999", "2000", "2001")) %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)



municip_pib_serv_2002  <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_2.xlsx", 
                                  skip = 3, sheet = "Tabela 2", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_serv_2002 <- municip_pib_serv_2002 %>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip",  "1999", "2000", "2001")) %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)




#2010
municip_pib_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_novo_1.xlsx", 
                              skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2010 <- municip_pib_2010 %>% rename(cod = "...1", municip = "...2") %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)




municip_pib_agro_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_novo_1.xlsx", 
                                 skip = 3, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


municip_pib_agro_2010 <- municip_pib_agro_2010 %>% rename(cod = "...1", municip = "...2") %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)



municip_pib_indust_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_novo_2.xlsx", 
                                   skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_indust_2010 <- municip_pib_indust_2010 %>% rename(cod = "...1", municip = "...2") %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)




municip_pib_serv1_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_novo_2.xlsx", 
                                     skip = 3, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


municip_pib_serv1_2010 <- municip_pib_serv1_2010 %>% rename(cod = "...1", municip = "...2") %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_serv") %>%
  arrange(cod)




municip_pib_serv2_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/PIB/Setores/pib_mun_novo_3.xlsx", 
           skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


municip_pib_serv2_2010 <- municip_pib_serv2_2010 %>% rename(cod = "...1", municip = "...2") %>%
  filter(cod > 1) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_serv") %>%
  arrange(cod)


municip_pib_serv_2010 <- municip_pib_serv1_2010 %>% mutate(pib_serv = municip_pib_serv1_2010$pib_serv + 
                                                             municip_pib_serv2_2010$pib_serv)

municip_pib_serv_2010_wide <- pivot_wider(municip_pib_serv_2010, names_from = "year", 
                                     values_from = "pib_serv")


#Merging
municip_pib_tot <- full_join(dplyr::select(municip_pib_2002, -"municip"), municip_pib_2010, by = "cod")

municip_pib_agro <- full_join(dplyr::select(municip_pib_agro_2002, -"municip"), municip_pib_agro_2010, by = "cod")

municip_pib_indust <- full_join(dplyr::select(municip_pib_indust_2002, -"municip"), municip_pib_indust_2010, by = "cod")

municip_pib_serv <- full_join(dplyr::select(municip_pib_serv_2002, -"municip"), municip_pib_serv_2010_wide, by = "cod")


#Pivoting

municip_pib_tot <- municip_pib_tot %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_tot") %>%
  arrange(cod)

municip_pib_agro <- municip_pib_agro %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_agro") %>%
  arrange(cod)

municip_pib_indust <- municip_pib_indust %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_indust") %>%
  arrange(cod)

municip_pib_serv <- municip_pib_serv %>% pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_serv") %>%
  arrange(cod)


#Another Merge
municip_pib <- full_join(municip_pib_tot, municip_pib_agro, by = c("cod", "year", "municip"))

municip_pib <- full_join(municip_pib, municip_pib_indust, by = c("cod", "year", "municip"))

municip_pib <- full_join(municip_pib, municip_pib_serv, by = c("cod", "year", "municip")) %>%
  arrange(cod)


##Deflating
#Reads IPCA for deflation
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "")

ipca <- ipca %>% filter(Date >= 1999 & Date <= 2017)

deflate <- function(x) x/(ipca$Index_2/100)

municip_pib_real <- mutate_all(municip_pib[4:7], deflate) %>%
  add_column(cod = municip_pib$cod, municip = municip_pib$municip, year = municip_pib$year, .before = "pib_tot")

municip_pib_real <- municip_pib_real %>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))

#Adding population
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pop_sidra.Rdata")

municip_pib_real <- municip_pib_real %>% filter(year >= 2000 & year <= 2015)

municip_pib_final <- inner_join(municip_pib_real, dplyr::select(pop_sidra, -"municip"), by = c("cod", "year"))


#Adding Bartik
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pq_bartik_final.Rdata")

municip_pib_final <- inner_join(municip_pib_final, dplyr::select(pq_bartik_final, -"municip"), by = c("cod", "year"))

#Adding Controls
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")

municip_pib_final <- inner_join(municip_pib_final, dplyr::select(controls, -"municip"), by = "cod")


#Write to Stata
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")
write.dta(municip_pib_final, "municip_pib_final.dta")



