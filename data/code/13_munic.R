#Load packaages
source("./00_load_packages.R")

######### 1. Reads and cleans the Pesquisa de Informações Básicas Municipais - 
#########    MUNIC by IBGE 

load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pq_bartik_final.Rdata")
pq_bartik <- 
  read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/bartik_residuals.dta")

#Changing Cods and Widening
controls <- controls %>% mutate(cod = substr(controls$cod,1, 
                                             nchar(controls$cod)-1)) %>%
  mutate(cod = as.integer(cod))

pq_bartik <- pq_bartik %>% dplyr::select(c("cod", "municip", "year",
                                           "log_pq_shares")) %>%
  as_tibble()

pq_bartik_munic <- pivot_wider(pq_bartik, values_from = "log_pq_shares",
                               names_from = "year")

pq_bartik_munic <- pq_bartik_munic %>% 
  mutate(cod = substr(pq_bartik_munic$cod,1,nchar(pq_bartik_munic$cod)-1)) %>%
  mutate(cod = as.integer(cod))

# 2004
path_2004 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2004.xls")

munic_2004_tmp <- path_2004 %>%
  excel_sheets() %>%
  set_names(., nm = .) %>%
  map(read_excel, path = path_2004, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                        "..", "X", "Não aplicável"))

munic_2004_tmp <- munic_2004_tmp[-1]

munic_2004 <- reduce(munic_2004_tmp, full_join, by = "A1") %>%
  relocate("A164", .after = "A1")

munic_2004 <- munic_2004 %>% na_if("Não aplicável")
munic_2004 <- munic_2004 %>% na_if("Ignorado")
munic_2004 <- munic_2004 %>% na_if("Recusa")
munic_2004 <- munic_2004 %>% na_if("Não informado")
munic_2004 <- munic_2004 %>% na_if("Não disponível")

munic_2004 <- munic_2004 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
munic_2004 <- munic_2004 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))

munic_2004 <- munic_2004 %>% mutate_all(function(x) ifelse(x == "Separadamente", 1, x))
munic_2004 <- munic_2004 %>% mutate_all(function(x) ifelse(x == "Em conjunto", 0, x))

# ###
# Planejamento
# A28-A33
# 
# IPTU
# A34-A40
# 
# ISS
# A41-A45
# 
# Taxas
# A46-A51
# 
# Guarda Municipal
# A99 - A116

munic_2004 <- munic_2004 %>% dplyr::select(c("A1", "A164", "A28", "A29", "A30",
                                             "A31", "A33", "A34", "A35", "A36",
                                             "A37", "A38", "A39", "A40", "A41",
                                             "A42", "A43", "A44", "A45", "A46", 
                                             "A47", "A48", "A49", "A50", "A51",
                                             "A99", "A100"))

#Merging controls and all bartik-measure years
munic_2004_bartik <- munic_2004 %>% dplyr::select(-c("A1", "A164")) %>% 
  mutate_if(is.character, as.numeric) %>%
  add_column(munic_2004$A1, munic_2004$A164, .before = "A28") %>%
  rename(cod = "munic_2004$A1", municip = "munic_2004$A164") %>%
  mutate(cod = as.integer(cod))

#### NA introduzido por coerção? Ok!

munic_2004_bartik <- munic_2004_bartik %>% replace(is.na(.), 0)


munic_2004_bartik <- inner_join(munic_2004_bartik, controls, by = "cod")

#Adds Bartiks
munic_2004_final <- inner_join(munic_2004_bartik, pq_bartik_munic, by = "cod")

#Write to STATA
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(munic_2004_final, "munic_2004.dta")





# 2005
path_2005 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2005xls")

munic_2005_tmp <- path_2005 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_2005, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                        "..", "X", "Não aplicável"))

munic_2005_tmp <- munic_2005_tmp[-1]

munic_2005 <- reduce(munic_2005_tmp, full_join, by = "A1") %>%
  relocate("A326", .after = "A1")

munic_2005 <- munic_2005 %>% na_if("Não aplicável")
munic_2005 <- munic_2005 %>% na_if("Ignorado")

munic_2005 <- munic_2005 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
munic_2005 <- munic_2005 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))

munic_2005 <- munic_2005 %>% mutate_all(function(x) ifelse(x == "Separadamente", 1, x))
munic_2005 <- munic_2005 %>% mutate_all(function(x) ifelse(x == "Em conjunto", 0, x))



# IPTU
# A103-A109
# 
# Taxas
# A110-A115
# 
# Equipamentos culturais
#   Bibliotecas públicas
#   A267-A269
#   Museus
#   A270-A272
#   Teatros
#   A273-A275
#   Estádios
#   A276-A278


munic_2005 <- munic_2005 %>% dplyr::select(c("A1", "A326", "A103", "A104", "A105",
                                             "A106", "A107", "A108", "A109", "A110",
                                             "A111", "A112", "A113", "A114", "A115",
                                             "A267", "A268", "A269"))




# 2006
path_2006 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2006.xls")

munic_2006_tmp <- path_2006 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_2006, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                             "..", "X", "Não aplicável"))

munic_2006_tmp <- munic_2006_tmp[-1]

munic_2006 <- reduce(munic_2006_tmp, full_join, by = "A1") %>%
  relocate("A357", .after = "A1")


munic_2006 <- munic_2006 %>% na_if("Não aplicável")
munic_2006 <- munic_2006 %>% na_if("Ignorado")

munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))

munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Separadamente", 1, x))
munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Em conjunto", 0, x))



# 
# IPTU, ISS e Taxas
# A2-A17
# 
# Isenções de impostos e concessões
# A18-A40
# 
# Guarda municipal efetivo
# A287


munic_2006 <- munic_2006 %>% dplyr::select(c("A1", "A357", "A18", "A19", "A20",
                                             "A21", "A22", "A23", "A24", "A25",
                                             "A26", "A27", "A28", "A29", "A30", 
                                             "A31", "A32", "A33", "A34", "A35",
                                             "A36", "A37", "A38", "A39", "A40",
                                             "A287"))


#Merging controls and all bartik-measure years
munic_2006_bartik <- munic_2006 %>% dplyr::select(-c("A1", "A357")) %>%
  mutate_if(is.character, as.numeric) %>%
  add_column(munic_2006$A1, munic_2006$A357, .before = "A18") %>%
  rename(cod = "munic_2006$A1", municip = "munic_2006$A357") %>%
  mutate(cod = as.integer(cod))


munic_2006_bartik <- munic_2006_bartik %>% replace(is.na(.), 0)


munic_2006_bartik <- inner_join(munic_2006_bartik, controls, by = "cod")

#Adds Bartiks
munic_2006_final <- inner_join(munic_2006_bartik, pq_bartik_munic, by = "cod")


munic_2006_final <- munic_2006_final %>% rename(pq_2000 = "2000", pq_2001 = "2001",
                                                pq_2002 = "2002", pq_2003 = "2003",
                                                pq_2004 = "2004", pq_2005 = "2005",
                                                pq_2006 = "2006", pq_2007 = "2007",
                                                pq_2008 = "2008", pq_2009 = "2009",
                                                pq_2010 = "2010", pq_2011 = "2011",
                                                pq_2012 = "2012", pq_2013 = "2013",
                                                pq_2014 = "2014", pq_2015 = "2015")

#Write to STATA
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(munic_2006_final, "munic_2006.dta")





# 2008
path_2008 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2008.xls")

munic_2008_tmp <- path_2008 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_2008, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                             "..", "X", "Não aplicável"))

munic_2008_tmp <- munic_2008_tmp[-1]

munic_2008 <- reduce(munic_2008_tmp, full_join, by = "A1") %>%
  relocate("A338", .after = "A1")

# munic_2006 <- munic_2006 %>% na_if("Não aplicável")
# munic_2006 <- munic_2006 %>% na_if("Ignorado")
# 
# munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
# munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))
# 
# munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Separadamente", 1, x))
# munic_2006 <- munic_2006 %>% mutate_all(function(x) ifelse(x == "Em conjunto", 0, x))

#Nada



# 2009
path_2009 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2009.xls")

munic_2009_tmp <- path_2009 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_2009, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                             "..", "X", "Não aplicável"))

munic_2009_tmp <- munic_2009_tmp[-1]

munic_2009 <- reduce(munic_2009_tmp, full_join, by = "A1")


munic_2009 <- munic_2009 %>% na_if("Não aplicável")
munic_2009 <- munic_2009 %>% na_if("Ignorado")

munic_2009 <- munic_2009 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
munic_2009 <- munic_2009 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))

munic_2009 <- munic_2009 %>% mutate_all(function(x) ifelse(x == "Separadamente", 1, x))
munic_2009 <- munic_2009 %>% mutate_all(function(x) ifelse(x == "Em conjunto", 0, x))


# IPTU, ISS e Taxas
# A71-A84
# 
# Isenções
# A85-A98
# 
# Equipamentos culturais
# A247-A263
# 
# Regularização fundiária
# A348-A349
# 
# Saúde da Família
# A409-A413
# 
# Guarda Municipal
# A450


munic_2009 <- munic_2009 %>% dplyr::select(c("A1", "A85", "A86", "A87", "A88",
                                             "A89", "A90", "A91","A92", "A93", "A94",
                                             "A95", "A96", "A97", "A98", "A348", "A349",
                                             "A247", "A450", "A409", "A410", "A411", 
                                             "A412", "A413"))


#Merging controls and all bartik-measure years
munic_2009_bartik <- munic_2009 %>% dplyr::select(-"A1") %>% mutate_if(is.character, as.numeric) %>%
  add_column(munic_2009$A1, .before = "A85") %>%
  rename(cod = "munic_2009$A1") %>%
  mutate(cod = as.integer(cod))


munic_2009_bartik <- munic_2009_bartik %>% replace(is.na(.), 0)


munic_2009_bartik <- inner_join(munic_2009_bartik, controls, by = "cod")

#Adds Bartiks
munic_2009_final <- inner_join(munic_2009_bartik, pq_bartik_munic, by = "cod")


munic_2009_final <- munic_2009_final %>% rename(pq_2000 = "2000", pq_2001 = "2001",
                                                pq_2002 = "2002", pq_2003 = "2003",
                                                pq_2004 = "2004", pq_2005 = "2005",
                                                pq_2006 = "2006", pq_2007 = "2007",
                                                pq_2008 = "2008", pq_2009 = "2009",
                                                pq_2010 = "2010", pq_2011 = "2011",
                                                pq_2012 = "2012", pq_2013 = "2013",
                                                pq_2014 = "2014", pq_2015 = "2015")


#Write to STATA
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles")

write.dta(munic_2009_final, "munic_2009.dta")




# 2011
path_2011 <- here("data", "raw", "data_municipality", "munic", "MUNIC_2011.xls")

munic_2011_tmp <- path_2011 %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path_2011, col_names = TRUE, na = c("NA","N/A","", "...", "-", 
                                                             "..", "X", "Não aplicável"))

munic_2011_tmp <- munic_2011_tmp[-1]

munic_2011 <- reduce(munic_2011_tmp, full_join, by = "A1") %>%
  relocate("A570", .after = "A1")


munic_2011 <- munic_2011 %>% na_if("Não aplicável")
munic_2011 <- munic_2011 %>% na_if("Ignorado")

munic_2011 <- munic_2011 %>% mutate_all(function(x) ifelse(x == "Sim", 1, x))
munic_2011 <- munic_2011 %>% mutate_all(function(x) ifelse(x == "Não", 0, x))

#munic_2011 <- munic_2011 %>% mutate_all(function(x) ifelse(x ==(grepl('^[0-9.]+$')),
as.numeric(x), x))


#Nada







