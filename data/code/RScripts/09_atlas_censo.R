# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 1. Reads and cleans 1991, 2000 AND 2010 Census data from the Atlas do Desenvolvimento Humano no Brasil - PNUD #######################################
# Fortunatley, this dataset compiles the varaibles from the microdata of the past Census, so for the time being we will no work directly with IBGE raw data

# Reads Atlas raw data at the municipality level
atlas_mun <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/Atlas PNUD/atlas2013_dadosbrutos_pt.xlsx",
                       sheet = "MUN 91-00-10", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Choose the variables of interest; we always use 7 code digits for the municipalities identification
# All values are in 2010 R$

# ANO - Year of the Census observation
# Codmun7 - Municipalities 7-digit IBGE codes
# Munic�pio - Municipalities names
# E_ANOSESTUDO - Expectation of year of schooling at the age of 18
# T_ANALF15M - Illiteracy rate for the population above 15 years old
# T_ANALF18M - Illiteracy rate for the population above 18 years old
# T_FREQ6A14 - Rate of school attendance for population between 6 and 14 years old
# T_FUND15A17 - Percentage of population between 15 and 17 years that have completed elementary school
# T_FUND18M - Percentage of population above 18 years that have completed elementary school
# T_MED18M - Percentage of population above 18 years that have completed high school
# CORTE1 - First quintile of per capita household income distribution
# CORTE2 - Second quintile of per capita household  income distribution
# CORTE3 - Third quintile of per capita household income distribution
# CORTE4 - Fourth quintile of per capita household income distribution
# CORTE9 - 9th decil of per capita household income distribution
# GINI - Gini index for per capita household income
# PIND - Proportion of extremely poor
# PINDCRI - Proportion of extremely poor children
# PMPOB - Proportion of poor
# PMPOBCRI - Proportion of poor children
# PREN10RICOS - Percentange of total income owned by the richest 10%
# PREN20 - Percentange of total income owned by the poorest 20%
# PREN40 - Percentange of total income owned by the poorest 40%
# PREN60 - Percentange of total income owned by the poorest 60%
# R1040 - Ratio for 10% richest/40% poorest
# R2040 - Ratio for 20% richest/40 poorest
# RDPC - Average per capita income
# RDPC1 - Average HOUSEHOLD income per capita of the FIRST poorest fifth of the population
# RDPC10 - Average income per capita of the richest tenth
# RDPC2 - Average income per capita of the SECOND poorest fifth of the population
# RDPC3 - Average income per capita of the THIRD poorest fifth of the population
# RDPC4 - Average income per capita of the FOURTH poorest fifth of the population
# RDPC5 - Average income per capita of the RICHEST fifth of the population
# RDPCT - Average per capita income excluding null incomes
# THEIL - Theil index
# P_AGRO - Percentage of worker in the agriculture sector
# P_SERV - Percentage of worker in the service sector
# RENOCUP - Average income of workers
# T_AGUA - Percentange of population that live in houses with piped water
# T_BANAGUA - Percentange of population that live in houses with bathroom and piped water
# T_DENS - Percentage of population that live in houses with density greater than 2 per bedroom
# T_LIXO - Percentage of population that live in urban houses with garbage collection
# T_LUZ - Percentage of popualtion that live in houses with electricity
# AGUA_ESGOTO - Percentage of population in houses with inadequate water supply and sewage services
# T_SLUZ - Percentage of population in houses without electricity
# pesoRUR - Total rural population
# pesourb - Total urban popualtion
# pesotot - Total population
# POP - Total popualtion that live im permanent particular houses
# I_ESCOLARIDADE - Index for elementary schooling
# I_FREQ_PROP - Index for school attendence
# IDHM - Human development index for the municipalities
# IDHM_ E - Subindex for education
# IDHM_L - Subindex for longevity
# IDHM_R - Subindex for income



variables <- c("ANO", "Codmun7", "Munic�pio", "E_ANOSESTUDO", "T_ANALF15M", "T_ANALF18M", "T_FREQ6A14", "T_FUND15A17", "T_FUND18M",
               "T_MED18M", "CORTE1", "CORTE2", "CORTE3", "CORTE4", "CORTE9", "GINI", "PIND", "PINDCRI", "PMPOB", "PMPOBCRI",
               "PREN10RICOS", "PREN20", "PREN40", "PREN60", "R1040", "R2040", "RDPC", "RDPC1", "RDPC10", "RDPC2", "RDPC3", "RDPC4",
               "RDPC5", "RDPCT", "THEIL", "P_AGRO", "P_SERV", "RENOCUP", "T_AGUA", "T_BANAGUA", "T_DENS", "T_LIXO", "T_LUZ",
               "AGUA_ESGOTO", "T_SLUZ", "pesoRUR", "pesourb", "pesotot", "POP", "I_ESCOLARIDADE", "I_FREQ_PROP", "IDHM", "IDHM_E", "IDHM_L", "IDHM_R")

atlas_mun <- atlas_mun %>% dplyr::select(variables)

save(atlas_mun, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/atlas_mun.RData")


# Changing Na values to zero
atlas_mun <- atlas_mun %>% mutate_all(~replace(., is.na(.), 0))

atlas_mun2 <- atlas_mun %>% dplyr::select(variables)




######### 2. Reads and cleans data from IBGE Pesquisa de Assist�ncia M�dico-Sanit�ria for 2002 and 2009 ######################################################

# Reading the excel file for 2002 number of health establishment and number of beds; did some changes on the original excel file
ams_2002_tot <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2002/tab05_2002_total.xls",
                        skip = 7, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ams_2002_atend <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2002/tab06_2002_atend.xls",
                             skip = 7, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ams_2002_leitos <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2002/tab18_2002_leitos.xls",
                              skip = 7, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# The same for 2009
ams_2009_tot <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2009/tab05_2009_total.xls",
                              skip = 8, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


ams_2009_atend <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2009/tab06_2009_atend_corr.xls",
                           skip = 8, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

ams_2009_leitos <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2009/tab13_2009_leitos.xls",
                             skip = 8, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))



# Doing some cleanup

ams_2002_tot <- ams_2002_tot %>% rename (cod_uf = "C�digo num�rico da UF", cod_mun = "...16", 
                                         municip = "Grandes Regi�es", estab_total = "Total...6", estab_tot_pub = "Total...7", 
                                         estab_federal = "Federal", estab_estadual = "Estadual", estab_munic = "Municipal", 
                                         estab_total_priv = "Total...11", estab_cluc = "Com fins lucrativos", estab_nluc = "Sem fins lucrativos", 
                                         estab_sus = "SUS")

ams_2002_atend <- ams_2002_atend %>% rename (cod_uf = "C�digo num�rico da UF", cod_mun = "...34", municip = "Grandes Regi�es", 
                                             atend_total = "Total...6", atend_total_cintern = "Com interna��o...7", atend_total_nintern = "Sem interna��o...8", 
                                             atend_total_terap = "Apoio � diagnose e terapia...9", atend_total_pub = "Total...10", 
                                             atend_cintern_pub = "Com interna��o...11", atend_nintern_pub = "Sem interna��o...12", 
                                             atend_terap_pub = "Apoio � diagnose e terapia...13", atend_total_federal = "Total...14", 
                                             atend_cintern_federal = "Com interna��o...15", atend_nintern_federal = "Sem interna��o...16", 
                                             atend_terap_federal = "Apoio � diagnose e terapia...17", atend_total_estadual =  "Total...18", 
                                             atend_cintern_estadual = "Com interna��o...19", atend_nintern_estadual = "Sem interna��o...20", 
                                             atend_terap_estadual = "Apoio � diagnose e terapia...21", atend_total_munic = "Total...22", 
                                             atend_cintern_munic = "Com interna��o...23" , atend_nintern_munic = "Sem interna��o...24", 
                                             atend_terap_munic = "Apoio � diagnose e terapia...25", atend_total_priv = "Total...26" , 
                                             atend_cintern_priv = "Com interna��o...27", atend_nintern_priv = "Sem interna��o...28", 
                                             atend_terap_priv = "Apoio � diagnose e terapia...29", atend_total_sus = "Total...30", 
                                             atend_cintern_sus = "Com interna��o...31", atend_nintern_sus = "Sem interna��o...32", 
                                             atend_terap_sus = "Apoio � diagnose e terapia...33")


ams_2002_leitos <- ams_2002_leitos %>% rename (cod_uf = "C�digo num�rico da UF", cod_mun = "...13", 
                                               municip = "Grandes Regi�es", leitos_total = "Total...6", leitos_tot_pub = "Total...7",
                                               leitos_federal = "Federal", leitos_estadual = "Estadual", leitos_munic = "Municipal", 
                                               leitos_total_priv = "Total...11", leitos_sus = "SUS")




ams_2009_tot <- ams_2009_tot %>% rename (municip = "Grandes Regi�es", estab_total = "Total...2", estab_tot_pub = "Total...3", 
                                         estab_federal = "Federal", estab_estadual = "Estadual", estab_munic = "Municipal", 
                                         estab_total_priv = "Total...7", estab_cluc = "Com fins\nlucrativos", estab_nluc = "Sem fins\nlucrativos", 
                                         estab_sus = "SUS")


ams_2009_leitos <- ams_2009_leitos %>% rename(municip = "Grandes Regi�es", leitos_total = "Total...2", leitos_tot_pub = "Total...3", 
                                              leitos_federal = "Federal", leitos_estadual = "Estadual", leitos_munic = "Municipal", 
                                              leitos_total_priv = "Total...7", leitos_sus = "SUS")


ams_2009_atend <- ams_2009_atend %>% rename(municip = "Grandes Regi�es\n", atend_total = "Total...2", atend_total_cintern = "Com\ninter-\nna��o...3", 
                                            atend_total_nintern = "Sem\ninter-\nna��o...4", atend_total_terap = "Apoio �\ndiagnose e\nterapia...5", 
                                            atend_total_pub = "Total...6", atend_cintern_pub = "Com\ninter-\nna��o...7", atend_nintern_pub =  "Sem\ninter-\nna��o...8", 
                                            atend_terap_pub = "Apoio �\ndiagnose e\nterapia...9", atend_total_priv = "Total...10", 
                                            atend_cintern_priv = "Com\ninter-\nna��o...11", atend_nintern_priv = "Sem\ninter-\nna��o...12" , 
                                            atend_terap_priv = "Apoio � diagnose e terapia...13", atend_total_sus = "Total...14",
                                            atend_cintern_sus =  "Com\ninter-\nna��o...15", atend_nintern_sus = "Sem\ninter-\nna��o...16",
                                            atend_terap_sus = "Apoio � diagnose e terapia...17")




# Concatenate strings to form municipalities IBGE code
ams_2002_tot <- ams_2002_tot %>% mutate(cod_uf =  as.character(cod_uf))
ams_2002_tot <- ams_2002_tot %>% mutate(cod = paste0(cod_uf, cod_mun)) %>%
  dplyr::select(c("cod", "municip", "estab_total", "estab_tot_pub", "estab_federal", "estab_estadual", "estab_munic", "estab_total_priv",
                  "estab_cluc", "estab_nluc", "estab_sus"))

ams_2002_tot <- ams_2002_tot %>% mutate(cod = as.integer(cod))


ams_2002_leitos <- ams_2002_leitos %>% mutate(cod_uf =  as.character(cod_uf))
ams_2002_leitos <- ams_2002_leitos %>% mutate(cod = paste0(cod_uf, cod_mun)) %>%
  dplyr::select(-c("N�mero de Ordem", "C�digo num�rico da regi�o", "cod_uf", "cod_mun", "C�digo num�rico do munic�pio"))

ams_2002_leitos <- ams_2002_leitos %>% mutate(cod = as.integer(cod))


ams_2002_atend <- ams_2002_atend %>% mutate(cod_uf =  as.character(cod_uf))
ams_2002_atend <- ams_2002_atend %>% mutate(cod = paste0(cod_uf, cod_mun)) %>%
  dplyr::select(-c("N�mero de Ordem", "C�digo num�rico da regi�o", "cod_uf", "cod_mun", "C�digo num�rico do munic�pio"))

ams_2002_atend <- ams_2002_atend %>% mutate(cod = as.integer(cod))



#Take out regions
ams_2002_tot <- ams_2002_tot %>% filter(municip != "BRASIL" & municip != "REGI�ONORTE" & municip != "REGI�ONORDESTE" & 
                                          municip != "REGI�OSUDESTE" & municip != "REGI�OSUL"
                                        & municip != "REGI�OCENTRO-OESTE")

# Take out states
ams_2002_tot <- ams_2002_tot %>% filter(!grepl("UF:", ams_2002_tot$municip))


# Joining 2002

ams_2002 <- inner_join(ams_2002_tot, dplyr::select(ams_2002_leitos, -c("municip")), by = "cod")
ams_2002 <- inner_join(ams_2002, dplyr::select(ams_2002_atend, -c("municip")), by = "cod")

ams_2002 <- ams_2002 %>% mutate_all(~replace(., is.na(.), 0))

save(ams_2002, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2002.RData")


# Transforming names for 2009
#Take out regions
ams_2009_tot <- ams_2009_tot %>% filter(municip != "BRASIL" & municip != "NORTE" & municip != "NORDESTE" & municip != "SUDESTE" & municip != "SUL"
                                        & municip != "CENTRO-OESTE")

# Give state abbreviations
ams_2009_tot <- ams_2009_tot %>%
  group_by(grp = cumsum(str_detect(municip, '^UF:\\s+'))) %>%
  mutate(UF = toupper(str_extract(first(municip), '(?<=UF: )\\w{2}')),
         municip = case_when(row_number() > 1 
                               ~ sprintf('%s (%s)', municip, UF), TRUE ~ municip)) %>% 
  ungroup %>%
  dplyr::select(-c("grp"))

# Typical string manipulations
ams_2009_tot <- ams_2009_tot %>% mutate(municip = tolower(municip))
ams_2009_tot <- ams_2009_tot %>% mutate(municip = stri_trans_general(str = ams_2009_tot$municip, id = "Latin-ASCII"))
ams_2009_tot$municip  <- gsub(" ", "", ams_2009_tot$municip)
ams_2009_tot$municip  <- gsub("-", "", ams_2009_tot$municip)

# Take out states
ams_2009_tot <- ams_2009_tot %>% filter(!grepl("uf:", ams_2009_tot$municip))



# Again two times
ams_2009_atend <- ams_2009_atend %>% filter(municip != "BRASIL" & municip != "NORTE" & municip != "NORDESTE" & municip != "SUDESTE" & municip != "SUL"
                                        & municip != "CENTRO-OESTE")

ams_2009_atend <- ams_2009_atend %>%
  group_by(grp = cumsum(str_detect(municip, '^UF:\\s+'))) %>%
  mutate(UF = toupper(str_extract(first(municip), '(?<=UF: )\\w{2}')),
         municip = case_when(row_number() > 1 
                             ~ sprintf('%s (%s)', municip, UF), TRUE ~ municip)) %>% 
  ungroup %>%
  dplyr::select(-c("grp"))

ams_2009_atend <- ams_2009_atend %>% mutate(municip = tolower(municip))
ams_2009_atend <- ams_2009_atend %>% mutate(municip = stri_trans_general(str = ams_2009_atend$municip, id = "Latin-ASCII"))
ams_2009_atend$municip  <- gsub(" ", "", ams_2009_atend$municip)
ams_2009_atend$municip  <- gsub("-", "", ams_2009_atend$municip)

ams_2009_atend <- ams_2009_atend %>% filter(!grepl("uf:", ams_2009_atend$municip))



ams_2009_leitos <- ams_2009_leitos %>% filter(municip != "BRASIL" & municip != "NORTE" & municip != "NORDESTE" & municip != "SUDESTE" & municip != "SUL"
                                            & municip != "CENTRO-OESTE")

ams_2009_leitos <- ams_2009_leitos %>%
  group_by(grp = cumsum(str_detect(municip, '^UF:\\s+'))) %>%
  mutate(UF = toupper(str_extract(first(municip), '(?<=UF: )\\w{2}')),
         municip = case_when(row_number() > 1 
                             ~ sprintf('%s (%s)', municip, UF), TRUE ~ municip)) %>% 
  ungroup %>%
  dplyr::select(-c("grp"))

ams_2009_leitos <- ams_2009_leitos %>% mutate(municip = tolower(municip))
ams_2009_leitos <- ams_2009_leitos %>% mutate(municip = stri_trans_general(str = ams_2009_leitos$municip, id = "Latin-ASCII"))
ams_2009_leitos$municip  <- gsub(" ", "", ams_2009_leitos$municip)
ams_2009_leitos$municip  <- gsub("-", "", ams_2009_leitos$municip)

ams_2009_leitos <- ams_2009_leitos %>% filter(!grepl("uf:", ams_2009_leitos$municip))



# Reads municipalities names for merging
aux <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/match_names_ams.xlsx",
                            col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

aux <- aux %>% rename(municip2 = "Nome_Munic�pio") %>%
  dplyr::select(c("cod", "municip", "municip2", "Nome_UF"))

#Typical string manipulations
aux <- aux %>% mutate(municip = stri_trans_general(str = aux$municip, id = "Latin-ASCII"))
aux <- aux %>% mutate(municip = tolower(municip))
aux <- aux %>% mutate(municip = gsub("-", "", aux$municip))
aux$municip <- gsub(" ", "", aux$municip)

aux <- aux %>% arrange(municip)




#dupl <- ams_2002[duplicated(ams_2002$municip), ]



####################################### Merge IBGE codes on 2009 data

# Not gooin to use this now

# mun_codes <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/RELATORIO_DTB_BRASIL_MUNICIPIO_ams.xls",
#                         col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
# 
# mun_codes <- mun_codes %>% mutate(municip = ifelse(Nome_UF == "Rond�nia",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(RO)'),
#                                                 ifelse(Nome_UF == "Acre",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(AC)'),    
#                                                 ifelse(Nome_UF == "Amazonas",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(AM)'),                
#                                                 ifelse(Nome_UF == "Roraima",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(RR)'),                                                       
#                                                 ifelse(Nome_UF == "Par�",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(PA)'),        
#                                                 ifelse(Nome_UF == "Amap�",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(AP)'),
#                                                 ifelse(Nome_UF == "Tocantins",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(TO)'),
#                                                 ifelse(Nome_UF == "Maranh�o",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(MA)'),      
#                                                 ifelse(Nome_UF == "Piau�",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(PI)'),       
#                                                 ifelse(Nome_UF == "Cear�",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(CE)'),         
#                                                 ifelse(Nome_UF == "Rio Grande do Norte",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(RN)'),     
#                                                 ifelse(Nome_UF == "Para�ba",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(PB)'),                                                         
#                                                 ifelse(Nome_UF == "Pernambuco",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(PE)'),                                                        
#                                                 ifelse(Nome_UF == "Alagoas",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(AL)'),
#                                                 ifelse(Nome_UF == "Sergipe",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(SE)'),
#                                                 ifelse(Nome_UF == "Bahia",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(BA)'),
#                                                 ifelse(Nome_UF == "Minas Gerais",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(MG)'),
#                                                 ifelse(Nome_UF == "S�o Paulo",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(SP)'),
#                                                 ifelse(Nome_UF == "Rio de Janeiro",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(RJ)'),
#                                                 ifelse(Nome_UF == "Esp�rito Santo",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(ES)'),
#                                                 ifelse(Nome_UF == "Rio Grande do Sul",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(RS)'),                                                                 
#                                                 ifelse(Nome_UF == "Paran�",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(PR)'),                                                                 
#                                                 ifelse(Nome_UF == "Santa Catarina",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(SC)'),                
#                                                 ifelse(Nome_UF == "Mato Grosso do Sul",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(MS)'),
#                                                 ifelse(Nome_UF == "Mato Grosso",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(MT)'),          
#                                                 ifelse(Nome_UF == "Goi�s",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(GO)'),
#                                                 ifelse(Nome_UF == "Distrito Federal",  paste0(Nome_Munic�pio[-length(Nome_Munic�pio)], '(DF)'), ""
#                                                 ))))))))))))))))))))))))))))



# mun_codes <- mun_codes %>% rename (cod = "C�digo Munic�pio Completo")
# 
# mun_codes <- arrange(mun_codes, municip)
# 
# mun_codes <- mun_codes %>% mutate(municip = stri_trans_general(str = mun_codes$municip, id = "Latin-ASCII"))
# mun_codes <- mun_codes %>% mutate(municip = tolower(municip))
# mun_codes$municip  <- gsub(" ", "", mun_codes$municip)
# mun_codes$municip  <- gsub("-", "", mun_codes$municip)


# a <- anti_join(ams_2009_tot, mun_codes, by = "municip")
# b <- anti_join(mun_codes, ams_2009_tot, by = "municip")




# Check antijoins
c <- anti_join(ams_2009_tot, aux, by = "municip")
d <- anti_join(aux, ams_2009_tot, by = "municip")




# Merging
teste <- full_join(ams_2009_tot, aux, by = "municip")

ams_2009 <- full_join(teste, ams_2009_atend, by = "municip")
ams_2009 <- full_join(ams_2009, ams_2009_leitos, by = "municip")

ams_2009 <- ams_2009 %>% mutate_all(~replace(., is.na(.), 0))

save(ams_2009, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/ams_2009.RData")


######### 3. Reads Censo Escolar Data ############################################################################################################################

ams_2009_leitos <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Munic�pios/AMS/2009/tab13_2009_leitos.xls",
                              skip = 8, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))




















