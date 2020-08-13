# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 1. Reads and cleans 1991, 2000 AND 2010 Census data from the Atlas do Desenvolvimento Humano no Brasil - PNUD #######################################
# Fortunatley, this dataset compiles the varaibles from the microdata of the past Census, so for the time being we will no work directly with IBGE raw data

# Reads Atlas raw data at the municipality level
atlas_mun <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Atlas PNUD/atlas2013_dadosbrutos_pt.xlsx",
                       sheet = "MUN 91-00-10", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Choose the variables of interest; we always use 7 code digits for the municipalities identification
# All values are in 2010 R$

# ANO - Year of the Census observation
# Codmun7 - Municipalities 7-digit IBGE codes
# Município - Municipalities names
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
# CORTE 9 - 9th decil of per capita household income distribution
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



variables <- c("ANO", "Codmun7", "Município", "E_ANOSESTUDO", "T_ANALF15M", "T_ANALF18M", "T_FREQ6A14", "T_FUND15A17", "T_FUND18M",
               "T_MED18M", "CORTE1", "CORTE2", "CORTE3", "CORTE4", "CORTE9", "GINI", "PIND", "PINDCRI", "PMPOB", "PMPOBCRI",
               "PREN10RICOS", "PREN20", "PREN40", "PREN60", "R1040", "R2040", "RDPC", "RDPC1", "RDPC10", "RDPC2", "RDPC3", "RDPC4",
               "RDPC5", "RDPCT", "THEIL", "P_AGRO", "P_SERV", "RENOCUP", "T_AGUA", "T_BANAGUA", "T_DENS", "T_LIXO", "T_LUZ",
               "AGUA_ESGOTO", "T_SLUZ", "pesoRUR", "pesourb", "pesotot", "POP", "I_ESCOLARIDADE", "I_FREQ_PROP", "IDHM", "IDHM_E", "IDHM_L", "IDHM_R")

atlas_mun <- atlas_mun %>% dplyr::select(variables)

# Changing Na values to zero
atlas_mun <- atlas_mun %>% mutate_all(~replace(., is.na(.), 0))

######### 2. Reads and cleans data from IBGE Pesquisa de Assistência Médico-Sanitária for 2002 and 2009 ######################################################

# Reading the excel file for 2002 number of health establishments
ams_2002_tot <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/AMS/2002/tab05_2002_total.xls",
                        skip = 3, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))











