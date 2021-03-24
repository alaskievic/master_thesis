# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")




######### 1. Matching AMCs and Municipality Codes ###############################


amc_ibge <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/Municipios_X_AMCs_1872_1997.xls", 
                        sheet = "Municípios_X_AMCs", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


amc_ibge %<>% dplyr::select(UFMUNDV, NOMEMUN, CODAMC1991_1997, NEW_CODE_1991_1997, 
                            NOME_1991_1997) %>% setnames(c("cod", "name_amcom", 
                                                         "old_amc", "amc", "name_amc"))

thiago <- read_csv("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/controls1991amc.csv",
                     col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

thiago %<>% dplyr::select(-X1)


# Matching controls and amc

amc_thiago <- inner_join(amc_ibge, thiago, by = "amc")


# Matching with AMC codes from BD+

bd_amc <- read_csv("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/bd_amc.csv",
                   col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Filter only the desired periods

bd_amc_1991 <- bd_amc %>% filter(ano_de == 1991 & ano_para == 2010)

bd_amc_1991 %<>% rename(cod = id_municipio)


amc_final <- full_join(amc_thiago, bd_amc_1991, by = "cod")


save(amc_final,
          file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/amc_final.Rdata")
