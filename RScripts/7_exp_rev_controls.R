# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")


######### 1. Reads municipalities expenditure data from IPEA and constructs a data set ##########################################################################

# Changes working directory to apply purr::map
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Despesas")

# List of all Excel files
file.list_1 <- list.files(pattern='*.xls')

# Read all of them and store in a list
df_list_map_1 <- map(file.list_1, read_excel, sheet = "Séries", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Reads Brazilian IPCA inflation data with 2010 = 100
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Prices/ipca_anual.xls", 
                   sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 2000 &  Date <= 2010)


# Set expenditures names
exp_names <- c("agro", "comunic", "corrente", "corrente_outras", "educ", "energia", "func_total", "habit", "indcomsev", "judic", "legis", 
               "orcam", "prev", "saude", "seguran", "transp")

# Function for some cleanup
df_fix = function(x){
  x %>% rename(cod = "Codigo", municip = "Município") %>%
    dplyr::select(c("cod", "municip", `2000`:`2010`)) %>%
    pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "expenditure") %>%
    replace_na(list(expenditure = 0)) %>%
    arrange(cod)
}


# Apply the function to all dataframes
exp_list_nominal_long <- map(.x = df_list_map_1, .f = df_fix)



# Merge,  set expenditures names and select only the variable of interest
tmp <- reduce(exp_list_nominal_long, merge, by = c("cod", "municip", "year"))

exp_nominal_long <- tmp %>% setNames(c("cod", "municip", "year", glue("exp_{exp_names}"))) %>%
  as_tibble() %>%
  dplyr::select(c("cod", "municip", "year", "exp_corrente", "exp_educ", "exp_func_total", "exp_habit", "exp_orcam", "exp_prev",
                      "exp_saude", "exp_seguran", "exp_transp"))

# Deflates the values
deflate <- function(x) x/(ipca$Index_2/100)

##### na.rm???
exp_real_long <- mutate_all(exp_nominal_long[4:12], deflate) %>%
  add_column(cod = exp_nominal_long$cod, municip = exp_nominal_long$municip, year = exp_nominal_long$year, .before = "exp_corrente") %>%
  replace(is.na(.), 0)

# Saving
save(exp_real_long, file = "exp_real_long.RData")

######### 2. Reads and cleans up revenues data from IPEA #####################################################################################################

# Changes working directory to apply purr::map
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Receitas")

# List of all Excel files
file.list_2 <- list.files(pattern='*.xls')

# Read all of them and store in a list
df_list_map_2 <- map(file.list_2, read_excel, sheet = "Séries", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Set revenues names
rev_names <- c("rev_impost_tot", "rev_impost_outros", "rev_iptu", "rev_iss", "rev_taxas", "rev_tot", "rev_fpm", "rev_corrente", 
               "rev_orcam", "transf_ipva", "transf_icms", "trans_estad")

# Function for some cleanup
df_fix = function(x){
  x %>% rename(cod = "Codigo", municip = "Município") %>%
    dplyr::select(c("cod", "municip", `2000`:`2010`)) %>%
    pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "revenue")%>%
    replace_na(list(revenue = 0)) %>%
    arrange(cod)
}


# Apply the function to all dataframes
rev_list_nominal <- map(.x = df_list_map_2, .f = df_fix)



# Merge,  set expenditures names and select only the variable of interest
tmp <- reduce(rev_list_nominal, merge, by = c("cod", "municip", "year"))

rev_nominal <- tmp %>% setNames(c("cod", "municip", "year", glue("{rev_names}"))) %>%
  as_tibble()

# Deflates the values
deflate <- function(x) x/(ipca$Index_2/100)

##### na.rm???

rev_real <- mutate_all(rev_nominal[4:15], deflate) %>%
  add_column(cod = rev_nominal$cod, municip = rev_nominal$municip, year = rev_nominal$year, .before = "rev_impost_tot") %>%
  replace(is.na(.), 0)




######### 3. Reads and cleans ITR revenue data from Receita Federal ####################################################################################################

# Reads ITR data
itr <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/arrecadacao_itr.xlsx",
                     skip = 8, sheet = "Plan1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Reads municipalities codes from IBGE
mun_codes <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Código Municípios/cod_ibge/RELATORIO_DTB_BRASIL_MUNICIPIO_itr.xls",
                        col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Select the period and renames variables
rev_itr <- itr %>% rename(municip = "MUNICÍPIO") %>%
  dplyr::select(-"UNIDADE DA FEDERAÇÃO") %>%
  dplyr::select(c(municip, `2000`:`2010`)) %>%
  filter(!is.na(municip)) %>%
  filter(municip !="Exterior - EX")

# transfrom all names to lowercase
rev_itr <- rev_itr %>% mutate(municip = tolower(municip))


rev_itr <- rev_itr %>% mutate(municip = gsub("- ", "(", rev_itr$municip))

rev_itr$municip[-length(rev_itr$municip)] <- paste0(rev_itr$municip[-length(rev_itr$municip)], ')')
rev_itr[5526, 1] <- "palmas(to)"


# Change mun codes strings
teste_codes <- mun_codes %>% mutate(new_names = ifelse(Nome_UF == "Rondônia",  paste0(Nome_Município[-length(Nome_Município)], '(RO)'),
                                                 ifelse(Nome_UF == "Acre",  paste0(Nome_Município[-length(Nome_Município)], '(AC)'),    
                                                 ifelse(Nome_UF == "Amazonas",  paste0(Nome_Município[-length(Nome_Município)], '(AM)'),                
                                                 ifelse(Nome_UF == "Roraima",  paste0(Nome_Município[-length(Nome_Município)], '(RR)'),                                                       
                                                 ifelse(Nome_UF == "Pará",  paste0(Nome_Município[-length(Nome_Município)], '(PA)'),        
                                                 ifelse(Nome_UF == "Amapá",  paste0(Nome_Município[-length(Nome_Município)], '(AP)'),
                                                 ifelse(Nome_UF == "Tocantins",  paste0(Nome_Município[-length(Nome_Município)], '(TO)'),
                                                 ifelse(Nome_UF == "Maranhão",  paste0(Nome_Município[-length(Nome_Município)], '(MA)'),      
                                                 ifelse(Nome_UF == "Piauí",  paste0(Nome_Município[-length(Nome_Município)], '(PI)'),       
                                                 ifelse(Nome_UF == "Ceará",  paste0(Nome_Município[-length(Nome_Município)], '(CE)'),         
                                                 ifelse(Nome_UF == "Rio Grande do Norte",  paste0(Nome_Município[-length(Nome_Município)], '(RN)'),     
                                                 ifelse(Nome_UF == "Paraíba",  paste0(Nome_Município[-length(Nome_Município)], '(PB)'),                                                         
                                                 ifelse(Nome_UF == "Pernambuco",  paste0(Nome_Município[-length(Nome_Município)], '(PE)'),                                                        
                                                 ifelse(Nome_UF == "Alagoas",  paste0(Nome_Município[-length(Nome_Município)], '(AL)'),
                                                 ifelse(Nome_UF == "Sergipe",  paste0(Nome_Município[-length(Nome_Município)], '(SE)'),
                                                 ifelse(Nome_UF == "Bahia",  paste0(Nome_Município[-length(Nome_Município)], '(BA)'),
                                                 ifelse(Nome_UF == "Minas Gerais",  paste0(Nome_Município[-length(Nome_Município)], '(MG)'),
                                                 ifelse(Nome_UF == "São Paulo",  paste0(Nome_Município[-length(Nome_Município)], '(SP)'),
                                                 ifelse(Nome_UF == "Rio de Janeiro",  paste0(Nome_Município[-length(Nome_Município)], '(RJ)'),
                                                 ifelse(Nome_UF == "Espírito Santo",  paste0(Nome_Município[-length(Nome_Município)], '(ES)'),
                                                 ifelse(Nome_UF == "Rio Grande do Sul",  paste0(Nome_Município[-length(Nome_Município)], '(RS)'),                                                                 
                                                 ifelse(Nome_UF == "Paraná",  paste0(Nome_Município[-length(Nome_Município)], '(PR)'),                                                                 
                                                 ifelse(Nome_UF == "Santa Catarina",  paste0(Nome_Município[-length(Nome_Município)], '(SC)'),                
                                                 ifelse(Nome_UF == "Mato Grosso do Sul",  paste0(Nome_Município[-length(Nome_Município)], '(MS)'),
                                                 ifelse(Nome_UF == "Mato Grosso",  paste0(Nome_Município[-length(Nome_Município)], '(MT)'),          
                                                 ifelse(Nome_UF == "Goiás",  paste0(Nome_Município[-length(Nome_Município)], '(GO)'),
                                                 ifelse(Nome_UF == "Distrito Federal",  paste0(Nome_Município[-length(Nome_Município)], '(DF)'), ""
                                                 ))))))))))))))))))))))))))))

#Soemthing went wrong with Brasília (?)
teste_codes[5570, 10] <- "Brasília(DF)"

#Remove spaces
teste_codes$new_names  <- gsub(" ", "", teste_codes$new_names)
rev_itr$municip <- gsub(" ", "", rev_itr$municip)

#Remove special characters
teste_codes <- teste_codes %>% mutate(new_names = stri_trans_general(str = teste_codes$new_names, id = "Latin-ASCII"))
rev_itr <- rev_itr %>% mutate(municip = stri_trans_general(str = rev_itr$municip, id = "Latin-ASCII"))

# transfrom all names to lowercase
teste_codes <- teste_codes %>% mutate(new_names = tolower(new_names))


#Select
teste_codes <- teste_codes %>% dplyr::select(c("new_names", "Nome_Município", "Nome_UF", "Código Município Completo")) %>%
  rename(municip = "new_names", cod = "Código Município Completo")


# Merge the two datasets to get codes for itr data
itr_final <- full_join(teste_codes, rev_itr, by = "municip") %>%
  arrange(cod)


# Deflates ITR data
itr_long <- pivot_longer(itr_final, -c("municip", "Nome_Município", "Nome_UF", "cod"), values_to = "exp_itr", names_to = "year")


itr_real <- mutate_all(itr_long[6], deflate) %>%
  add_column(cod = itr_long$cod, municip = itr_long$municip, municip2 = itr_long$Nome_Município,  year = itr_long$year, 
             UF = itr_long$Nome_UF, .before = "exp_itr") %>%
  replace(is.na(.), 0)

# Real Final Join

rev_final <- left_join(itr_real, rev_real, by = c("cod", "year"))
rev_final_2 <- full_join(rev_real, itr_real, by = c("cod", "year"))


# Saving
save(rev_final, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/rev_final.RData")

# Saving in excel
#write_xlsx(itr_final, "C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/AMS/match_names.xlsx")


# Identifying duplicates for future correction
#dupl <- a[duplicated(a$municip), ]

# See which municipalities are missing
#anti_1 <- anti_join(teste_itr, teste_codes, by = "municip")
#anti_2 <- anti_join(teste_codes, teste_itr, by = "municip")


######### 5. Reads data containig municipalities that opted for the agreement of 100% revenue ITR ##################################################################################### 
#http://servicos.receita.fazenda.gov.br/Servicos/termoitr

itr_agr <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/itr_convenio.xlsx",
                   skip = 1 ,sheet = "Original", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


itr_agr <- itr_agr %>%
  group_by(grp = cumsum(str_detect(Município, '^UF:\\s+'))) %>%
             mutate(UF = toupper(str_extract(first(Município), '(?<=UF: )\\w{2}')),
                    Município = case_when(row_number() > 1 
                                     ~ sprintf('%s (%s)', Município, UF), TRUE ~ Município)) %>% 
  ungroup %>%
  dplyr::select(-c("grp"))



itr_agr <- itr_agr %>% rename(municip = "Município", situ = "Situação do Convênio", op_date = "Data da Opção", op_vigen = "Data da Vigência")%>% 
  mutate(op_date = ymd(op_date)) %>%
  mutate(op_vigen = ymd(op_vigen))


# Transform strings
itr_agr <- itr_agr %>% mutate(municip = tolower(municip))
itr_agr <- itr_agr %>% mutate(municip = stri_trans_general(str = itr_agr$municip, id = "Latin-ASCII"))
itr_agr$municip  <- gsub(" ", "", itr_agr$municip)
itr_agr$municip  <- gsub("-", "", itr_agr$municip)


# Codes and names for matching
teste_codes$municip  <- gsub("-", "", teste_codes$municip)

# Joining
itr_conv <- inner_join(itr_agr, teste_codes, by = "municip")

# Generate dummies
itr_conv$dummy_op_2010 <- ifelse(year(itr_conv$op_date) < 2010, 1, 0)
itr_conv$dummy_vigen_2010 <- ifelse(year(itr_conv$op_vigen) < 2010, 1, 0)
itr_conv$dummy_denun <- ifelse(itr_conv$situ == "Denúncia Vigente", 1, 0)
itr_conv$dummy_conv <- ifelse(itr_conv$situ == "Convênio Vigente", 1, 0)
itr_conv$dummy_conv_op_2010 <- ifelse(itr_conv$situ == "Convênio Vigente" & year(itr_conv$op_date) < 2010 , 1, 0)
itr_conv$dummy_conv_vigen_2010 <- ifelse(itr_conv$situ == "Convênio Vigente" & year(itr_conv$op_vigen) < 2010 , 1, 0)

#anti_1 <- anti_join(itr_agr, teste_codes, by = "municip")
#anti_2 <- anti_join(teste_codes, itr_agr, by = "municip")
#dupl <- teste[duplicated(teste$municip), ]


######### 5. Reads some municipality data that will be used as controls ##################################################################################### 

# Altitude measured in meters
altitude <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/altitude.xls",
                        col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Geographic area in km2
geo_area <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/area_geografica.xls",
                       col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Distance from state capital using 2000 municipality division
dist_state <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/dist_capital_estadual.xls",
                         col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Distance from federal capital
dist_federal <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/dist_capital_federal.xls",
                           col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Latitude in degrees
latitude <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/latitude.xls",
             col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Longitude
longitude <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/longitude.xls",
                        col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Just changing some names and arraging
altitude <- altitude %>% rename(altitude = "1998", cod = "Codigo", municip = "Município") %>%
  dplyr::select(-"Sigla") %>%
  arrange(cod)

dist_federal <- dist_federal %>% rename(dist_federal = "1998", cod = "Codigo", municip = "Município") %>%
  dplyr::select(-"Sigla") %>%
  arrange(cod)

dist_state <- dist_state %>% rename(dist_state = "1998", cod = "Codigo", municip = "Município") %>%
  dplyr::select(-"Sigla") %>%
  arrange(cod)

geo_area <- geo_area %>% rename(geo_area_2000 = "2000", geo_area_2010 = "2010", cod = "Codigo", municip = "Município") %>%
  dplyr::select(c("cod", "municip", "geo_area_2000", "geo_area_2010")) %>%
  arrange(cod)

latitude <- latitude %>% rename(latitude= "1998", cod = "Codigo", municip = "Município") %>%
  dplyr::select(-"Sigla") %>%
  arrange(cod)

longitude <- longitude %>% rename(longitude = "1998", cod = "Codigo", municip = "Município") %>%
  dplyr::select(-"Sigla") %>%
  arrange(cod)

# Joining all controls in one dataframe
list_controls <- list(altitude, dist_federal, dist_state, geo_area, latitude, longitude)

controls <- purrr::reduce(list_controls, inner_join, by = c("cod", "municip"))

# Creating a dummy if it is a state capital
controls$capital_dummy[controls$dist_state == 0] <- 1
controls$capital_dummy[controls$dist_state != 0] <- 0

# Transform it in a facotr variable
controls <- controls %>% mutate(capital_dummy = as_factor(capital_dummy))

# For merging
controls <- controls %>% mutate(cod = as.integer(cod))

save(controls, file = "C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/controls.RData")


######### Reads and merges MUNIC data #######################################################################################################################
munic_2004<- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/IPEA/Controles/altitude.xls",
                       col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))




