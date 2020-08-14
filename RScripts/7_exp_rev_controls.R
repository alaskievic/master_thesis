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

mun_codes <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Dados/Dados Municípios/Código Municípios/cod_ibge/RELATORIO_DTB_BRASIL_MUNICIPIO.xls",
                        col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# Select the period and renames variables
rev_itr <- itr %>% rename(municip = "MUNICÍPIO") %>%
  dplyr::select(-"UNIDADE DA FEDERAÇÃO") %>%
  dplyr::select(c(municip, `2000`:`2010`)) %>%
  filter(!is.na(municip))

# Arrange the data alphabetically to match it to code numbers
rev_itr$municip <- gsub(" -.*", "", rev_itr$municip)

rev_itr <- arrange(rev_itr, municip)

# There is a observation called exterior, take it out
rev_itr <- rev_itr %>% filter(municip !="Exterior")


# Select codes and municipalities names
mun_codes <- mun_codes %>% rename (municip = "Nome_Município", cod = "Código Município Completo") %>%
  dplyr::select(cod, municip)

# Arrange alphabetically to match the ITR data set
mun_codes <- arrange(mun_codes, municip)

# Merge the two datasets to get codes for itr data
rev_itr_codes <- semi_join(rev_itr, mun_codes, by = "municip")

a <- full_join(mun_codes, rev_itr, by = "municip") %>%
  arrange(cod)


# Identifying duplicates for future correction
dupl <- a[duplicated(a$municip), ]

# See which municipalities are missing
anti_1 <- anti_join(rev_itr, mun_codes, by = "municip")
anti_2 <- anti_join(mun_codes, rev_itr, by = "municip")

######### Municipalities with same name are highly problematic

# I changed some names in both excel files to match grammar between them. Most importantly, I changed Embu-Guaçu to Embu Guaçu;
# Lauro Muller; Quixaba which appears twice; Pariquera Açu; Januário Cicco - Boa Saúde;  in the official IBGE file
 

######################## Treinar string replacment nesses dois datasets depois



# Deflates the values
deflate <- function(x) x/(ipca$Index_2/100)

##### na.rm???

rev_real <- mutate_all(rev_nominal[4:15], deflate) %>%
  add_column(cod = rev_nominal$cod, municip = rev_nominal$municip, year = rev_nominal$year, .before = "rev_impost_tot") %>%
  replace(is.na(.), 0)






















