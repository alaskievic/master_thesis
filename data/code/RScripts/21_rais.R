# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packages
source("./00_load_packages.R")

memory.limit(size = NA)
memory.limit(size = 50000)


######### 1. Reads cleaned RAIS Data for Municipalities and Calculates Shares ##

# PASSO 1: criar usuário e projeto no BigQuery

# PASSO 2: criar arquivo de credenciais e salvar numa pasta
# https://console.cloud.google.com/apis/credentials/serviceaccountkey?project=<project_id>
# service account name: admin
# role: project owner

# Apontar a autenticação para o arquivo json
bq_auth(path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RAIS SQL/RAIS-66fc2cf2df60.json")

# Criar conexão com o BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  billing = "rais-301720",
  project = "basedosdados"
)

query = "WITH rais_totemp AS (SELECT id_municipio, ano, COUNT(*) as total_emp
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_agri AS (
SELECT id_municipio, ano, COUNT(cnae_1) as agriculture
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '01%'OR
    cnae_1 LIKE '02%'
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_manu AS (
SELECT id_municipio, ano, COUNT(cnae_1) as manufacturing
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '10%' OR
    cnae_1 LIKE '11%' OR
    cnae_1 LIKE '12%' OR
    cnae_1 LIKE '13%' OR
    cnae_1 LIKE '14%' OR
    cnae_1 LIKE '15%' OR
    cnae_1 LIKE '16%' OR
    cnae_1 LIKE '17%' OR
    cnae_1 LIKE '18%' OR
    cnae_1 LIKE '19%' OR
    cnae_1 LIKE '2%' OR
    cnae_1 LIKE '3%' 
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_manuc AS (
SELECT id_municipio, ano, COUNT(cnae_1) as manufacturing_construc
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '10%' OR
    cnae_1 LIKE '11%' OR
    cnae_1 LIKE '12%' OR
    cnae_1 LIKE '13%' OR
    cnae_1 LIKE '14%' OR
    cnae_1 LIKE '15%' OR
    cnae_1 LIKE '16%' OR
    cnae_1 LIKE '17%' OR
    cnae_1 LIKE '18%' OR
    cnae_1 LIKE '19%' OR
    cnae_1 LIKE '2%' OR
    cnae_1 LIKE '3%' OR
    cnae_1 LIKE '45%'  /*Construction*/
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_serv AS (
SELECT id_municipio, ano, COUNT(cnae_1) as services
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '5%' OR
    cnae_1 LIKE '6%' OR
    cnae_1 LIKE '7%' OR
    cnae_1 LIKE '8%' OR
    cnae_1 LIKE '9%' 
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_servcomp AS (
SELECT id_municipio, ano, COUNT(cnae_1) as services_complete
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '40%' OR
    cnae_1 LIKE '41%' OR
    cnae_1 LIKE '5%' OR
    cnae_1 LIKE '6%' OR
    cnae_1 LIKE '7%' OR
    cnae_1 LIKE '8%' OR
    cnae_1 LIKE '9%' 
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
)
SELECT *
FROM rais_totemp
FULL OUTER JOIN rais_agri
ON rais_totemp.id_municipio = rais_agri.id_municipio AND rais_totemp.ano = rais_agri.ano
FULL OUTER JOIN rais_manu
ON rais_totemp.id_municipio = rais_manu.id_municipio AND rais_totemp.ano = rais_manu.ano
FULL OUTER JOIN rais_manuc
ON rais_totemp.id_municipio = rais_manuc.id_municipio AND rais_totemp.ano = rais_manuc.ano
FULL OUTER JOIN rais_serv
ON rais_totemp.id_municipio = rais_serv.id_municipio AND rais_totemp.ano = rais_serv.ano
FULL OUTER JOIN rais_servcomp
ON rais_totemp.id_municipio = rais_servcomp.id_municipio AND rais_totemp.ano = rais_servcomp.ano;"

teste_rais = dbGetQuery(con, query)

# Observe that teste_rais equals the excel file below after filtering for year >= 1994


rais <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/RAIS/rais_municip.xlsx", 
                       col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

rais %<>% dplyr::select(c("id_municipio", "ano", "total_emp", "agriculture", 
                          "manufacturing", "manufacturing_construc", "services", 
                          "services_complete")) %>%
  rename(cod = "id_municipio", year = "ano") %>% filter(year >= 1994) %>%
  arrange(cod)

# Replacing NA values with 0
rais[is.na(rais)] <- 0


# Merging with municipalities names
mun_codes <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/cod_ibge/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx", 
                   col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

mun_codes %<>% rename(municip = "Nome_Município", cod = "Código Município Completo") %>%
  dplyr::select(c(cod, municip)) %>% mutate(cod = as.integer(cod))


rais <- full_join(rais, mun_codes, by = "cod")


# Calculates Shares
rais_shares <- rais %>% mutate(agriculture = agriculture/total_emp,  
                          manufacturing = manufacturing/total_emp,
                          manufacturing_construc = manufacturing_construc/total_emp,
                          services = services/total_emp, 
                          services_complete = services_complete/total_emp) %>%
  dplyr::select(-total_emp) %>%
  rename(agriculture_rais = "agriculture", manufacturing_rais = "manufacturing", 
         manufc_rais = "manufacturing_construc", services_rais = "services",
         servicesc_rais = "services_complete") %>% arrange(cod,year)

rais_shares <- full_join(rais_shares, dplyr::select(rais, -"municip"), by = c("cod", "year"))

# Saving
save(rais_shares, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/rais_shares.RData")


# Share for Brazil for Graph
rais_brazil <- rais %>% group_by(year) %>%
  summarise(agriculture = sum(agriculture)/sum(total_emp), 
            manufacturing = sum(manufacturing)/sum(total_emp), 
            manufacturing_construc = sum(manufacturing_construc)/sum(total_emp),
            services = sum(services)/sum(total_emp), 
            services_complete = sum(services_complete)/sum(total_emp))


# Pivoting for Graph
rais_brazil %<>% rename (Agriculture = "agriculture", Manufacturing = "manufacturing", 
                         "Manufacturing + Construction" = "manufacturing_construc",
                         Services = "services") %>% dplyr::select(-services_complete)

rais_brazil %<>% pivot_longer(-year, names_to = "Sector", values_to = "shares")


# Graphing
# Defining Palette
c25 <- c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

rais_graph <- ggplot(rais_brazil, aes(x=year)) +
  geom_line(data = rais_brazil, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing",
                                "Manufacturing + \nConstruction", "Services"))+
  geom_point(data = rais_brazil, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  scale_y_continuous(breaks= c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) +
  scale_x_continuous(breaks= c(1995, 2000, 2005, 2010, 2015, 2020)) +
  ggtitle("Employment Shares in Brazil (RAIS Data)") +
  theme(legend.text=element_text(size=13))+
  theme_bw(base_size = 13)

rais_graph

ggsave(filename = "empshares_rais.png", plot = rais_graph, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")



######### 2. Employment Shares for Agroindustry ################################

# Apontar a autenticação para o arquivo json
bq_auth(path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RAIS SQL/RAIS-66fc2cf2df60.json")

# Criar conexão com o BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  billing = "rais-301720",
  project = "basedosdados"
)



query_2 = "WITH rais_totemp AS (SELECT id_municipio, ano, COUNT(*) as total_emp
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_agroindust AS (
SELECT id_municipio, ano, COUNT(cnae_1) as agroindust
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '15130' OR
    cnae_1 LIKE '15148' OR
    cnae_1 LIKE '152%'  OR
    cnae_1 LIKE '153%'  OR
    cnae_1 LIKE '154%'  OR
    cnae_1 LIKE '155%'  OR
    cnae_1 LIKE '156%'  OR
    cnae_1 LIKE '157%'  OR
    cnae_1 LIKE '158%'  OR
    cnae_1 LIKE '159%'  OR
    cnae_1 LIKE '160%'  OR
    cnae_1 LIKE '171%'  OR
    cnae_1 LIKE '172%'  OR
    cnae_1 LIKE '173%'  OR
    cnae_1 LIKE '174%'  OR
    cnae_1 LIKE '175%'  OR
    cnae_1 LIKE '176%'  OR
    cnae_1 LIKE '177%'  OR
    cnae_1 LIKE '181%'  OR
    cnae_1 LIKE '182%'  OR
    cnae_1 LIKE '191%'  OR
    cnae_1 LIKE '192%'  OR
    cnae_1 LIKE '193%'  OR
    cnae_1 LIKE '201%'  OR
    cnae_1 LIKE '202%'  OR
    cnae_1 LIKE '211%'  OR
    cnae_1 LIKE '212%'  OR
    cnae_1 LIKE '213%'  OR
    cnae_1 LIKE '214%'
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
),
rais_industheavy AS (
SELECT id_municipio, ano, COUNT(cnae_1) as industheavy
FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
WHERE ano >= 1990 AND 
    cnae_1 LIKE '24%' OR
    cnae_1 LIKE '25%' OR
    cnae_1 LIKE '26%' OR
    cnae_1 LIKE '26%' OR
    cnae_1 LIKE '27%' OR
    cnae_1 LIKE '28%' OR
    cnae_1 LIKE '29%' OR
    cnae_1 LIKE '30%' OR
    cnae_1 LIKE '31%' OR
    cnae_1 LIKE '32%' OR
    cnae_1 LIKE '33%' OR
    cnae_1 LIKE '34%' OR  
    cnae_1 LIKE '26%' OR
    cnae_1 LIKE '35%' OR
    cnae_1 LIKE '36%' OR
    cnae_1 LIKE '37%'
GROUP BY id_municipio, ano
ORDER BY id_municipio, ano
)
SELECT *
FROM rais_totemp
FULL OUTER JOIN rais_agroindust
ON rais_totemp.id_municipio =rais_agroindust.id_municipio AND rais_totemp.ano = rais_agroindust.ano
FULL OUTER JOIN rais_industheavy
ON rais_totemp.id_municipio = rais_industheavy.id_municipio AND rais_totemp.ano =rais_industheavy.ano;"

rais_2 = dbGetQuery(con, query_2)

rais_2 %<>% dplyr::select(c("id_municipio", "ano", "total_emp", "agroindust", 
                          "industheavy")) %>%
  rename(cod = "id_municipio", year = "ano") %>% filter(year >= 1994) %>%
  arrange(cod)

# Replacing NA values with 0
rais_2[is.na(rais_2)] <- 0

rais_2 %<>% mutate(agroindust_rais = agroindust/total_emp, 
                    industheavy_rais = industheavy/total_emp)


# Merging with previous RAIS Dataset
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/rais_shares.RData")

rais_shares_2 <- full_join(rais_shares, dplyr::select(rais_2, - "total_emp"),
                             by = c("cod", "year"))


save(rais_shares_2, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/rais_shares_2.RData")
