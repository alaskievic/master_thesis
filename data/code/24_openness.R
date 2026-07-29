#Load packages
source("00_load_packages.R")

############ 1. Calculate Exports and Imports over Total GDP  ##################

# Exports
mun_exports <- read_excel(here("data", "raw", "data_municipality",
                               "ipea", "exports_imports", "export_mun.xls"), 
                               sheet = "Séries",
                               col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

mun_imports <- read_excel(here("data", "raw", "data_municipality",
                               "ipea", "exports_imports", "import_mun.xls"), 
                          sheet = "Séries",
                          col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


# colnames(mun_exports)[-(1:3)] <- paste("export", colnames(mun_exports)[-(1:3)], sep = "_")
# colnames(mun_imports)[-(1:3)] <- paste("import", colnames(mun_imports)[-(1:3)], sep = "_")


mun_exports %<>% dplyr::select(-"Sigla") %>% rename(cod = "Codigo", municip = "Município") %>%
  mutate(cod = as.integer(cod)) %>% replace(is.na(.), 0)


mun_imports %<>% dplyr::select(-"Sigla") %>% rename(cod = "Codigo", municip = "Município") %>%
  mutate(cod = as.integer(cod)) %>% replace(is.na(.), 0)



# Passing to longer
mun_exports %<>% pivot_longer(-c("cod", "municip"), values_to = "export_value", 
                              names_to = "year") %>% mutate(year = as.integer(year)) %>%
  filter(year >= 2000 & year <= 2010)

mun_imports %<>% pivot_longer(-c("cod", "municip"), values_to = "import_value", 
                              names_to = "year") %>% mutate(year = as.integer(year)) %>%
  filter(year >= 2000 & year <= 2010)


## Convert Values to Reais
# Reads BRL(R$)/US$ exchange rate data
cambio <- read_excel(here("data", "raw", "prices", "cambio_nominal.xls"),
                     sheet = "Séries", col_names = TRUE, na = "") %>%
  filter(Date >= 2000 &  Date <= 2010)


convert <- function(x) (x*cambio$Cambio/1000)

mun_exports <- mutate_all(mun_exports[4], convert) %>%
  add_column(cod  = mun_exports$cod, municip = mun_exports$municip,
             year = mun_exports$year, .before = "export_value")

mun_imports <- mutate_all(mun_imports[4], convert) %>%
  add_column(cod  = mun_imports$cod, municip = mun_imports$municip,
             year = mun_imports$year, .before = "import_value")

## Deflate values
# Reads IPCA for deflation
ipca <- read_excel(here("data", "raw", "prices", "ipca_anual.xls"),
                   sheet = "Séries", col_names = TRUE, na = "")

ipca <- ipca %>% filter(Date >= 2000 & Date <= 2010)

deflate <- function(x) x/(ipca$Index_2/100)

mun_exports_real <- mutate_all(mun_exports[4], deflate) %>%
  add_column(cod  = mun_exports$cod, municip = mun_exports$municip,
             year = mun_exports$year, .before = "export_value")

mun_imports_real <- mutate_all(mun_imports[4], deflate) %>%
  add_column(cod  = mun_imports$cod, municip = mun_imports$municip,
             year = mun_imports$year, .before = "import_value")



# Calculate average over 2000-2010
mun_exports_real %<>% group_by(cod) %>% mutate(export_mean = mean(export_value)) %>%
  ungroup()

mun_imports_real %<>% group_by(cod) %>% mutate(import_mean = mean(import_value)) %>%
  ungroup()


# Load GDP data and calculate average total GDP between 2000 and 2010
load(here("data", "output", "final", "municip_pib_real.RData"))

municip_pib_final %<>% filter(year >= 2000 & year <= 2010)

municip_pib_final %<>% group_by(cod) %>% mutate(pibtot_mean = mean(pib_tot)) %>%
  mutate(pibagro_mean = mean(pib_agro)) %>% ungroup()


# Join everything
openness <- full_join(mun_exports_real, dplyr::select(mun_imports_real, -"municip"),
                                               by = c("cod", "year")) %>%
  full_join(., dplyr::select(municip_pib_final, - "municip"), by = c("cod", "year"))


# Calculate measures of openness to trade
openness %<>% mutate(open_exp = export_value/pib_tot) %>% 
              mutate(open_imp = import_value/pib_tot) %>%
              mutate(open_total = (export_value + import_value)/pib_tot) %>%
              mutate(open_exp_mean = export_mean/pibtot_mean) %>%
              mutate(open_imp_mean = import_mean/pibtot_mean) %>%
              mutate(open_total_mean = (export_mean + import_mean)/pibtot_mean) %>%
              mutate(agrogdp_meanshare = pibagro_mean/pibtot_mean)


save(openness, file = here("data", "output", "final", "openness.RData"))
