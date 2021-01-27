# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

######### 1. Reads and cleans Municipalities GDP by sector  ####################

# GDPs
municip_pib_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_1.xlsx", 
                              skip = 3, sheet = "Produto Interno Bruto a preç...", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2002 %<>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip", "1999", "2000", "2001")) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)


municip_pib_agro_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_1.xlsx", 
                                 skip = 3, sheet = "Valor adicionado bruto a pre...", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_agro_2002 %<>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip",  "1999", "2000", "2001")) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)



municip_pib_indust_2002 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_2.xlsx", 
                                   skip = 3, sheet = "Tabela 1", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_indust_2002 %<>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip", "1999", "2000", "2001")) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)



municip_pib_serv_2002  <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_2.xlsx", 
                                  skip = 3, sheet = "Tabela 2", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_serv_2002 %<>% rename(cod = "...1", municip = "...2") %>%
  dplyr::select(c("cod", "municip",  "1999", "2000", "2001")) %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)




# 2010
municip_pib_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_novo_1.xlsx", 
                              skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_2010  %<>% rename(cod = "...1", municip = "...2") %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)


municip_pib_agro_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_novo_1.xlsx", 
                                 skip = 3, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_agro_2010 %<>% rename(cod = "...1", municip = "...2") %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)


municip_pib_indust_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_novo_2.xlsx", 
                                   skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_indust_2010 %<>% rename(cod = "...1", municip = "...2") %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  arrange(cod)


municip_pib_serv1_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_novo_2.xlsx", 
                                     skip = 3, sheet = 2, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


municip_pib_serv1_2010 %<>% rename(cod = "...1", municip = "...2") %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_serv") %>%
  arrange(cod)


municip_pib_serv2_2010 <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/PIB/Setores/pib_mun_novo_3.xlsx", 
           skip = 3, sheet = 1, col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

municip_pib_serv2_2010 %<>% rename(cod = "...1", municip = "...2") %>%
  slice(-(n())) %>%
  mutate(cod = as.integer(cod)) %>%
  pivot_longer(-c("cod", "municip"), names_to = "year", values_to = "pib_serv") %>%
  arrange(cod)


municip_pib_serv_2010 <-municip_pib_serv1_2010 %>%
  mutate(pib_serv = municip_pib_serv1_2010$pib_serv + municip_pib_serv2_2010$pib_serv)

municip_pib_serv_2010_wide <- pivot_wider(municip_pib_serv_2010, names_from = "year", 
                                     values_from = "pib_serv")


# Merging
municip_pib_tot <- full_join(dplyr::select(municip_pib_2002, -"municip"),
                             municip_pib_2010, by = "cod")

municip_pib_agro <- full_join(dplyr::select(municip_pib_agro_2002, -"municip"),
                              municip_pib_agro_2010, by = "cod")

municip_pib_indust <- full_join(dplyr::select(municip_pib_indust_2002, -"municip"),
                                municip_pib_indust_2010, by = "cod")

municip_pib_serv <- full_join(dplyr::select(municip_pib_serv_2002, -"municip"),
                              municip_pib_serv_2010_wide, by = "cod")


# Pivoting
municip_pib_tot %<>% pivot_longer(-c("cod", "municip"), names_to = "year",
                                  values_to = "pib_tot") %>% arrange(cod)

municip_pib_agro %<>% pivot_longer(-c("cod", "municip"), names_to = "year",
                                   values_to = "pib_agro") %>% arrange(cod)

municip_pib_indust %<>% pivot_longer(-c("cod", "municip"), names_to = "year",
                                     values_to = "pib_indust") %>% arrange(cod)

municip_pib_serv %<>% pivot_longer(-c("cod", "municip"), names_to = "year",
                                   values_to = "pib_serv") %>% arrange(cod)


# Another Merge
municip_pib <- full_join(municip_pib_tot, municip_pib_agro, by = c("cod", "year", "municip"))

municip_pib <- full_join(municip_pib, municip_pib_indust, by = c("cod", "year", "municip"))

municip_pib <- full_join(municip_pib, municip_pib_serv, by = c("cod", "year", "municip")) %>%
  arrange(cod)


## Deflating

# Reads IPCA for deflation
ipca <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Prices/ipca_anual.xls",
                   sheet = "Séries", col_names = TRUE, na = "")

ipca <- ipca %>% filter(Date >= 1999 & Date <= 2017)

deflate <- function(x) x/(ipca$Index_2/100)

municip_pib_real <- mutate_all(municip_pib[4:7], deflate) %>%
  add_column(cod = municip_pib$cod, municip = municip_pib$municip,
             year = municip_pib$year, .before = "pib_tot")

municip_pib_real <- municip_pib_real %>% mutate(cod = as.integer(cod)) %>%
  mutate(year = as.integer(year))



municip_pib_final <- municip_pib_real %>% filter(year >= 2000 & year <= 2015) %>%
  filter(cod>1)

save(municip_pib_final,
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/municip_pib_real.Rdata")



######### 2. GDP Shares Graph  #################################################


gdp_shares <- municip_pib_real %>% filter (cod==1) %>% 
  mutate(gdpagro_sh = pib_agro/pib_tot) %>%
  mutate(gdpindust_sh = pib_indust/pib_tot) %>%
  mutate(gdpserv_sh = pib_serv/pib_tot) %>%
  dplyr::select(cod, municip, year, gdpagro_sh, gdpindust_sh, gdpserv_sh) %>%
  pivot_longer(-c("cod", "municip", "year"), names_to = "Sector", values_to = "shares")


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


sh_graph <- ggplot(gdp_shares, aes(x=year)) +
  geom_line(data = gdp_shares, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing", "Services"))+
  geom_point(data = gdp_shares, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  theme_bw(base_size = 13) +
  theme(legend.text=element_text(size=13), legend.position = c(0.85, 0.6), 
        legend.box.background = element_blank(), legend.title = element_blank())

sh_graph
ggsave(filename = "empshares.png", plot = sh_graph, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")







