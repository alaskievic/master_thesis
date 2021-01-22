# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

memory.limit(size = NA)
memory.limit(size = 50000)


######### 1. Reads cleaned RAIS Data for Municipalities and Calculates Shares ##


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

