# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")

memory.limit(size = NA)
memory.limit(size = 50000)


######### 1. Using IPUMS Cata to Construct Employment Shares for Brazil ########


### 1960

# Reads ipums data
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1960.xml",
                           data_file = "C:/Users/Andrei/Desktop//Dissertation/Analysis/IPUMS/CSV/brazil_1960.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

# Reads as an survey object (using individual weights)
brazil %<>% as_survey_design(weights = PERWT)

# Cleans ipums labels
brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))

# Restrict the dataset to employed individuals
brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)


# Displays harmonized occupational sectors
ipums_val_labels(brazil$variables$INDGEN)

# Cleans ipums labels
#brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))

 
# brazil %<>%
#   mutate(INDGEN = as_factor(ifelse(INDGEN == "Agriculture, fishing, and forestry",
#                                    1, 0)))

# Calculate employment shares for each sector
occ_1960 <- table(brazil$variables$INDGEN)
share_1960 <- prop.table(occ_1960) %>% as.data.frame() %>% as_tibble()




### 1970
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1970.xml",
                           data_file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1970.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))

brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1970 <- table(brazil$variables$INDGEN)
share_1970 <- prop.table(occ_1970) %>% as.data.frame() %>% as_tibble()


### 1980
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1980.xml",
                           data_file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1980.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1980 <- table(brazil$variables$INDGEN)
share_1980 <- prop.table(occ_1980) %>% as.data.frame() %>% as_tibble()



### 1991
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1991.xml",
                           data_file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_1991.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_1991 <- table(brazil$variables$INDGEN)
share_1991 <- prop.table(occ_1991) %>% as.data.frame() %>% as_tibble()



### 2000
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_2000.xml",
                           data_file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_2000.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))


occ_2000 <- table(brazil$variables$INDGEN)
share_2000 <- prop.table(occ_2000) %>% as.data.frame() %>% as_tibble()



### 2010
brazil <- read_ipums_micro(ddi = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_2010.xml",
                           data_file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/IPUMS/CSV/brazil_2010.csv",
                           vars = c("PERWT", "EMPSTAT", "INDGEN"), verbose = FALSE)

brazil %<>% as_survey_design(weights = PERWT)

brazil %<>% mutate(EMPSTAT = as_factor(lbl_clean(EMPSTAT)))


brazil %<>% 
  mutate(EMPSTAT = as_factor(ifelse(EMPSTAT == "Employed", 1, 0))) %>% filter(EMPSTAT == 1)

brazil %<>% mutate(INDGEN = as_factor(lbl_clean(INDGEN)))

occ_2010 <- table(brazil$variables$INDGEN)
share_2010 <- prop.table(occ_2010) %>% as.data.frame() %>%as_tibble()


# Joining

share_1960 %<>% mutate(Var1 = c("Agriculture, fishing, and forestry", 
                                  "Mining and extraction", 
                                  "Manufacturing",
                                  "Electricity, gas, water and waste management",
                                  "Construction", "Wholesale and retail trade", 
                                  "Hotels and restaurants", 
                                  "Transportation, storage, and communications", 
                                  "Financial services and insurance",
                                  "Public administration and defense",
                                  "Business services and real estate", 
                                  "Education", "Health and social work", 
                                  "Other services", "Private household services", 
                                  "Unknown"))

share_1970 %<>% slice(-1)

share_1960 %<>% slice_head(n=15) %>% rename(sh_1960 = "Freq")
share_1970 %<>% slice_head(n=15) %>% rename(sh_1970 = "Freq")
share_1980 %<>% slice_head(n=15) %>% rename(sh_1980 = "Freq")
share_1991 %<>% slice_head(n=15) %>% rename(sh_1991 = "Freq")
share_2000 %<>% slice_head(n=15) %>% rename(sh_2000 = "Freq")
share_2010 %<>% slice_head(n=15) %>% rename(sh_2010 = "Freq")


share_final <- full_join(share_2000, share_2010, by = "Var1")
share_final <- full_join(share_final, share_1991, by = "Var1")
share_final <- full_join(share_final, share_1980, by = "Var1")
share_final <- full_join(share_final, share_1970, by = "Var1")
share_final <- full_join(share_final, share_1960, by = "Var1")


share_final %<>% set_names(c("sector", "2000", "2010", "1991", "1980", "1970",
                             "1960"))

share_final %<>% pivot_longer(-"sector", names_to = "year", values_to = "shares")

share_final %<>% arrange(year)

save(share_final, 
     file = "C:/Users/Andrei/Desktop/share_final.RData")


################ 2. Making a Graph #############################################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/share_final.RData")

# Summing up service sectors

share_final %<>% pivot_wider(names_from = "sector", values_from = "shares")

share_final %<>% rename(Agriculture = "Agriculture, fishing, and forestry", 
                        Industry = "Manufacturing") %>%
  mutate(year = as.integer(year))


share_final %<>% group_by(year) %>%
  mutate(Manufacturing = Industry + `Mining and extraction`) %>%
  mutate("Manufacturing + Construction" = Manufacturing + `Construction`) %>%
  mutate(Services = `Wholesale and retail trade` + `Hotels and restaurants` + 
           `Transportation, storage, and communications` + 
           `Financial services and insurance` + 
           `Public administration and defense` + 
           `Business services and real estate` + 
           `Education` + `Health and social work` + `Other services` + 
           `Private household services`) %>%
  mutate(Services2 = Services + `Electricity, gas, water and waste management`)

share_final %<>% pivot_longer(-"year", names_to = "sectors",
                              values_to = "shares")


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

share_final %<>% filter(sectors == "Agriculture"| sectors == "Manufacturing"|
                           sectors == "Manufacturing + Construction" |
                          sectors == "Services") %>%
  rename(Sector = sectors)


devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
ggthemr('fresh')

sh_graph <- ggplot(share_final, aes(x=year)) +
  geom_line(data = share_final, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing",
                                "Manufacturing + \nConstruction", "Services"))+
  geom_point(data = share_final, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Employment Shares in Brazil (IPUMS Census Data)") +
  theme(legend.text=element_text(size=13))

sh_graph <- ggplot(share_final, aes(x=year)) +
  geom_line(data = share_final, aes(y=shares, color = Sector, group = Sector), lwd = 1)+
  scale_color_manual(values=c25, 
                     labels = c("Agriculture", "Manufacturing",
                                "Manufacturing + \nConstruction", "Services"))+
  geom_point(data = share_final, aes(x=year, y=shares, color = Sector), size=2)+
  labs(x = "Year", y = "Employment Shares") +
  ggtitle("Employment Shares in Brazil (IPUMS Census Data)") +
  theme(legend.text=element_text(size=13))+
  theme_bw(base_size = 13)

sh_graph
ggsave(filename = "empshares.png", plot = sh_graph, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")


