library(haven)
library(tidyverse)
library(magrittr)

load("../../output/final/agro_struc.Rdata")
agro_struc %<>% filter(year != 1995) %<>% 
  dplyr::select(c(cod, year, municip, n_workers, total_area, n_tract, tot_num))

write_dta(agro_struc, "../../output/agri_2006_2017.dta")
