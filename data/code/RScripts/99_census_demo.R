# Load packages
source("./00_load_packages.R")

### 1. Saving Census Microdata in .parquet format

census_1970 <- read_csv (file = here("data", "raw", "data_municipality", 
                                     "censo_demo", "1991",
                                     "microdados_pessoa_1991.csv"))

