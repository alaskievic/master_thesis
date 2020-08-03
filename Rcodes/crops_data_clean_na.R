library(tidyverse)
library(readxl)


setwd("C:/Users/Andrei/Desktop/Dissertation/Dados")

crops_files <- list.files(path = 'C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM_corr', pattern = '*.xlsx', full.names = TRUE)
crops_files <- setNames(crops_files, crops_files)

crops_df <- map(crops_files, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
summary_crops <- map(crops_df, summary)

crops_names = c("banana", "barley", "cocoa", "coffee", "cotton", "indiantea", "maize", "orange", "rice", "rubber", "sorghum",
          "soybean", "sugarcane", "tobacco", "wheat", "yerbamate")

type_convert(summary_crops[[1]][7,17], na.strings = "NA")
crops_strings <- map(summary_crops[[x]][7,17], str_sub, start = 9, end = 12)


a <- str_sub(summary_crops[[1]][7,17], start = 9, end = 12)

b <- type.convert(a)

count <- 0
for (i in 1:18) { 

  count[i] <- 5563 - sum(is.na(crops_df[[i]]$"1999"))
  
}

count <- array(count, dim = length(count))
count <- count[-c(5,9)]

rownames(count) <- crops_names

count <- data.frame(count)

count2 <- as_tibble(count, rownames = NA)

crops_count <- rownames_to_column(count2, var = "crops") %>%
  rename(Crops = crops, Number = count) %>%
  arrange(desc(Number))




