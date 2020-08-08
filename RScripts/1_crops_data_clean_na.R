
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")



########## 1. Read PAM crops data and check the number of municipalities that produced each crop in 1999 #############################################


# Here Dados_PAM_corr are the PAM excel files with some minor cleaning changes that I did "by hand"
crops_files <- list.files(path = 'C:/Users/Andrei/Desktop/Dissertation/Dados/Dados_PAM/Dados_PAM_corr', pattern = '*.xlsx', full.names = TRUE)
crops_files <- setNames(crops_files, crops_files)

# Making a list that includes all the files
crops_df <- map(crops_files, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
summary_crops <- map(crops_df, summary)

crops_names = c("banana", "barley", "cocoa", "coffee", "cotton", "indiantea", "maize", "orange", "rice", "rubber", "sorghum",
          "soybean", "sugarcane", "tobacco", "wheat", "yerbamate")

# Loop through the summary statistics and count the NAs; 5563 is the total number of municipalities observed in the PAM dataset
count <- 0
for (i in 1:18) { 

  count[i] <- 5563 - sum(is.na(crops_df[[i]]$"1999"))
  
}

count <- array(count, dim = length(count))

# Takes out the first type of cotton (arbóreo) and oatmeal that cannot be matched to the prices dataset
count <- count[-c(5,9)]

rownames(count) <- crops_names

count <- data.frame(count)

count <- as_tibble(count, rownames = NA)

crops_count <- rownames_to_column(count, var = "crops") %>%
  rename(num_munic = count) %>%
  arrange(desc(num_munic))

# Creates latex table using kable
table_na <- kable(crops_count, "latex", col.names = c("Crops", "Number of Municipalities"))

# Trying another package that seems better
table_na_another <- stargazer(crops_count, summary=FALSE, rownames=FALSE)






