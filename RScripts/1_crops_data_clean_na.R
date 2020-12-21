
# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")



########## 1. Read PAM crops data and check the number of municipalities that produced each crop in 1999 #############################################


# Here Dados_PAM_corr are the PAM excel files with some minor cleaning changes that I did "by hand"
crops_files <- list.files(path = 'C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados_PAM/Dados_PAM_corr', pattern = '*.xlsx', full.names = TRUE)
crops_files <- setNames(crops_files, crops_files)

# Making a list that includes all the files
crops_df <- map(crops_files, read_excel, sheet = "Tabela", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))
summary_crops <- map(crops_df, summary)

crops_names = c("banana", "barley", "cattle", "cocoa", "coffee", "cotton_1", "cotton_2", "indiantea", "maize", "oatmeal", "orange", "rice", "rubber", "sorghum",
          "soybean", "sugarcane", "tobacco", "wheat", "yerbamate")

# Loop through the summary statistics and count the NAs; 5563 is the total number of municipalities observed in the PAM dataset
count_1999 <- 0
count_2000 <- 0
count_2010 <- 0
for (i in 1:19) {
  
  count_1999[i] <- 5563 - sum(is.na(crops_df[[i]]$"1999"))
  count_2000[i] <- 5563 - sum(is.na(crops_df[[i]]$"2000"))
  count_2010[i] <- 5563 - sum(is.na(crops_df[[i]]$"2010"))
}

count_1999 <- array(count_1999, dim = length(count_1999))
count_2000 <- array(count_2000, dim = length(count_2000))
count_2010 <- array(count_2010, dim = length(count_2010))

# Takes out the first type of cotton (arbóreo) and oatmeal that cannot be matched to the prices dataset
#count_2000 <- count[-c(5,9)]

rownames(count_1999) <- crops_names
rownames(count_2000) <- crops_names
rownames(count_2010) <- crops_names

count_1999 <- data.frame(count_1999)
count_2000 <- data.frame(count_2000)
count_2010 <- data.frame(count_2010)



count_1999 <- as_tibble(count_1999, rownames = NA)
count_2000 <- as_tibble(count_2000, rownames = NA)
count_2010 <- as_tibble(count_2010, rownames = NA)


total_count <- bind_cols(count_1999, count_2000)
total_count <- bind_cols(total_count, count_2010)


final_count <- rownames_to_column(total_count, var = "crops")





# Creates latex table using kable
table_na <- kable(crops_count, "latex", col.names = c("Crops", "Number of Municipalities"))

# Trying another package that seems better
table_na_another <- stargazer(crops_count, summary=FALSE, rownames=FALSE)






