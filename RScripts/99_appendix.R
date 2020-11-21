# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts")

#Load packaages
source("./0_load_packages.R")




#install.packages("devtools")
#devtools::install_github("lucasmation/microdadosBrasil")

library('microdadosBrasil')




# Censo Demográfico 2000
download_sourceData("CENSO", 2000, unzip = T)
d <- read_CENSO('domicilios', 2000)



d <- read_CENSO('pessoas', 2000)


### Graphs

fao_pr <- read.dta13("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/StataFiles/fao_pr.dta")


# Banana
banana <- ggplot(data = fao_pr, aes(x = pr_banana, y = banana)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

banana


soybean <- ggplot(data = fao_pr, aes(x = pr_soybean, y = soybean)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

soybean


# Full measure
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/fao_final.Rdata")
load("C:/Users/Andrei/Desktop/Dissertation/Dados/master_thesis/RScripts/pq_bartik_final.Rdata")


library(unikn)

# (a) Vector of colors (as RGB values):
o_i_colors <- c(rgb(  0,   0,   0, maxColorValue = 255),  # black
                rgb(230, 159,   0, maxColorValue = 255),  # orange
                rgb( 86, 180, 233, maxColorValue = 255),  # skyblue
                rgb(  0, 158, 115, maxColorValue = 255),  # green
                rgb(240, 228,  66, maxColorValue = 255),  # yellow
                rgb(  0, 114, 178, maxColorValue = 255),  # blue
                rgb(213,  94,   0, maxColorValue = 255),  # vermillion
                rgb(204, 121, 167, maxColorValue = 255)   # purple
)

# (b) Vector of color names:
o_i_names <- c("black", "orange", "skyblue", "green", "yellow", "blue", "vermillion", "purple")

# (c) Use newpal() to combine colors and names:
pal_okabe_ito <- newpal(col = o_i_colors,
                        names = o_i_names)




scatter <- full_join(fao_final, pq_bartik_final, by = c("cod", "year"))



fao_scatter <- ggplot(data = scatter, aes(x = log(sum_fao), y = log(pq_shares))) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  theme_bw()

fao_scatter


fao_scatter <- ggplot(data = scatter, aes(x = sum_fao, y = pq_shares)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  theme_bw()

fao_scatter








