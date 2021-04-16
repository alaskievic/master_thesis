# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")




######### 1. Matching AMCs and Municipality Codes ###############################


amc_ibge <- read_excel("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/Municipios_X_AMCs_1872_1997.xls", 
                        sheet = "Municípios_X_AMCs", col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))


amc_ibge %<>% dplyr::select(UFMUNDV, NOMEMUN, CODAMC1991_1997, NEW_CODE_1991_1997, 
                            NOME_1991_1997) %>% setnames(c("cod", "name_amcom", 
                                                         "old_amc", "amc", "name_amc"))

thiago <- read_csv("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/controls1991amc.csv",
                     col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

thiago %<>% dplyr::select(-X1)


# Matching controls and amc

amc_thiago <- inner_join(amc_ibge, thiago, by = "amc")


# Matching with AMC codes from BD+

bd_amc <- read_csv("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Código Municípios/AMC/bd_amc.csv",
                   col_names = TRUE, na = c("NA","N/A","", "...", "-", "..", "X"))

# Filter only the desired periods

bd_amc_1991 <- bd_amc %>% filter(ano_de == 1991 & ano_para == 2010)

bd_amc_1991 %<>% rename(cod = id_municipio)


amc_final <- full_join(amc_thiago, bd_amc_1991, by = "cod")


save(amc_final,
          file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/amc_final.Rdata")






######### 2. Scatterbin plots ##################################################


scatter <- read_dta(file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc.dta")

fao_scatter <- ggplot(data = scatter, aes(x = cattle_1995, y = pr_cattle_1995)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  theme_bw()

fao_scatter


fao_scatter <- ggplot(data = scatter, aes(x = banana_1995, y = pr_banana_1995)) + 
  geom_point(color='blue') +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point') +
  theme_bw()

fao_scatter

fao_scatter <- binsreg(x = scatter$cattle_1995, y = scatter$pr_cattle_1995, line = c(3,3))
fao_scatter <- binsreg(x = scatter$pr_cattle_1995, y = scatter$cattle_1995, line = c(3,3))


p1 <- binsreg(x = scatter$pr_banana_1995, y = scatter$banana_1995)
p2 <- binsreg(x = scatter$pr_barley_1995, y = scatter$barley_1995)
p3 <- binsreg(x = scatter$pr_cattle_1995, y = scatter$cattle_1995)
p4 <- binsreg(x = scatter$pr_cocoa_1995, y = scatter$cocoa_1995)
p5 <- binsreg(x = scatter$pr_coffee_1995, y = scatter$coffee_1995)
p6 <- binsreg(x = scatter$pr_cotton_1995, y = scatter$maize_1995)
p7 <- binsreg(x = scatter$pr_maize_1995, y = scatter$rice_1995)
p8 <- binsreg(x = scatter$pr_orange_1995, y = scatter$orange_1995)
p9 <- binsreg(x = scatter$pr_rice_1995, y = scatter$rice_1995)
p10 <- binsreg(x = scatter$pr_sorghum_1995, y = scatter$sorghum_1995)
p11 <- binsreg(x = scatter$pr_soybean_1995, y = scatter$soybean_1995)
p12 <- binsreg(x = scatter$pr_sugarcane_1995, y = scatter$sugarcane_1995)
p13 <- binsreg(x = scatter$pr_tea_1995, y = scatter$tea_1995)
p14 <- binsreg(x = scatter$pr_tobacco_1995, y = scatter$tobacco_1995)
p15 <- binsreg(x = scatter$pr_wheat_1995, y = scatter$wheat_1995)


install.packages("cowplot")
library(cowplot)


plot_grid(p1$bins_plot, p2$bins_plot, p3$bins_plot, p4$bins_plot, p5$bins_plot, 
          p6$bins_plot, p7$bins_plot,p8$bins_plot,p9$bins_plot,p10$bins_plot,
          p11$bins_plot, p12$bins_plot, p13$bins_plot, p14$bins_plot, p15$bins_plot)





scatter <- read_dta(file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc.dta")


pred_fao <- ggplot(scatter, aes(x = pr_cattle_1995, y = cattle_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
               formula = y~x , 
               parse = TRUE,
               label.y = 0.2,
               label.x = 0.9)+
  theme_bw(base_size = 13) +
  labs(x = "Predicted Exposure to Ag. Price Shocks", y = "Actual Exposure to Ag. Price Shocks") + 
  annotate("text", x = 7.5, y = 0.65, 
           label = "Mean = 6.611 \n Median = 6.630\n SD = 0.451",
           size = 5)

pred_fao

# Banana
pred_banana <- ggplot(scatter, aes(x = pr_banana_1995, y = banana_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 13) +
  labs(x = "Predicted Banana Share", y = "Pre-Period Banana Share") + 
  annotate("text", x = 0.55, y = 0.3, 
           label = "Mean = 6.611 \n Median = 6.630\n SD = 0.451",
           size = 5)

pred_banana

model <- lm(banana_1995 ~ pr_banana_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


# Cattle
pred_cattle <- ggplot(scatter, aes(x = pr_cattle_1995, y = cattle_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 13) +
  labs(x = "Predicted Cattle Share", y = "Pre-Period Cattle Share") + 
  annotate("text", x = 0.7, y = 0.3, 
           label = "Coefficient = 1.01 \n Robust s.e. = 0.02 \n t-statistic = 53.56",
           size = 5)

pred_cattle

model <- lm(cattle_1995 ~ pr_cattle_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))




install.packages("lmtest")
install.packages("sandwich")

library("lmtest")
library("sandwich")

model <- lm(cattle_1995 ~ pr_cattle_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))




histo <- hist %>% ggplot( aes(x=sum_fao_cattle_1995)) +
  geom_histogram(aes( y= ..density..), bins = 50, color = "blue", fill = "lightblue") +
  geom_vline(aes(xintercept = mean(sum_fao_cattle_1995)), color = "blue",
             linetype = "dashed", size = 1) +
  theme_bw(base_size = 16) + 
  scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Commodity Exposure Measure") +
  geom_density(colour = "#003399", size = 1) +
  annotate("text", x = 7.5, y = 0.65, 
           label = "Mean = 6.611 \n Median = 6.630\n SD = 0.451",
           size = 5)
