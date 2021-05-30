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


############# 3. Additional Municipality and AMCs Controls #####################
load("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/amc_final.Rdata")

past <-
  read_dta("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Demo/Outros/past_controls.dta")

past_share <- 
  read_dta("C:/Users/Andrei/Desktop/Dissertation/Analysis/Dados Municípios/Censo Demo/1991/empshares_1991.dta")

past_share %<>% rename(cod = id_municipio) %>% mutate(year = 1991)

amc_final %<>% rename(cod_amc = amc)

amc_full <- inner_join(amc_final, past, by = "cod_amc")

save(amc_full, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/amc_full.Rdata")

save(past_share, 
     file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Final Datasets/past_share.Rdata")

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





scatter <- 
  read_dta(file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc.dta")


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
  annotate("text", x = 0.75, y = 0.65, 
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










################################################################################

# Cattle
install.packages("lmtest")
install.packages("sandwich")

library("lmtest")
library("sandwich")

model <- lm(cattle_1995 ~ pr_cattle_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_cattle <- ggplot(scatter, aes(x = pr_cattle_1995, y = cattle_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Cattle Share", y = "Pre-Period Cattle Share") + 
  annotate("text", x = 0.69, y = 0.3, 
           label = "Coefficient = 1.01 \n Robust s.e. = 0.02 \n t-statistic = 53.56",
           size = 5)

pred_cattle


ggsave(filename = "pred_cattle.png", plot = pred_cattle, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")




# Banana
model <- lm(banana_1995 ~ pr_banana_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_banana <- ggplot(scatter, aes(x = pr_banana_1995, y = banana_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Banana Share", y = "Pre-Period Banana Share") + 
  annotate("text", x = 0.7, y = 0.3, 
           label = "Coefficient = 1.13 \n Robust s.e. = 0.08\n t-statistic = 14.67",
           size = 5)

pred_banana


ggsave(filename = "pred_banana.png", plot = pred_banana, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")



# Barley
model <- lm(barley_1995 ~ pr_barley_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_barley <- ggplot(scatter, aes(x = pr_barley_1995, y = barley_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Barley Share", y = "Pre-Period Barley Share") + 
  annotate("text", x = 0.07, y = 0.02, 
           label = "Coefficient = 0.81 \n Robust s.e. = 0.12 \n t-statistic = 6.89",
           size = 5)

pred_barley


ggsave(filename = "pred_barley.png", plot = pred_barley, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")






# Orange
model <- lm(orange_1995 ~ pr_orange_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_orange <- ggplot(scatter, aes(x = pr_orange_1995, y = orange_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Orange Share", y = "Pre-Period Orange Share") + 
  annotate("text", x = 0.43, y = 0.2, 
           label = "Coefficient = 1.04 \n Robust s.e. = 0.03 \n t-statistic = 29.4",
           size = 5)

pred_orange


ggsave(filename = "pred_orange.png", plot = pred_orange, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")




# Cocoa
model <- lm(cocoa_1995 ~ pr_cocoa_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_cocoa <- ggplot(scatter, aes(x = pr_cocoa_1995, y = cocoa_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Cocoa Share", y = "Pre-Period Cocoa Share") + 
  annotate("text", x = 0.65, y = 0.25, 
           label = "Coefficient = 0.98 \n Robust s.e. = 0.06 \n t-statistic = 15.7",
           size = 5)

pred_cocoa


ggsave(filename = "pred_cocoa.png", plot = pred_cocoa, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")






# Coffee
model <- lm(coffee_1995 ~ pr_coffee_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_coffee <- ggplot(scatter, aes(x = pr_coffee_1995, y = coffee_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Coffee Share", y = "Pre-Period Coffee Share") + 
  annotate("text", x = 0.45, y = 0.2, 
           label = "Coefficient = 0.91 \n Robust s.e. = 0.07 \n t-statistic = 13.6",
           size = 5)

pred_coffee


ggsave(filename = "pred_coffee.png", plot = pred_coffee, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")









# Cotton
model <- lm(cotton_1995 ~ pr_cotton_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_cotton <- ggplot(scatter, aes(x = pr_cotton_1995, y = cotton_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Cotton Share", y = "Pre-Period Cotton Share") + 
  annotate("text", x = 0.075, y = 0.15, 
           label = "Coefficient = 0.98 \n Robust s.e. = 0.08 \n t-statistic = 12.7",
           size = 5)

pred_cotton


ggsave(filename = "pred_cotton.png", plot = pred_cotton, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")









# Maize
model <- lm(maize_1995 ~ pr_maize_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_maize <- ggplot(scatter, aes(x = pr_maize_1995, y = maize_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Maize Share", y = "Pre-Period Maize Share") + 
  annotate("text", x = 0.57, y = 0.25, 
           label = "Coefficient = 1.05 \n Robust s.e. = 0.02 \n t-statistic = 65.5",
           size = 5)

pred_maize


ggsave(filename = "pred_maize.png", plot = pred_maize, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")








# Rice
model <- lm(rice_1995 ~ pr_rice_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_rice <- ggplot(scatter, aes(x = pr_rice_1995, y = rice_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Rice Share", y = "Pre-Period Rice Share") + 
  annotate("text", x = 0.58, y = 0.25, 
           label = "Coefficient = 1.02 \n Robust s.e. = 0.03 \n t-statistic = 34.3",
           size = 5)

pred_rice


ggsave(filename = "pred_rice.png", plot = pred_rice, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")










# Sorghum
model <- lm(sorghum_1995 ~ pr_sorghum_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_sorghum <- ggplot(scatter, aes(x = pr_sorghum_1995, y = sorghum_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Sorghum Share", y = "Pre-Period Sorghum Share") + 
  annotate("text", x = 0.06, y = 0.15, 
           label = "Coefficient = 0.96 \n Robust s.e. = 0.18 \n t-statistic = 5.44",
           size = 5)

pred_sorghum


ggsave(filename = "pred_sorghum.png", plot = pred_sorghum, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")








# Soybean
model <- lm(soybean_1995 ~ pr_soybean_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_soybean <- ggplot(scatter, aes(x = pr_soybean_1995, y = soybean_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Soybean Share", y = "Pre-Period Soybean Share") + 
  annotate("text", x = 0.4, y = 0.25, 
           label = "Coefficient = 1.15 \n Robust s.e. = 0.02 \n t-statistic = 50.3",
           size = 5)

pred_soybean


ggsave(filename = "pred_soybean.png", plot = pred_soybean, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")







# Sugarcane
model <- lm(sugarcane_1995 ~ pr_sugarcane_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_sugarcane <- ggplot(scatter, aes(x = pr_sugarcane_1995, y = sugarcane_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Sugarcane Share", y = "Pre-Period Sugarcane Share") + 
  annotate("text", x = 0.75, y = 0.25, 
           label = "Coefficient = 1.03 \n Robust s.e. = 0.02 \n t-statistic = 48.7",
           size = 5)

pred_sugarcane


ggsave(filename = "pred_sugarcane.png", plot = pred_sugarcane, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")








# Tea
model <- lm(tea_1995 ~ pr_tea_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_tea <- ggplot(scatter, aes(x = pr_tea_1995, y = tea_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Tea Share", y = "Pre-Period Tea Share") + 
  annotate("text", x = 0.15, y = 0.3, 
           label = "Coefficient = 1.06 \n Robust s.e. = 0.20 \n t-statistic = 5.2",
           size = 5)

pred_tea


ggsave(filename = "pred_tea.png", plot = pred_tea, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")






# Tobacco
model <- lm(tobacco_1995 ~ pr_tobacco_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_tobacco <- ggplot(scatter, aes(x = pr_tobacco_1995, y = tobacco_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Tobacco Share", y = "Pre-Period Tobacco Share") + 
  annotate("text", x = 0.1, y = 0.3, 
           label = "Coefficient = 1.13 \n Robust s.e. =  0.06 \n t-statistic = 17.5",
           size = 5)

pred_tobacco


ggsave(filename = "pred_tobacco.png", plot = pred_tobacco, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")








# Wheat
model <- lm(wheat_1995 ~ pr_wheat_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_wheat <- ggplot(scatter, aes(x = pr_wheat_1995, y = wheat_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Wheat Share", y = "Pre-Period Wheat Share") + 
  annotate("text", x = 0.12, y = 0.05, 
           label = "Coefficient = 1.03 \n Robust s.e. =  0.03 \n t-statistic = 34.7",
           size = 5)

pred_wheat


ggsave(filename = "pred_wheat.png", plot = pred_wheat, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")





# Measure
model <- lm(pq_sum ~ sum_fao_cattle_1995, data = scatter)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


pred_wheat <- ggplot(scatter, aes(x = pr_wheat_1995, y = wheat_1995)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Predicted Wheat Share", y = "Pre-Period Wheat Share") + 
  annotate("text", x = 0.12, y = 0.05, 
           label = "Coefficient = 1.03 \n Robust s.e. =  0.03 \n t-statistic = 34.7",
           size = 5)

pred_wheat


ggsave(filename = "pred_wheat.png", plot = pred_wheat, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")







test <- plot_grid(pred_banana, pred_barley, pred_cattle, pred_cocoa, pred_coffee, pred_cotton, 
          pred_maize, pred_orange, pred_rice, pred_sorghum, pred_soybean, pred_sugarcane, 
          pred_tea, pred_tobacco, pred_wheat)


test






pop_struc <- 
  read_dta(file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc_r.dta")


pop_struc %<>% filter(year == 2010) %>% filter(dshares != 0)

model <- lm(dshares ~ dfaoc95, data = pop_struc)
summary(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))




pred_measure <- ggplot(pop_struc, aes(x = dfaoc95, y = dshares)) + 
  geom_point(shape=19, color="blue", stat = "unique") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred", lwd = 1.2) +
  theme_bw(base_size = 16) +
  labs(x = "Change in Predicted Exposure Measure", 
       y = "Change in Actual Exposure Measure") + 
  annotate("text", x = 0.8, y = 0.55, 
           label = "Coefficient =  0.67 \n Robust s.e. =  0.02 \n t-statistic = 32.8",
           size = 5)

pred_measure


ggsave(filename = "pred_measure.png", plot =pred_measure, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")






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
