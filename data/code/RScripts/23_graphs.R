# Set Working Directory
setwd("C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/RScripts")

#Load packaages
source("./00_load_packages.R")

memory.limit(size = NA)
memory.limit(size = 50000)


######################## 1. Load Files from Statas #############################

pop_struc <- 
  read_dta(file = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/StataFiles/pop_struc_r.dta")


pop_struc %<>% dplyr::select(cod, year, municip, P_AGRO, P_INDUST, P_SERV, dfaoc95) %>%
  mutate(round = ifelse(year ==2000, "2000", "2010")) %>%
  mutate(tabov_med = ifelse(dfaoc95 > median(dfaoc95, na.rm = TRUE), 1, 0)) %>%
  mutate(tabov_med = na.locf(tabov_med, na.rm = F))   %>%
  mutate(tabov_med = ifelse(is.na(tabov_med), 0, tabov_med)) %>%
  mutate(tabov_med = factor(tabov_med))


pop_struc_agro <- pop_struc %>% group_by(tabov_med, round) %>%
               summarise_at("P_AGRO", list(~ mean(.), ~ sd(.))) %>%
               ungroup() %>%
               mutate(label = ifelse(tabov_med == 1, "Above Median \n Exposure", "Below Median \n Exposure"))

pop_struc_indust <- pop_struc %>% group_by(tabov_med, round) %>%
  summarise_at("P_INDUST", list(~ mean(.), ~ sd(.))) %>%
  ungroup() %>%
  mutate(label = ifelse(tabov_med == 1, "Above Median \n Exposure", "Below Median \n Exposure"))


pop_struc_serv <- pop_struc %>% group_by(tabov_med, round) %>%
  summarise_at("P_SERV", list(~ mean(.), ~ sd(.))) %>%
  ungroup() %>%
  mutate(label = ifelse(tabov_med == 1, "Above Median \n Exposure", "Below Median \n Exposure"))



library(directlabels)
library(plotrix)

agro <- ggplot(pop_struc_agro, aes(x = factor(round), y = mean, group = tabov_med, colour = tabov_med)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.03)) + 
  geom_dl(aes(label = label), method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_line(position = position_dodge(width = 0.03)) +
  theme_classic() +
  scale_x_discrete(labels = c("2000", "2010")) +
  ylab("") +
  xlab("") +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "none"
  )

agro

ggsave(filename = "diff_agrsh.png", plot = agro, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")


indust <- ggplot(pop_struc_indust, aes(x = factor(round), y = mean, group = tabov_med, colour = tabov_med)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.03)) + 
  geom_dl(aes(label = label), method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_line(position = position_dodge(width = 0.03)) +
  theme_classic() +
  scale_x_discrete(labels = c("2000", "2010")) +
  ylab("") +
  xlab("") +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "none"
  )


indust

ggsave(filename = "diff_manush.png", plot = indust, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")

serv <- ggplot(pop_struc_serv, aes(x = factor(round), y = mean, group = tabov_med, colour = tabov_med)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.03)) + 
  geom_dl(aes(label = label), method = list(dl.trans(x = x + 0.2), "last.points")) +
  geom_line(position = position_dodge(width = 0.03)) +
  theme_classic() +
  scale_x_discrete(labels = c("2000", "2010")) +
  ylab("") +
  xlab("") +
  theme_bw(base_size = 16) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "none"
  )

serv

ggsave(filename = "diff_servsh.png", plot = serv, 
       path = "C:/Users/Andrei/Desktop/Dissertation/Analysis/master_thesis/Figures")

library(cowplot)

# 
# title <- ggdraw() + 
#   draw_label(
#     "Employment Shares (Mean and 1SD Error Bars)",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 7)
#   )

grid <- plot_grid(agro, indust, serv)

grid











