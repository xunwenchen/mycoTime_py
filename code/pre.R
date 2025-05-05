# load packages ----
rm(list = ls())
library(dplyr)
library(ggplot2)
library(agricolae)
library(ggpubr)
library(vegan)
library(rEDM)
library(igraph)
library(quantreg)



# source own functions if any ----
source("code/fun.R")
# set ggplot theme ----
theme_set(theme_bw())
# set sequential colors for plotting ----
two_colors <- c("darkgray", "indianred")
four_colors <- c("darkgray", "indianred", "lightblue", "lightgreen")

save <- TRUE # set to 'FALSE' if you don't want to save the plots

# load data ----
data <- read.csv("data/combined_data.csv") # replace own data here
# set level of c('NM', 'AM') for myco as factor
data$myco <- factor(data$myco, levels = c('NM', 'AM'))
# set level of c(0, 2, 5, 15) for stress as factor
data$stress <- factor(data$stress, levels = c(0, 2, 5, 15))

# see names of variables
names(data)

# see first few rows of data
head(data)

# group by time and myco, remove NA values
df_t_m <- data %>% 
  group_by(time, myco) %>% 
  summarise(soil_tem = mean(soil_tem, na.rm = TRUE))



ggplot(df_t_m, aes(x = time, y = soil_tem, color = myco))+
  geom_point()+
  geom_line()+
  labs(title = "Soil temperature over time",
       x = "Time",
       y = "Soil temperature",
       color = "")+
  scale_color_manual(values = two_colors)


# plot logistic map of soil temperature ----
ggplot(data, aes(x = time, y = soil_tem, color= myco)) +
  geom_point(alpha = 0.3) +
  # remove gridlines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
  geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
       y = "Soil temperature (degree C)",
       color = "") +
  scale_color_manual(values = two_colors)
  

# Cd = 0 ----
  ggplot(data %>% 
           subset(stress == 0), aes(x = time, y = soil_tem, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil temperature (degree C)",
         color = "")+
  scale_color_manual(values = two_colors)
  
  
# Cd = 2 ----
  ggplot(data %>% 
           subset(stress == 2), aes(x = time, y = soil_tem, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil temperature (degree C)",
         color = "")+
  scale_color_manual(values = two_colors)
  
# Cd = 5 ----
  ggplot(data %>% 
           subset(stress == 5), aes(x = time, y = soil_tem, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil temperature (degree C)",
         color = "")+
  scale_color_manual(values = two_colors)
  
# Cd = 15 ----
  ggplot(data %>% 
           subset(stress == 15), aes(x = time, y = soil_tem, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil temperature (degree C)",
         color = "")+
  scale_color_manual(values = two_colors)
  
# do the same for soil moisture but do it at different stress levels ----
  ggplot(data, aes(x = time, y = soil_moi, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil moisture (%)",
         color = "")+
  scale_color_manual(values = two_colors)
  
  
ggplot(data %>% 
         subset(stress == 0), aes(x = time, y = soil_moi, color= myco)) +
  geom_point(alpha = 0.3) +
  # remove gridlines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
  geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
       y = "Soil moisture (%)",
       color = "")+
  scale_color_manual(values = two_colors)
  
  ggplot(data %>% 
           subset(stress == 2), aes(x = time, y = soil_moi, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil moisture (%)",
         color = "")+
    scale_color_manual(values = two_colors)
  
  ggplot(data %>% 
           subset(stress == 5), aes(x = time, y = soil_moi, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil moisture (%)",
         color = "")+
    scale_color_manual(values = two_colors)
  
  ggplot(data %>% 
           subset(stress == 15), aes(x = time, y = soil_moi, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Soil moisture (%)",
         color = "")+
    scale_color_manual(values = two_colors)
  
  
  
# NH4_N ----
 p_nh4 <-  ggplot(data, aes(x = time, y = NH4_N, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "NH4-N (mg/kg)",
         color = "")+
    scale_color_manual(values = two_colors)
# NO3_N ----
 p_no3 <-  ggplot(data, aes(x = time, y = NO3_N, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "NO3-N (mg/kg)",
         color = "")+
    scale_color_manual(values = two_colors)
# NO2_N ----
p_no2 <-   ggplot(data, aes(x = time, y = NO2_N, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "NO2-N (mg/kg)",
         color = "")  +
    ylim(0, 0.8)+
    scale_color_manual(values = two_colors)
  
  
  # pH ----
  p_pH <- ggplot(data, aes(x = time, y = pH, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "pH",
         color = "")  +
    ylim(5, 8)+
    scale_color_manual(values = two_colors)
# combined plot ----
p_n <- ggarrange(p_nh4, p_no3, p_no2, p_pH,
                 ncol = 1, nrow = 4,
                 labels = c("A", "B", "C", "D"))
p_n

if(save){
  ggsave("out/soil_N.jpg", p_n, width = 88*1.5, height = 66*3, units = "mm", dpi = 300)
  ggsave("out/soil_N.pdf", p_n, width = 88*1.5, height = 66*3, units = "mm", dpi = 300)
}

# AP ----
  ggplot(data, aes(x = time, y = AP, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "AP (mg/kg)",
         color = "")+
    scale_color_manual(values = two_colors)


  # Pn ----
p_Pn <- ggplot(data, aes(x = time, y = Pn, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Pn",
         color = "")+
    scale_color_manual(values = two_colors)
# Tr ----
p_Tr <-   ggplot(data, aes(x = time, y = Tr, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Tr",
         color = "")+
    scale_color_manual(values = two_colors)
# GS ----
p_GS <- ggplot(data, aes(x = time, y = GS, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Gs",
         color = "")+
    scale_color_manual(values = two_colors)
# CI ----
p_CI <- ggplot(data, aes(x = time, y = CI, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "CI",
         color = "")+
    scale_color_manual(values = two_colors)
# WUE ----
p_WUE <- ggplot(data, aes(x = time, y = WUE, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "WUE",
         color = "")  +
    scale_color_manual(values = two_colors)
  
# Tr_ratio ----
p_Tr_ratio <- ggplot(data, aes(x = time, y = Tr_ratio, color= myco)) +
    geom_point(alpha = 0.3) +
    # remove gridlines
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    # add loess smooth line with confidence interval
    geom_smooth(method = "loess", se = T) +
    labs(x = "Time",
         y = "Tr_ratio",
         color = "")  +
    scale_color_manual(values = two_colors)
# combined plot ----
p_photo <- ggarrange(p_Pn, p_Tr, p_GS, p_CI, p_WUE, p_Tr_ratio,
                 ncol = 2, nrow = 3,
                 labels = c("A", "B", "C", "D", "E", "F"))
p_photo

if(save){
  ggsave("out/photosyn.jpg", p_photo, width = 88*3, height = 66*3, units = "mm", dpi = 300)
  ggsave("out/photosyn.pdf", p_photo, width = 88*3, height = 66*3, units = "mm", dpi = 300)
}
