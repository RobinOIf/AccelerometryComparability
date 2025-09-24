setwd("C:/Users/Robin/OneDrive/Dokumente/RUB/Projekte/Accelerometrie/Plots")

#### packages ####
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
#library(plyr)
library(gridExtra)
library(cowplot)
library(patchwork)
library(dplyr)

#### import data ####
cut_data <- read_excel("CutData.xlsx") # importing data



# aggregate cut data, leaving in participants
#aggregated_cut_data <- cut_data %>% group_by(ID,category,sensorLocation,)  %>% 
#  summarise(across(c(
    
  #  movementAcceleration1Hz, 
  #  paMetricEnmo1Hz,
  #  paMetricMeanAmplitudeDeviation1Hz,
  #  paMetricActiCounts
    
#  ), list(
#    min = min,
 #   max = max,
 #   mean = mean, 
 #  sd = sd
 #   
#  )), .groups = 'drop') 

aggregated_cut_data <- cut_data %>%
  group_by(ID, category, sensorLocation) %>%
  summarise(across(
    c(
      movementAcceleration1Hz, 
      paMetricEnmo1Hz,
      paMetricMeanAmplitudeDeviation1Hz,
      paMetricActiCounts
    ), 
    list(
      min = ~min(.),
      max = ~max(.),
      mean = ~mean(.), 
      sd = ~sd(.)
    ), 
    .names = "{.col}_{.fn}"
  ), .groups = 'drop')

#### data wrangling  ####
# category as factor
aggregated_cut_data$category <- as.factor(aggregated_cut_data$category)
aggregated_cut_data$category <- factor(aggregated_cut_data$category, labels = c("Lying", "Sitting","Standing","ADL","climbing stairs","Walking","Jogging","Cycling"))
#  renaming
aggregated_cut_data$sensorLocation[aggregated_cut_data$sensorLocation=="right_ankle"] <- 'ankle'
aggregated_cut_data$sensorLocation[aggregated_cut_data$sensorLocation=="right_side_hip"] <- 'hip'
aggregated_cut_data$sensorLocation[aggregated_cut_data$sensorLocation=="right_thigh"] <- 'thigh'
aggregated_cut_data$sensorLocation[aggregated_cut_data$sensorLocation=="right_upper_arm"] <- 'upper arm'
aggregated_cut_data$sensorLocation[aggregated_cut_data$sensorLocation=="right_wrist"] <- 'wrist'


#### boxplots per category and per sensor location ####

levels(aggregated_cut_data$category)[5] <- "Stairs"
aggregated_cut_data$category <- factor(aggregated_cut_data$category, 
                                       levels = c("lying", "sitting", "standing", "adl", "stairs", "walking", "jogging", "cycling"))


P1_mg <- ggplot(aggregated_cut_data, aes(x=category, y=movementAcceleration1Hz_mean, color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=1, size=0.1) +
  xlab("PA category") + 
  ylab("MAI (in mg)")+
  guides(color=guide_legend(title="sensor location")) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.11,.81)
  )

  #scale_color_grey() +
  #theme_bw()

# Speichern mit hoher Auflösung
ggsave("P1_MAI_highres.png", plot = P1_mg, width = 8, height = 5, dpi = 600)



P2_ENMO <- ggplot(aggregated_cut_data, aes(x=category, y=(paMetricEnmo1Hz_mean), color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=1, size=0.1) +
  xlab("PA category") + 
  ylab("ENMO (in mg)")+
  guides(color=guide_legend(title="sensor location")) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.11,.81)
  )
# Speichern mit hoher Auflösung
ggsave("P1_ENMO_highres.png", plot = P2_ENMO, width = 8, height = 5, dpi = 600)





P3_MAD <- ggplot(aggregated_cut_data, aes(x=category, y=(paMetricMeanAmplitudeDeviation1Hz_mean), color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=1, size=.1) +
  xlab("PA category") + 
  ylab("MAD (in mg)")+
  guides(color=guide_legend(title="sensor location")) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.11,.81)
  )

ggsave("P1_MAD_highres.png", plot = P3_MAD, width = 8, height = 5, dpi = 600)





P4_counts <- ggplot(aggregated_cut_data, aes(x=category, y=(paMetricActiCounts_mean), color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=1, size=0.1) +
  xlab("PA category") + 
  ylab("CPM (ActiGraph GT3X+)")+
  guides(color=guide_legend(title="sensor location")) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.11,.81)
  )

ggsave("P1_CPM_highres.png", plot = P4_counts, width = 8, height = 5, dpi = 600)




###############################################################################

### scater plots

# MAI within metrics (wrist, thigh, hip), grouped by activity

cut_data$category[cut_data$category==1] <- "lying"
cut_data$category[cut_data$category==2] <- "sitting"
cut_data$category[cut_data$category==3] <- "standing"
cut_data$category[cut_data$category==4] <- "adl"
cut_data$category[cut_data$category==5] <- "stairs"
cut_data$category[cut_data$category==6] <- "walking"
cut_data$category[cut_data$category==7] <- "jogging"
cut_data$category[cut_data$category==8] <- "cycling"

# MAI dataframe
MAI <- data.frame(MAI_wrist <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_wrist"], MAI_hip <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_side_hip"], MAI_thigh <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_thigh"], category <- cut_data$category[cut_data$sensorLocation == "right_thigh"]  )

# plot wrist ~ thigh
p5_MAI_wrist_thigh <- MAI %>% ggplot(aes(x = MAI_wrist, y = MAI_thigh, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI wrist") +
  ylab("MAI thigh")  + 
  theme(legend.position="none")


# plot wrist ~ hip
p6_MAI_wrist_hip <- MAI %>% ggplot(aes(x = MAI_wrist, y = MAI_hip, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI wrist")  + 
  ylab("MAI hip")  + 
  theme(legend.position="none")

# plot wrist ~ hip
p7_MAI_thigh_hip <- MAI %>% ggplot(aes(x = MAI_thigh, y = MAI_hip, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI thigh") +
  ylab("MAI hip")   + 
  guides(colour = guide_legend(override.aes = list(size=7, alpha = .6))) +
  labs(fill = "category") +
  theme(legend.position = "top")

legend <- get_legend(p7_MAI_thigh_hip) # extract legend
p7_MAI_thigh_hip <- p7_MAI_thigh_hip + theme(legend.position="none")



p8_MAI_within <- p5_MAI_wrist_thigh + p6_MAI_wrist_hip + p7_MAI_thigh_hip





# MAI vs. Counts - wrist, thigh, hip), grouped by activity


# MAI dataframe
MAI_counts <- data.frame(MAI_wrist <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_wrist"], MAI_hip <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_side_hip"], MAI_thigh <- cut_data$movementAcceleration1Hz[cut_data$sensorLocation == "right_thigh"], category <- cut_data$category[cut_data$sensorLocation == "right_thigh"] ,
                  counts_wrist <- cut_data$paMetricActiCounts[cut_data$sensorLocation == "right_wrist"], counts_hip <- cut_data$paMetricActiCounts[cut_data$sensorLocation == "right_side_hip"], counts_thigh <-cut_data$paMetricActiCounts[cut_data$sensorLocation == "right_thigh"], 
                  counts_ankle <- cut_data$paMetricActiCounts[cut_data$sensorLocation == "right_ankle"])

# plot MAI ~ counts, wrist 
p9_MAI_counts_wrist <- MAI_counts %>% ggplot(aes(x = MAI_wrist, y = counts_wrist, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI wrist") +
  ylab("counts wrist")  + 
  theme(legend.position="none")

# plot MAI ~ counts, hip 
p10_MAI_counts_hip  <- MAI_counts %>% ggplot(aes(x = MAI_hip, y = counts_hip, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI hip")  + 
  ylab("counts hip")  + 
  theme(legend.position="none")

# plot MAI ~ counts, thigh 
p11__MAI_counts_thigh  <- MAI_counts %>% ggplot(aes(x = MAI_thigh, y = counts_thigh, colour = factor(category))) + 
  geom_point(size=.3, alpha = .2)  +
  xlab("MAI thigh") +
  ylab("counts thigh")   + 
  guides(colour = guide_legend(override.aes = list(size=7, alpha = .6),title = "category")) +
  labs(fill = "category") +
  theme(legend.position = "top") 


legend <- get_legend(p11__MAI_counts_thigh) # extract legend
p11__MAI_counts_thigh <- p11__MAI_counts_thigh + theme(legend.position="none")



p12_MAI_counts_between <- p9_MAI_counts_wrist + p10_MAI_counts_hip + p11__MAI_counts_thigh

















###############################################################################
MAD <- data.frame(MAD_chest <- cut_data$paMetricMeanAmplitudeDeviation1Hz [cut_data$sensorLocation == "chest"], MAD_hip <- cut_data$paMetricMeanAmplitudeDeviation1Hz[cut_data$sensorLocation == "right_side_hip"])



# new plots:
# relationship between positions:

# smallest: counts - wrist and ankle

smallest_pos <- MAI_counts %>% ggplot(aes(x = counts_wrist, y = counts_ankle, colour = factor(category))) + 
  geom_point(shape = 16,size=0.2, alpha = .5)  +
  xlab("wrist cpm") +
  ylab("thigh cpm")  + 
  theme(legend.position="none") + 
  ggtitle("Smallest Correlation")



# highest: mad - chest and hip

highest_pos <- MAD %>% ggplot(aes(x = MAD_chest, y = MAD_hip, colour = factor(category))) + 
  geom_point(shape = 16,size=0.2, alpha = .5)  +
  xlab("chest MAD") +
  ylab("hip MAD")  + 
  theme(legend.position="none")+
  ggtitle("Highest Correlation")



betw.positions <- smallest_pos + highest_pos 

ggsave("positions.png", plot = betw.positions, width = 10, height = 5, dpi = 600)




# relationship between metrics:

MAD_counts <- data.frame(MAD_chest <- cut_data$paMetricMeanAmplitudeDeviation1Hz [cut_data$sensorLocation == "chest"], counts_chest <- cut_data$paMetricActiCounts[cut_data$sensorLocation == "chest"])
MAD_ENMO <- data.frame(MAD_chest <- cut_data$paMetricMeanAmplitudeDeviation1Hz [cut_data$sensorLocation == "chest"], ENMO_chest <- cut_data$paMetricEnmo1Hz [cut_data$sensorLocation == "chest"])


# smallest: chest - mad and cpm
smallest_met <- MAD_counts %>% ggplot(aes(x = MAD_chest, y = counts_chest, colour = factor(category))) + 
  geom_point(shape = 16,size=.2, alpha = .5)  +
  xlab("chest MAD") +
  ylab("chest cpm")  + 
  ggtitle("Smallest Correlation") +
  theme(legend.position="none")

# highest: chest - enmo and mad

highest_met <- MAD_ENMO %>% ggplot(aes(x = MAD_chest, y = ENMO_chest, colour = factor(category))) + 
  geom_point(shape = 16,size=.2, alpha = .5)  +
  xlab("chest MAD") +
  ylab("chest ENMO") +
  ggtitle("Highest Correlation")+
  theme(legend.position="none")


betw.metrics <- smallest_met + highest_met 


ggsave("metrics.png", plot = betw.metrics, width = 10, height = 5, dpi = 600)

ggsave("legend.png", plot = p7_MAI_thigh_hip, width = 6, height = 6, dpi = 600)





###############################################################################






# aggregate cut data, leaving in participants
aggregated_cut_data <- cut_data %>% group_by(ID,category,sensorLocation,)  %>% 
  summarise(across(c(
    
    movementAcceleration1Hz, 
    paMetricEnmo1Hz,
    paMetricMeanAmplitudeDeviation1Hz,
    paMetricActiCounts
    
  ), list(
    min = min,
    max = max,
    mean = mean, 
    sd = sd
    
  )), .groups = 'drop') 




####  boxplots per condition (points = subject means ) ####


# aggregate cut data, leaving in participants
aggregated_cut_data_conditions <- cut_data %>% group_by(ID,condition,sensorLocation,)  %>% 
  summarise(across(c(
    
    movementAcceleration1Hz, 
    paMetricEnmo1Hz,
    paMetricMeanAmplitudeDeviation1Hz,
    paMetricActiCounts
    
  ), list(
    min = min,
    max = max,
    mean = mean, 
    sd = sd
    
  )), .groups = 'drop') 



# data wrangling  #
# condition as factor
aggregated_cut_data_conditions$condition <- as.factor(aggregated_cut_data_conditions$condition)
aggregated_cut_data_conditions$condition <- factor(aggregated_cut_data_conditions$condition, labels = c(

# category 1: Lying
"natural lying",
"lying horizontal" ,
"lying left",
"lying right" ,
"lying prone",

# category 2: Sitting
"reclining" ,
"natural sitting",
"sitting - leaned forward" ,
"sitting - leaned backward",
"sitting - crossed legs right" ,
"sitting - crossed legs left",

# category 3: Standing
"natural standing" ,
"Stehen" ,
"standing still - upper body movement",

# category 4: ADL (activities of daily life)
"light activity" ,
"working at PC",
"Tisch decken",
"reading newspaper",
"tidy up" ,
"get dressed",
"put clean sheets on the bed" ,
"smartphone usage" ,
"hang out the laundry",
"vacuuming" ,
"Fenster putzen" ,

# category 5: climbing stairs
"slope up slope down" ,

# category 6: Walking
"walking 2.8 km/h" ,
"walking 3.2 km/h" ,
"walking 5.4 km/h" ,

# category 7: running
"running 7.6 km/h" ,
"running 12 km/h",

# category 8: cycling
"Radfahren" 
))


#  renaming
aggregated_cut_data_conditions$sensorLocation[aggregated_cut_data_conditions$sensorLocation=="right_ankle"] <- 'ankle'
aggregated_cut_data_conditions$sensorLocation[aggregated_cut_data_conditions$sensorLocation=="right_side_hip"] <- 'hip'
aggregated_cut_data_conditions$sensorLocation[aggregated_cut_data_conditions$sensorLocation=="right_thigh"] <- 'thigh'
aggregated_cut_data_conditions$sensorLocation[aggregated_cut_data_conditions$sensorLocation=="right_upper_arm"] <- 'upper arm'
aggregated_cut_data_conditions$sensorLocation[aggregated_cut_data_conditions$sensorLocation=="right_wrist"] <- 'wrist'


P1b_mg <- ggplot(aggregated_cut_data_conditions, aes(x=condition, y=movementAcceleration1Hz_mean, color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=1, size=0.5) +
  xlab("PA category") + 
  ylab("movement acceleration [milli g]")+
  guides(color=guide_legend(title="sensor location"))+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))







####  boxplots per condition ####

plot_data <- cut_data


plot_data$mg_means <- 
aggregated_cut_data$movementAcceleration1Hz_mean

# data wrangling  #
# condition as factor
plot_data$condition <- as.factor(plot_data$condition)
plot_data$condition <- factor(plot_data$condition, labels = c(
  
  # category 1: Lying
  "natural lying",
  "lying horizontal" ,
  "lying left",
  "lying right" ,
  "lying prone",
  
  # category 2: Sitting
  "reclining" ,
  "natural sitting",
  "sitting - leaned forward" ,
  "sitting - leaned backward",
  "sitting - crossed legs right" ,
  "sitting - crossed legs left",
  
  # category 3: Standing
  "natural standing" ,
  "Stehen" ,
  "standing still - upper body movement",
  
  # category 4: ADL (activities of daily life)
  "light activity" ,
  "working at PC",
  "Tisch decken",
  "reading newspaper",
  "tidy up" ,
  "get dressed",
  "put clean sheets on the bed" ,
  "smartphone usage" ,
  "hang out the laundry",
  "vacuuming" ,
  "Fenster putzen" ,
  
  # category 5: climbing stairs
  "slope up slope down" ,
  
  # category 6: Walking
  "walking 2.8 km/h" ,
  "walking 3.2 km/h" ,
  "walking 5.4 km/h" ,
  
  # category 7: running
  "running 7.6 km/h" ,
  "running 12 km/h",
  
  # category 8: cycling
  "Radfahren" 
))


#  renaming
plot_data$sensorLocation[plot_data$sensorLocation=="right_ankle"] <- 'ankle'
plot_data$sensorLocation[plot_data$sensorLocation=="right_side_hip"] <- 'hip'
plot_data$sensorLocation[plot_data$sensorLocation=="right_thigh"] <- 'thigh'
plot_data$sensorLocation[plot_data$sensorLocation=="right_upper_arm"] <- 'upper arm'
plot_data$sensorLocation[plot_data$sensorLocation=="right_wrist"] <- 'wrist'



P1c_mg <- ggplot(plot_data, aes(x=condition, y=movementAcceleration1Hz, color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=.1, size=0.1) +
  xlab("PA category") + 
  ylab("movement acceleration [milli g]")+
  guides(color=guide_legend(title="sensor location"))+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

png(file="C:/Users/Robin/OneDrive/Dokumente/RUB/Projekte/Accelerometrie/Plots/plotall.png",
    width=8000 , height=4000)
ggplot(plot_data, aes(x=condition, y=movementAcceleration1Hz, color=sensorLocation)) + 
  geom_boxplot(outlier.shape = NA)  +
  geom_point(position=position_jitterdodge(dodge.width = 0.75, jitter.width = .1), alpha=.1, size=0.05) +
  xlab("PA category") + 
  ylab("movement acceleration [milli g]")+
  guides(color=guide_legend(title="sensor location"))+  
  scale_y_continuous(minor_breaks = seq(0, 4000, 100),breaks=seq(0, 4000, 100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=14))


dev.off()