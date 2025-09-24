setwd("C:/Users/Robin/OneDrive/Dokumente/RUB/Projekte/Accelerometrie/DataWranglingMov")

#### packages ####
library(tidyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)


#### import data ####
data1 <- read_excel("ValuePaMetricsTable1s1.xlsx") # importing data
data2 <- read_excel("ValuePaMetricsTable1s2.xlsx") # importing data

#### merging and filtering ####
data <- rbind(data1,data2) %>% # merge data
  mutate(ID = as.numeric( substr(probandId, 7, 9)))  %>%
  relocate(ID, .before=sensorLocation)  %>%
  select(-c(studyId,probandId,measurementTimePoint, activityClassRefNoValueChange1Hz,stepCountWithNan1Hz )) %>% #delete useless variables
  filter(activityClass != "Unknown") %>% # delete condition "unknown"
  mutate (paMetricActiCounts = sqrt(paMetricActiCountsActiLifeDown1Hz^2 + paMetricActiCountsActiLifeForward1Hz^2 +  paMetricActiCountsActiLifeRight1Hz^2)) %>% # creating vector magnitude of counts (vector summed values)
  
  # transform into mg & counts/min
  mutate (paMetricActiCounts = paMetricActiCounts*60)  %>% # counts per second
  mutate (paMetricEnmo1Hz = paMetricEnmo1Hz*1000)  %>%
  mutate (paMetricMeanAmplitudeDeviation1Hz = paMetricMeanAmplitudeDeviation1Hz*1000)  %>%
  mutate (movementAcceleration1Hz = movementAcceleration1Hz*1000) 
  
  
#### adding condition as numeric variable #### 
data <- data %>% 
  mutate(condition = as.numeric(recode(activityClass,  
                        # category 1: Lying
                        "natural lying" = "1",
                        "lying horizontal" = "2",
                        "lying left" = "3",
                        "lying right" = "4",
                        "lying prone" = "5",
                        
                        # category 2: Sitting
                        "reclining" = "6",
                        "natural sitting" = "7",
                        "sitting - leaned forward" = "8",
                        "sitting - leaned backward" = "9",
                        "sitting - crossed legs right" = "10",
                        "sitting - crossed legs left" = "11",
                        
                        # category 3: Standing
                        "natural standing" = "12",
                        "Stehen" = "13",
                        "standing still - upper body movement" = "14",
                        
                        # category 4: ADL (activities of daily life)
                        "light activity" = "15",
                        "working at PC" = "16",
                        "Tisch decken" = "17",
                        "reading newspaper" = "18",
                        "tidy up" = "19",
                        "get dressed" = "20",
                        "put clean sheets on the bed" = "21",
                        "smartphone usage" = "22",
                        "hang out the laundry" = "23",
                        "vacuuming" = "24",
                        "Fenster putzen" = "25",
                      
                        # category 5: climbing stairs
                        "slope up slope down" = "26",
                        
                        # category 6: Walking
                        "walking 2.8 km/h" = "27",
                        "walking 3.2 km/h" = "28",
                        "walking 5.4 km/h" = "29",
                        
                        # category 7: running
                        "running 7.6 km/h" = "30",
                        "running 12 km/h" = "31",
                        
                        # category 8: cycling
                        "Radfahren" = "32"
                        ))) 

#### categorizing conditions #### 
data$category[data$condition<=5] <- 1
data$category[data$condition>=6 & data$condition<=11] <- 2
data$category[data$condition>=12 & data$condition<=14] <- 3
data$category[data$condition>=15 & data$condition<=25] <- 4
data$category[data$condition==26] <- 5
data$category[data$condition>=27 & data$condition<=29] <- 6
data$category[data$condition>=30 & data$condition<=31] <- 7
data$category[data$condition==32] <- 8
  

#### reorder by ID ####

# ordering by ID
data <- data[order(data$ID),]
  
#### cut seconds ####  

# creating vector that numbers each condition 
  i=1
  s=1
  supportVector <- NULL
  while (i<=length(data$condition)) {
      if(i<length(data$condition))    
      { 
        if(data$condition[i]==data$condition[i+1])
        {supportVector[i]<-s}
        else
        {supportVector[i]<-s
        s=s+1}
      }  
      else{supportVector[i]<-s}  
        i=i+1
  }
  data$supportVector <- supportVector

# creating vector that lists number of measurements per condition (seconds)
  i=1
  k=1
  count <- 1
  countVector <- NULL
  while (i<=length(data$condition)) {
      if(i<length(data$condition))    
      { 
        if(data$supportVector[i]==data$supportVector[i+1])
        {count <- count+1}
        else
        {countVector[k] <- count
        k <- k+1
        count <- 1}
      }  
      else{countVector[k] <- count}  
      i=i+1
  }

# adding vector indicating how many measurements recorded for current condition (seconds)
  i=1 
  measurementsPerCondition <- NULL
  while (i<=length(data$condition)) {
      measurementsPerCondition[i] <- countVector[data$supportVector[i]]
      i=i+1
  }
  data$measurementsPerCondition <- measurementsPerCondition

# adding vector indicating how many seconds should be analyzed per condition 
data <- data %>% 
  mutate(seconds = as.numeric(recode(activityClass,  
                                       # category 1: Lying
                                       "natural lying" = "110",
                                       "lying horizontal" = "110",
                                       "lying left" = "110",
                                       "lying right" = "110",
                                       "lying prone" = "110",
                                       
                                       # category 2: Sitting
                                       "reclining" = "110",
                                       "natural sitting" = "110",
                                       "sitting - leaned forward" = "110",
                                       "sitting - leaned backward" = "110",
                                       "sitting - crossed legs right" = "110",
                                       "sitting - crossed legs left" = "110",
                                       
                                       # category 3: Standing
                                       "natural standing" = "110",
                                       "Stehen" = "110",
                                       "standing still - upper body movement" = "110",
                                       
                                       # category 4: ADL (activities of daily life)
                                       "light activity" = "170",
                                       "working at PC" = "170",
                                       "Tisch decken" = "170",
                                       "reading newspaper" = "170",
                                       "tidy up" = "170",
                                       "get dressed" = "170",
                                       "put clean sheets on the bed" = "170",
                                       "smartphone usage" = "170",
                                       "hang out the laundry" = "170",
                                       "vacuuming" = "170",
                                       "Fenster putzen" = "170",
                                       
                                       # category 5: climbing stair
                                       "slope up slope down" = "170",
                                       
                                       # category 6: Walking
                                       "walking 2.8 km/h" = "290",
                                       "walking 3.2 km/h" = "290",
                                       "walking 5.4 km/h" = "290",
                                       
                                       # category 7: running
                                       "running 7.6 km/h" = "290",
                                       "running 12 km/h" = "170",
                                       
                                       # category 8: cycling
                                       "Radfahren" = "290"
  ))) 


# how many rows to delete per condition
data$deletingNRows <- data$measurementsPerCondition-data$seconds

# indexing rows to delete
i=1
index<- NULL
while (i <= max(data$supportVector)) {
  row <- match(i,data$supportVector)
  
  if(data$deletingNRows[row]>0) { # only index rows in which more seconds were recorded than supposed to -10s
  # oben
  oben <- row:(row+ceiling(data$deletingNRows[row]/2)-1)
  index <- c(index, oben)
  
  # unten (außer wenn eine zeile gelöscht werden soll)
      if(data$deletingNRows[row] != 1){
      unten <- ((row+data$measurementsPerCondition[row])- floor(data$deletingNRows[row]/2)) :  ((row+data$measurementsPerCondition[row])-1)
      index <- c(index, unten)
  }
  }
  else { # for every condition were less seconds than supposed to were recorded, 5s will be deleted at the top and bottom
    # oben
    oben <- row:(row+4)
    index <- c(index, oben)
    
    # unten
    unten <-     (row+data$measurementsPerCondition[row]-5) : (row+data$measurementsPerCondition[row]-1)
    index <- c(index, unten)
  }
  i=i+1
  
}


# cut rows based on index vector, and create new dataframe with cut values
CutData <- data[-index,]
CutData <- CutData %>% select(-c(supportVector,deletingNRows)) %>%
  relocate(category, .after=activityClass) %>%
  relocate(condition, .after=activityClass)

#### pre aggregate  ####

# aggregate cut data, leaving in participants
pre_aggregated_cut_data <- CutData %>% group_by(ID,category, activityClass,condition,sensorLocation,)  %>% 
  summarise(across(c(
    
    movementAcceleration1Hz, 
    paMetricEnmo1Hz,
    paMetricMeanAmplitudeDeviation1Hz,
    paMetricActiCounts
    
  ), list(
    
    n = length,
    min = min,
    max = max,
    mean = mean, 
    median = median,
    sd = sd
    
  )), .groups = 'drop') %>% 
  select(-c(movementAcceleration1Hz_n, paMetricEnmo1Hz_n, paMetricMeanAmplitudeDeviation1Hz_n, paMetricActiCounts_min, paMetricActiCounts_max))  #delete useless variables

# aggregate sd for means across participants
aggregated_cut_data_between <- pre_aggregated_cut_data %>% group_by(category, activityClass,condition,sensorLocation)  %>% 
  summarise(across(c(
    
    movementAcceleration1Hz_mean,
    paMetricEnmo1Hz_mean,
    paMetricMeanAmplitudeDeviation1Hz_mean,
    paMetricActiCounts_mean,
    
    
  ), list(

    min_between = min,
    max_between = max
    
  )), .groups = 'drop') 



####  aggregate  ####


# aggregate cut data, across all participants categorized
aggregated_cut_data_categorized <- CutData %>% group_by(category,sensorLocation)  %>% 
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

aggregated_cut_data_categorized$category <- factor(aggregated_cut_data_categorized$category, labels = c("Lying", "Sitting","Standing","ADL","climbing stairs","Walking","Jogging","Cycling"))

# adding between subjects min and max
aggregated_cut_data_categorized <-cbind(aggregated_cut_data_categorized, aggregated_cut_data_between[,5:12])



# aggregate cut data, across all participants and all conditions
aggregated_cut_data <- CutData %>% group_by(category,condition, sensorLocation)  %>% 
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

aggregated_cut_data$category <- factor(aggregated_cut_data$category, labels = c("Lying", "Sitting","Standing","ADL","climbing stairs","Walking","Jogging","Cycling"))

# adding between subjects min and max
aggregated_cut_data <-cbind(aggregated_cut_data, aggregated_cut_data_between[,5:12])


#### formate table ####



mg <- paste0(round(aggregated_cut_data_categorized$movementAcceleration1Hz_mean)," +/- ",round(aggregated_cut_data_categorized$movementAcceleration1Hz_sd) ," (", round(aggregated_cut_data_categorized$movementAcceleration1Hz_mean_min_between), " - ", round(aggregated_cut_data_categorized$movementAcceleration1Hz_mean_max_between), ")") 
ENMO <- paste0(round(aggregated_cut_data_categorized$paMetricEnmo1Hz_mean)," +/- ",round(aggregated_cut_data_categorized$paMetricEnmo1Hz_sd) ," (", round(aggregated_cut_data_categorized$paMetricEnmo1Hz_mean_min_between), " - ", round(aggregated_cut_data_categorized$paMetricEnmo1Hz_mean_max_between), ")") 
MAD <- paste0(round(aggregated_cut_data_categorized$paMetricMeanAmplitudeDeviation1Hz_mean)," +/- ",round(aggregated_cut_data_categorized$paMetricMeanAmplitudeDeviation1Hz_sd) ," (", round(aggregated_cut_data_categorized$paMetricMeanAmplitudeDeviation1Hz_mean_min_between), " - ", round(aggregated_cut_data_categorized$paMetricMeanAmplitudeDeviation1Hz_mean_max_between), ")") 
counts <- paste0(round(aggregated_cut_data_categorized$paMetricActiCounts_mean)," +/- ",round(aggregated_cut_data_categorized$paMetricActiCounts_sd) ," (", round(aggregated_cut_data_categorized$paMetricActiCounts_mean_min_between), " - ", round(aggregated_cut_data_categorized$paMetricActiCounts_mean_max_between), ")") 

table_categorized <- data.frame(aggregated_cut_data_categorized$category,aggregated_cut_data_categorized$sensorLocation, mg, ENMO, MAD, counts)


#### safe files ####

# cut data 
write_xlsx(CutData,"CutData.xlsx")

# cleaned data 
write_xlsx(data,"CleanedData.xlsx")

# aggregated data 
write_xlsx(aggregated_cut_data,"aggregated_cut_data.xlsx")
write_xlsx(aggregated_cut_data_categorized,"aggregated_cut_data_categorized.xlsx")
write_xlsx(pre_aggregated_cut_data,"pre_aggregated_cut_data.xlsx")

write_xlsx(table_categorized,"table_categorized.xlsx")


