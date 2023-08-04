
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)

#### pre-processing for STM data ####
# 
# setwd("/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE/ready to use/fixed_plot_names/ARQ calcs")
# data <- read.csv("allARQ.csv")
# dates <- unique(data$Date)
# colnames(data)[colnames(data) == "Plot"] <- "PQ"
# data$Plot <- substr(data$PQ, start = 1, stop = 1)

dates <- as.Date(c("2022-03-10", "2022-05-24", "2022-06-06", "2022-06-15", "2022-06-20", "2022-06-29", "2022-07-11", "2022-07-19", "2022-07-27", 
           "2022-08-05", "2022-08-10","2022-09-20"), "%Y-%m-%d")

setwd("/Users/f00502n/Documents/Dartmouth/Fibox_Processing/CCASE/STM data")



#read STM data file 
# **note: line containing units must be deleted prior to reading it in if it is after column headers**
# also remove the VWC columns that are in values of microseconds
plots3.6 <- read_excel("HBEF_CCASE_2022 Treatment & Reference Soil Data_Compiled.xlsx", sheet = "Treatment Data", skip = 8)
plots1.2 <- read_excel("HBEF_CCASE_2022 Treatment & Reference Soil Data_Compiled.xlsx", sheet = "Reference Data", skip = 7)
#stm_data_2018 <- read.csv("CCASE_Sensor_Data_May18_Feb19.csv")

header_key <- read.csv("CCASE_Sensor_Headings.csv")

#this should technically be a function probably
tidy3.6 <- gather(plots3.6, "Heading", "value" , 4:length(plots3.6))
tidy1.2 <- gather(plots1.2, "Heading", "value" , 4:length(plots1.2))

tidy1.2 <- tidy1.2[, c("TIMESTAMP", "Heading", "value")]
tidy3.6 <- tidy3.6[, c("TIMESTAMP", "Heading", "value")]

# tidy.2018 <- gather(stm_data_2018, "Heading", "value", 3:length(stm_data_2018))
# tidy.2018 <- tidy.2018[, c("TIMESTAMP", "Heading", "value")]

STM_data <- rbind(tidy1.2, tidy3.6)
STM_data <- merge(STM_data, header_key, by = "Heading")
STM_data <- STM_data %>%
  select(-"Heading")

STM_data$PQ <- paste(STM_data$Plot, STM_data$ID, sep = "-")
STM_data$Date = as.POSIXct(as.character(STM_data$TIMESTAMP), format = "%Y-%m-%d") 
STM_data$Date = as.Date(STM_data$Date,tz="America/New_York")

STM_data = subset(STM_data, Date %in% as.Date(dates, "%m-%d-%y"))

#separate soil temperature
soil.temp <- STM_data[which(STM_data$Sensor == "Soil temp"),]
names(soil.temp)[names(soil.temp) == 'value'] <- "soil.temp"
soil.temp$soil.temp <- as.numeric(soil.temp$soil.temp)
#write.csv(soil.temp, "CCASE_soil_temp_2022.csv")

soil.temp.avg <- soil.temp %>%
  dplyr::group_by(Date, Plot, Depth.cm) %>%
  dplyr::summarise(across(soil.temp, mean, na.rm = T))
soil.temp.avg$Depth <- as.character(revalue(as.factor(soil.temp.avg$Depth.cm), c("5" = "OA", "10" = "10", "30" = "30")))

#separate soil moisture
soil.vwc <- STM_data[which(STM_data$Sensor == "Soil VWC"),]
names(soil.vwc)[names(soil.vwc) == 'value'] <- "VWC"
#remove soil VWC depth since it is all at 15cm
soil.vwc <- soil.vwc %>%
  select(-"Depth.cm") #, - "TIMESTAMP", -"Sensor", -"PQ", -"ID"
soil.vwc$VWC <- as.numeric(soil.vwc$VWC)
#write.csv(soil.vwc, "CCASE_soil_VWC_2022.csv")

#average soil moisture per plot per day
soil.vwc.avg <- soil.vwc %>% 
  dplyr::group_by(Plot, Date) %>%
  dplyr::summarise(across(VWC, mean, na.rm = T))
#write.csv(soil.vwc.avg, "CCASE_soil_VWC_2022_average.csv")

averages <- merge(soil.vwc.avg, soil.temp.avg, by = c("Date", "Plot"))
#write.csv(averages, "CCASE_temp_VWC_daily_avg_2022.csv")
# data$Date <- as.Date(data$Date)
# data2 <- merge(data, soil.vwc.avg, by =  c("Date", "Plot"))
# data2 <- merge(data2, soil.temp.avg, by = c("Date", "Plot", "Depth"))
# 
# data2 <- data2 %>%
#   select(-"Depth.cm.x", -"Depth.f", -"Depth.cm.y")
# write.csv(data2, "allARQ_withSTM.csv")

ggplot( data = soil.vwc.avg) +
  geom_point(aes(x = Date, y = VWC, color = as.factor(Plot))) +
  scale_colour_manual(values=c("red", "orange", "green", "blue", "purple", "yellow"))
