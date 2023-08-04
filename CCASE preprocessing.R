#### Pre-processing for Processed Data (FiboxAv and ARQ has been calculated) ####
# Sarah Goldsmith, December 2021

# desctiption: 
# contains four sections, some of which may not be needed for all files
# 1. Fix plot names that have been turned into dates (if opened and saved in Excel)
#     #Files required: ARQ calculations
# 2. Add treatment info to files where this was not entered
#     #Files required: ARQ calculations
# 3. Merge STM data
#     # Files required: HB STM data, this version merges the 2018 and 2019 files
# 4. Calculate average ambient CO2 (only necessary for "old" (before ARQ ))
#     #Files required: Processed Data (Fibox average has been calculated)

library(stringr)
library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)


#### 1. fix plot names if they have been turned into dates by Excel ####
setwd("/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE/ready to use/")

plot.key <- read.csv("/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE plot treatment key.csv")
plot.key <- plot.key[,c("Plot.1", "Treatment")]
colnames(plot.key) <- c("Plot", "Treatment")
plots <- c("3-Jan" = "1-3",    "2-Jan"= "1-2",    "1-Jan" = "1-1",    "4-Jun" = "6-4",    "3-Jun" = "6-3",   
           "2-Jun" = "6-2",    "1-Mar" = "3-1" ,    "4-Mar" = "3-4",    "3-Mar"= "3-3",    "2-Feb" = "2-2",    "1-Feb" = "2-1", 
           "4-Feb" = "2-4",  "3-May" = "5-3",    "1-May" = "5-1", "2-May" = "5-2",    "3-Apr" = "4-3",    "4-Apr" = "4-4",  
           "1-Apr" = "4-1")

print(filenames <- list.files())
filename <- "<2022-03-10> Processed Data Output.csv"
data <- read.csv(filename)

data$Plot <- 
  data$Plot %>%
  str_replace_all(plots)

##### 2. for files that need treatment info added ####
data <- data %>%
  select(-"Treatment")


temp_data  <- data %>% 
  filter(!grepl("CO2_FREE", Plot) & !grepl("2_PER", Plot)) %>%
  mutate(Plot = str_replace_all(Plot, "_", "-"))

temp_standards <- data %>%
  filter(grepl("CO2_FREE", Plot) | grepl("2_PER", Plot)) #I feel like there has to be a better way to do this

data <- rbind(temp_data, temp_standards)

data <- merge.data.frame(plot.key, data, by = "Plot")
write.csv(data, filename)

#### 3. Merge STM data ####
setwd("/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE/STM data")

#read STM data file 
# **note: line 13 (units) must be deleted prior to reading it in**
# also remove the VWC columns that are in values of microseconds
plots3.6 <- read.csv("CCASE_AllPlots_SoilTempMoisture_190201_191231_plots3-6.csv", skip = 12, header = TRUE)
plots1.2 <- read.csv("CCASE_AllPlots_SoilTempMoisture_190201_191231_plots1-2.csv", skip = 12, header = TRUE)
stm_data_2018 <- read.csv("CCASE_Sensor_Data_May18_Feb19.csv")

stm_2019 <- merge(plots1.2, plots3.6, by = "TIMESTAMP", all = T)
stm_2019 <- stm_2019 %>%
  select(-c("X", "RECORD.x","Sample.ID..assigned.by.Templer.lab.not.by.datalogger.program", "RECORD.y" ))

stm_all <- rbind.fill(stm_data_2018, stm_2019)
write.csv(stm_all, "compiled CCASE STM data 2018-2019.csv")


#### 4. calculate average ambient CO2 ####
setwd("/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE/Field Book")
filenames <- list.files(path = "/Users/sarahgoldsmith/Documents/Dartmouth/Fibox_Processing/CCASE/Field Book",
                        pattern = "*.xlsx", full.names = FALSE)

calculate.ambient.CO2 <- function(filename){
  data <- read_xlsx(filename, sheet = "Standards") # "old" format files have a separate tab for Standards
  date <- str_remove(filename, " Processed Data.xlsx")
  
  avg.ambient.CO2 <- data %>%
    filter(grepl("AMB", File)) %>%
    amb.CO2.all <- amb.CO2.all
    summarise(amb.CO2.all = mean(CO2))
  
  amb.CO2.all <- rbind(date, avg.ambient.CO2)
}

amb.CO2.all <- matrix(ncol = 2, nrow = length(filenames))
amb.CO2.all <- t(as.data.frame(sapply(filenames, calculate.ambient.CO2)))
rownames(amb.CO2.all) <- NULL
colnames(amb.CO2.all) <- c("Date", "amb.CO2.all")

write.csv(amb.CO2.all, "avg ambient CO2 for 2018-10-5 through 2019-6-12.csv", row.names = F) #update file name as necessary


