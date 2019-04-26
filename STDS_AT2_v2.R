##### Set working directory
rm(list=ls())
setwd("H:/V/UTS/0. MDSI/36103 Statistical Thinking for Data Science/AT2/Model Files")

##### Load libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)
library(ggrepel)
library(readxl)
library(corrplot)

##### Data Load and EDA ##### 

#### NSW Load Crime Statistics

## Read data 
crime <- read_excel("NSW Recorded Crime Statistics January 2014 to December 2018.xlsx")

## Check for missing values
crime[crime == "NULL"] <- 0
plot(colSums(is.na(crime)))

## Format and Filter Data

# Rename variables 
colnames(crime)[1] <- "LGA"
colnames(crime)[2] <- "Offence"
colnames(crime)[3] <- "Count_2014"
colnames(crime)[4] <- "Count_2015"
colnames(crime)[5] <- "Crim_Count"
colnames(crime)[6] <- "Count_2017"
colnames(crime)[7] <- "Count_2018"

# Omit records with missing values
crime <- crime[complete.cases(crime), ]

# Remove unusable variables
crime [,c(3:4, 6:10)] <- NULL

# Omit records where LGA is not LGA
crime <-crime[!(crime$LGA == "In Custody"),]

## Note: Outlier analysis is being done at the aggregate level as 
## here it would exclude highly populated LGAs on the basis of their high
## crimes count.

#### NSW Population Density

## Read data 
popdens_source <- read_excel("Pop_Density_06-2016.xlsx")
popdens <- popdens_source

## Format and Filter Data

#Remove unusable variables
popdens [,c(1,3:16)] <- NULL

# Rename variables 
colnames(popdens)[1] <- "LGA"
colnames(popdens)[2] <- "Pop_Dens_km2"


#### 2016 NSW Population

## Read data 
population <- popdens_source

## Format and Filter Data

#Remove unusable variables
population [,c(1,3:12,14:17)] <- NULL

# Rename variables 
colnames(population)[1] <- "LGA"
colnames(population)[2] <- "LGA_Pop"

#### NSW Socio-economic Data

## Read data 
seifa <- read_excel("ABS_SEIFA_LGA_22042019183005088.xlsx")

## Format and Filter Data

# Rename variables 
colnames(seifa)[2] <- "LGA"
colnames(seifa)[4] <- "Index_Type"

# Remove unusable measures
seifa <- seifa[(seifa$Measure %in% c("Score")), ]

# Remove highly correlated variables - keep best proxy
seifa <- seifa[(seifa$Index_Type %in% c("Index of Economic Resources")), ]

#Remove unusable variables
seifa [,c(1,3:8,10:11)] <- NULL

##### Create Measures #####

### Crime data
## Merge Datasets by LGA
crime <- merge(crime,popdens,by="LGA")
crime <- merge(crime,population,by="LGA")

## Create Measure: Per Capita Crime Variable
crime <-  crime %>% 
mutate(Crime_per_Capita=Crim_Count/LGA_Pop)

## Calculate the exponential of Crime_per_Capita
crime$Crime_per_Capita <- exp(crime$Crime_per_Capita)

## Change Per Capita Crime Variable to 7 dps
crime$Crime_per_Capita<-round(crime$Crime_per_Capita, 7)

##### Summarise Data #####

#### Aggregate

### Crime data - per capita crime

## Aggregate
crime_pcap_agg <- crime %>% 
  group_by(LGA) %>% 
  summarise(Crime_per_Capita_Sum=sum(Crime_per_Capita))

## Outlier Analysis

# Visualise Outliers
crime1 <- crime_pcap_agg %>% group_by(LGA) %>% summarise(sum(Crime_per_Capita_Sum))
names(crime1)[2] <- "Count"
boxplot(crime1$Count)

# Set Outlier Criteria
outlier_cutoff_pcap <- quantile(crime1$Count,0.75)+ (1.5 * IQR(crime1$Count))

# Identify Outliers
crime1 <- crime1 %>% filter(Count > outlier_cutoff_pcap)
ggplot(crime1,aes(x=LGA, y = Count)) + geom_point() 

# Omit Outliers
crime_pcap_agg <- crime_pcap_agg %>% filter(Crime_per_Capita_Sum <= outlier_cutoff_pcap)

### Crime data - Crime Count

## Aggregate
crime_count_agg <- crime %>% 
  group_by(LGA) %>% 
  summarise(Crime_Count=sum(Crim_Count))

## Outlier Analysis

# Visualise Outliers
crime2 <- crime_count_agg %>% group_by(LGA) %>% summarise(sum(Crime_Count))
names(crime2)[2] <- "Count"
boxplot(crime2$Count)

# Set Outlier Criteria
outlier_cutoff_count <- quantile(crime2$Count,0.75)+ (1.5 * IQR(crime2$Count))

# Identify Outliers
crime2 <- crime2 %>% filter(Count > outlier_cutoff_count)
ggplot(crime2,aes(x=LGA, y = Count)) + geom_point() 

# Omit Outliers - per capita crime - Crime Count
crime_count_agg <- crime_count_agg %>% filter(Crime_Count <= outlier_cutoff_count)

##### Merge Data #####

#### Merge Datasets by LGA
unified <- merge(crime_pcap_agg,crime_count_agg,by="LGA")
unified <- merge(unified,popdens,by="LGA")
unified <- merge(unified,seifa,by="LGA")
unified <- merge(unified,population,by="LGA")

### Format and Filter Data

## Rename variables 
colnames(unified)[2] <- "Crime_p_Cap"
colnames(unified)[3] <- "Crime_Count"
colnames(unified)[4] <- "Pop_Density"
colnames(unified)[5] <- "Socio_Eco_Ind"
colnames(unified)[6] <- "LGA_Pop"

##### Correlation Analysis #####
cor_matrix <- cor(unified[, c(2:6)], use = 'complete.obs')
corrplot.mixed(cor_matrix, lower = "circle", upper = "number", tl.pos = "lt", diag = "u")

##### Poisson Regression #####
qp_crime_rate <- glm(formula=Crime_p_Cap ~ Crime_Count+Pop_Density+Socio_Eco_Ind+LGA_Pop,
                     family = quasipoisson, data=unified)

qp_crime_count <- glm(formula=Crime_Count ~ Crime_p_Cap+Pop_Density+Socio_Eco_Ind+LGA_Pop,
                      family = quasipoisson, data=unified)
## Create Summary
summary(qp_crime_rate)
summary(qp_crime_count)

## Visualise Results
png("qp_crime_rate.png"); par(mfrow=c(2,2)); plot(qp_crime_rate); dev.off()
png("qp_crime_count.png"); par(mfrow=c(2,2)); plot(qp_crime_count); dev.off()
