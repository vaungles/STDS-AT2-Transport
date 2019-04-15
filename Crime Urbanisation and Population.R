#### Set working directory
rm(list=ls())
setwd("H:/V/UTS/0. MDSI/36103 Statistical Thinking for Data Science/AT2/Working Files")

#### Load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

#### Load, Format and Create Data

### NSW Load Crime Statistics

## Read data 
library(readxl)
crimedata <- read_excel("NSW Recorded Crime Statistics January 2014 to December 2018.xlsx")

## Format and Filter Data

# Rename variables 
colnames(crimedata)[1] <- "LGA"
colnames(crimedata)[2] <- "Offence"
colnames(crimedata)[3] <- "Count_2014"
colnames(crimedata)[4] <- "Count_2015"
colnames(crimedata)[5] <- "Count_2016"
colnames(crimedata)[6] <- "Count_2017"
colnames(crimedata)[7] <- "Count_2018"

# Remove unusable variables
crimedata [,8:10] <- NULL

# Omit records where LGA is not LGA
crimedata <-crimedata[!(crimedata$LGA == "In Custody"),]

# Omit records with missing values
crimedata <- crimedata[complete.cases(crimedata), ]


### Load NSW Population by Dwellings Statistics (Dwellings)

## Read data
dwellings <- read_excel("ABS Census 2016 Dwelling Structure and Type by Geographical Area.xlsx")

## Format and Filter Data

# Rename variables 
colnames(dwellings)[1] <- "LGA"
colnames(dwellings)[5] <- "Dwelling_Structure"
colnames(dwellings)[13] <- "Count"

# Remove unusable Dwelling Type records
dwellings <- dwellings[!(dwellings$Dwelling_Structure %in% c("Total", 
                                                             "Cabin, houseboat",
                                                             "Caravan",
                                                             "Not stated",
                                                             "Not appliacable",
                                                             "House or flat attached to a shop, office, etc.",
                                                             "Improved home, tent, sleepers out")), ]

# Remove records for all states other than NSW
dwellings <-dwellings[!(dwellings$STATE != 1),]

#Remove unusable variables
dwellings [,2:4] <- NULL
dwellings [,3:9] <- NULL
dwellings [,4:5] <- NULL

## Add Calculations

# Urbanisation Weights
dwellings <- dwellings %>% mutate(urban_weight = case_when(
  Dwelling_Structure == "Separate house" ~ 1,
  Dwelling_Structure == "Semi-detached, row or terrace house, townhouse etc. with : One storey" ~ 2,
  Dwelling_Structure == "Flat or apartment: Attached to a house" ~ 2,
  Dwelling_Structure == "Semi-detached, row or terrace house, townhouse etc. with : Two or more storeys" ~ 3,
  Dwelling_Structure == "Flat or apartment: In a one or two storey block" ~ 4,
  Dwelling_Structure == "Flat or apartment: In a three storey block" ~ 5,
  Dwelling_Structure == "Flat or apartment: In a four or more storey block" ~ 6))

# Create Urbanisation Adjusted Count
dwellings <-  dwellings %>% 
  rowwise() %>% 
  mutate(UrbAdjCount=urban_weight*Count)

### Load Population Data by LGA
population <- read_excel("Population by LGA.xlsx")

## Rename variables 
colnames(population)[17] <- "2016_Pop"

## Remove unusable variables
population [,2:16] <- NULL

#### Summarise Data

### Summarise Crime Data

## Aggregate
crimeagg <- crimedata %>% group_by(LGA) %>% 
  summarise(Tot_14=sum(Count_2014), 
            Tot_15=sum(Count_2015),
            Tot_16=sum(Count_2016),
            Tot_17=sum(Count_2017),
            Tot_18=sum(Count_2018))

## Add Calculations

# Create a Mean for Years 14-18
crimeagg <-  crimeagg %>% 
  rowwise() %>% 
  mutate(MeanCrime=mean(c(Tot_14,Tot_15,Tot_16,Tot_17,Tot_18)))

### Summarise Dwellings data

## Aggregate

dwellagg <- dwellings %>% group_by(LGA) %>% 
  summarise(TotUrbAdjCount=sum(UrbAdjCount))

## Order
dwellagg <- arrange(dwellagg, desc(TotUrbAdjCount))

#### Merge Data

### Add Population to Crime Data LGAs
agg_crime_pop <- merge(crimeagg,population,by="LGA")

## Create a Per Capita Crime Mean
agg_crime_pop <-  agg_crime_pop %>% 
  rowwise() %>% 
  mutate(p_cap_mean=MeanCrime/`2016_Pop`)

## Change Per Capita Crime Mean to 4 dps
agg_crime_pop$p_cap_mean<-round(agg_crime_pop$p_cap_mean, 4)

## Order dataset by Per Capita Crime Mean
agg_crime_pop <- arrange(agg_crime_pop, desc(p_cap_mean))

## Delete other variables
agg_crime_pop [,2:8] <- NULL

## Delete duplicate record
agg_crime_pop <- agg_crime_pop [-59,]

### Add p_cap_mean to aggregated Dwellings data
agg_DCP <- merge(agg_crime_pop,dwellagg,by="LGA")
names(agg_DCP)

## Rename variables 
colnames(agg_DCP)[2] <- "Crim_Ind"
colnames(agg_DCP)[3] <- "Urb_Ind"

## Order
agg_DCP <- arrange(agg_DCP, desc(Urb_Ind))

## Rename data
dcp <- agg_DCP


#### Visualise 

#Basic Plot
p <- ggplot(dcp, aes(x = Urb_Ind, y = Crim_Ind)) +
  geom_point() +
  ggtitle("Crime and Urbanisation of Local Government Areas") +
  scale_x_continuous(name = "urbanisation index") +
  scale_y_continuous(name = "crime index")

# Reformat to remove scientifc notation
p <- ggplot(dcp, aes(x = Urb_Ind, y = Crim_Ind)) +
  geom_point() +
  ggtitle("Crime and Urbanisation of Local Government Areas") +
  scale_x_continuous(name = "urbanisation index (thousand)",
                     labels = scales::number_format(scale = 1E-3)) +
  scale_y_continuous(name = "crime index")

# Remove outliers with Crime Index above 0.25
dcp_subset <- dcp %>%
  filter(Crim_Ind < 0.25)

p <- ggplot(dcp_subset, aes(x = Urb_Ind, y = Crim_Ind)) +
  geom_point() +
  ggtitle("Crime and Urbanisation of NSW Local Government Areas") +
  scale_x_continuous(name = "urbanisation index (thousand)",
                     labels = scales::number_format(scale = 1E-3)) +
  scale_y_continuous(name = "crime index")

dcp_subset <- dcp %>%
  filter(Crim_Ind < 0.25)

# Add smoothing line
p <- ggplot(dcp_subset, aes(x = Urb_Ind, y = Crim_Ind)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Crime and Urbanisation of NSW Local Government Areas") +
  scale_x_continuous(name = "urbanisation index (thousand)",
                     labels = scales::number_format(scale = 1E-3)) +
  scale_y_continuous(name = "crime index") 

p

write.csv(dcp,file="dcp.csv")
