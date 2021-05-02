###excercise 3- Alissa


#Task 0: Import your data
## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(tidyr)
## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path
#setting remove = FALSE preserves the original (E/N) column:
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

#inputs-----------
#Step a): Specify a temporal window v


#pos[n-2] to pos[n]
#pos[n-1] to pos[n]
#pos[n] to pos[n+1]
#pos[n] to pos[n+2]


#Step b): Measure the distance from every point to every other point within this temporal window v
#lead 1 to the  left and lag 1 to the right

x= c(1:10)
lead(x,1)
lag(x,1)

sabi <- wildschwein_BE %>% 
  filter(DatetimeUTC >= as.Date('2015-07-01 00:00:00') & DatetimeUTC <= as.Date('2015-07-03 00:00:00')) %>% 
  filter(TierName =="Sabi")


sabi <- sabi %>%
  mutate(
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -30 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -15 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +15 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2)  # distance to pos +30 minutes
  )

sabi <- sabi %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus2, nMinus1,nPlus1,nPlus2))
  ) %>%
  ungroup() 

#Step c): Remove all static points 
sabi <- sabi %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

#filter (take) any row which are NOT caled static
sabi_filter <- sabi %>%
  filter(!static)

sabi_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")


#Task 1: Segmentation -------------
caro = read.delim("caro60.csv",sep=",",dec=".",header=TRUE)
#Data cleansing
#set timezone
caro$DatetimeUTC <- as.POSIXct(as.character(caro$DatetimeUTC), format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC")
is.data.frame(caro)
#make a temporal window v of 6 minutes, i.e. a window size of 6 positions (n±3).

caro_frame <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -3 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3 = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2),   # distance to pos -3 minutes
  )

#task 2

caro_frame <- caro_frame %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3,nMinus2, nMinus1,nPlus1,nPlus2,nPlus3))) %>%
  ungroup()
#create boxplot 
ggplot(caro_frame)+
  geom_boxplot(mapping = aes(,stepMean, na.rm= TRUE))

#create histogramm
ggplot(caro_frame)+
  geom_histogram(mapping = aes(stepMean, na.rm= TRUE))

#choose a threshold between stops and moves --> depending on data and question!
#In this exercise, we use the mean of all stepMean values:

#Step c): Remove all static points (= stops)
#all points which are bigger than the mean of stepMean are defined as static (moving)
caro_frame <- caro_frame %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))


