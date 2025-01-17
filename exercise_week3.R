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

wildschwein_BE <- read_delim("wildschwein_BE.csv",",") # adjust path
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

#task 3- Visualize segmented trajectories


caro_frame%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour = static)) +
  theme(legend.position = "right")



#task 4 - Segment-based analysis--------------

#we need a unique ID for each segment that we can use as a grouping variable
#function which assign unique IDs based on the column static: 



rle_id <- function(vec){
  #Run Length Encoding - compute the lengths and values of runs equal values in a vector - lenghts: 	 an integer vector containing the length of each run
  x <- rle(vec)$lengths 
  as.factor(rep(seq_along(x), times=x))
}


#use the newly created function rle_id to assign unique IDs to subtrajectories

caro_frame <- caro_frame %>%
  mutate(segment_id = rle_id(static))


#visualize them by coloring segment_ID (with all moving points; static == FALSE)


caro_f <-caro_frame%>%
  filter(static == FALSE) %>% 
  ggplot(aes(E, N))  +
  geom_path(aes(colour = segment_id)) +
  geom_point(aes(colour = segment_id)) +
  coord_fixed() +
  theme(legend.position = "none")

caro_f

#removed all segments n < 5



caro_seg <- caro_frame %>% 
  group_by(segment_id) %>% 
  mutate(
    duration = as.integer(difftime(max(DatetimeUTC),min(DatetimeUTC),"mins"))
  ) %>% 
  filter(static == FALSE) %>%
  filter(duration >= 5) %>% 
  ggplot(aes(E, N))  +
  geom_path(aes(colour = segment_id)) +
  geom_point(aes(colour = segment_id)) +
  coord_fixed() +
  theme(legend.position = "none")

caro_seg

#Task 5: Similarity measures-----------

pede = read.delim("pedestrian.csv",sep=",",dec=".",header=TRUE)
#Data cleansing
#set timezone
pede$DatetimeUTC <- as.POSIXct(as.character(pede$DatetimeUTC), format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC")
is.data.frame(pede)

#first trial
ggplot(data=pede, aes(E,N))+
  geom_point(aes(alpha=TrajID))+
  geom_point(aes(colour=as.factor(TrajID)))+
    facet_wrap(~TrajID, labeller = labeller(TrajID = 
                                            c("1" = "TrajID: 1",
                                              "2" = "TrajID: 2",
                                              "3" = "TrajID: 3",
                                              "4" = "TrajID: 4",
                                              "5" = "TrajID: 5",
                                              "6" = "TrajID: 6")))+
  labs(title = "Visual Comparison of the 6 trajectories",
       subtitle = "Each subplot highlights a trajectory",
       caption = "Data source: pedestrian.csv")+
  theme(legend.position = "none")
#now plot with geom_point with all trajectories in background with alpha 0.1
ggplot(pede, aes(E,N)) +                                                                                   
  geom_point(data = dplyr::select(pede, -TrajID),alpha = 0.1) +                                      
  geom_point(aes(color = as.factor(TrajID)), size = 1) +                                                   
  geom_path(aes(color = as.factor(TrajID))) +                                                               
  facet_wrap(~TrajID,labeller = label_both) +                                                               
  coord_equal() +                                                                                           
  theme_minimal() +                                                                                         
  labs(title = "Visual Comparison of the 6 trajectories",
      subtitle = "Each subplot highlights a trajectory",
      caption = "Data source: pedestrian.csv")+
  theme(legend.position = "none")


#Task 6 Calculate similarity

library("SimilarityMeasures")
help(package = "SimilarityMeasures")
#https://cran.r-project.org/web/packages/SimilarityMeasures/SimilarityMeasures.pdf
#Attention: All functions in the package need matrices as input, with one trajectory per matrix.

#DTW-----------

#first make matrix for each traj 1-6 

matrix_traj <- function(traj_ID){
  pede_filtered <- pede%>% 
    filter(TrajID ==traj_ID) %>% 
    dplyr::select(E,N)
  
  pede_filtered_matrix <-as.matrix(pede_filtered)
  return(pede_filtered_matrix)
}

#leerer Container bzw. leere Liste erstellen, welche in For-loop gefüllt wird
traj_vec <- list()


for (val in c(1:6)){
  traj_vec[[val]]<- matrix_traj(val) #doppel Eckklammer, da 2 spalten
  
}
traj_vec


#function to compare traj1 with traj2-6

similarities_fun <- function (traj_a, traj_b){
  dynamic_time_wraping_f <- DTW(traj_a, traj_b, pointSpacing=-1) 
  edit_distance_f <- EditDist(traj_a, traj_b, pointDistance=20)
  frechet_f <- Frechet(traj_a, traj_b, testLeash=-1)
  #The accuracy of the algorithm (pointSpacing = ,pointDistance = and errorMarg =) can be varied to provide faster calculations. Please see Vlachos, Gunopoulos, and Kollios (2002) for more information.
  LCSS_f <- LCSS(traj_a, traj_b, pointSpacing=-1, pointDistance=100,errorMarg=2, returnTrans=FALSE)
  return(list(dynamic_time_wraping_f, edit_distance_f, frechet_f))
}

#leerer Container erstellen 

similar_list <- list()

#vergleiche alle trajectories mit 1. trajectory

for (val in c(1:6)){
  similar_list[[val]]<-similarities_fun(traj_vec[[1]], traj_vec[[val]])
}


#create data.frame from list

df <- data.frame(num=unlist(similar_list))

vec_names <- c(rep("DTW", 6), rep("ED", 6), rep("Fr", 6), rep("LCSS", 6))
vec_ID <-c(rep(c(1:6),4))

df <-cbind(df, vec_names)
df <-cbind(df, vec_ID )


# plot of similarity values

  ggplot(data=df, aes(vec_ID,num)) +
  geom_col(aes(fill = as.factor(vec_ID)))+
  theme(legend.position = "none")+
  facet_wrap(~vec_names, nrow = 2, scales = "free")
  
  





