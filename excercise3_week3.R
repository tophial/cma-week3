###excercise 3- Alissa
#inputs-----------
#Step a): Specify a temporal window v


#pos[n-2] to pos[n]
#pos[n-1] to pos[n]
#pos[n] to pos[n+1]
#pos[n] to pos[n+2]


#Step b): Measure the distance from every point to every other point within this temporal window v

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

#Step c): Remove “static points”
sabi <- sabi %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))


sabi_filter <- sabi %>%
  filter(!static)

sabi_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")


#Task 1: Segmentation -------------
