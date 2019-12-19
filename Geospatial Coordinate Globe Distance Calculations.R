

# Function for Coordinate Straight Line Distance on the Globe -------------


library(geosphere)
distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)

# Example
distm (c(153.003193, -27.452894), c(153.042629, -27.479643), fun = distHaversine)


# Others
library(sp)
spDistsN1() 

# More others
distHaversine()
distMeeus()
distRhumb()
distVincentyEllipsoid()
distVincentySphere()

Distance_Dataframe <- House_Data1_Wcoords %>%
  select(1,17:20)

# Calculate Distance as new variable
Distance_Dataframe <- Distance_Dataframe %>% rowwise() %>% 
  mutate(Distance = round(distHaversine(c(H_Lon, H_Lat), c(B_Lon, B_Lat))/1000,3))