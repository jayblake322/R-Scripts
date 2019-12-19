#This code is from a tutorial that can be found here : https://rpubs.com/michaeldgarber/r_gis_transplant
## it uses mapview for interactive maps

library(sf)
library(tidyverse)
library(mapview)
library(devtools)
library(ggmap)
library(readxl)
library(lwgeom)

#column types for 'Selected Facilities for QIP'
col_types_dialfacil = c("numeric", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric")

#column types for Network 6 transplant centers
col_types_transpcenter = c("text", "text", "text", "text", "text", "text", "text")

#load facilities Excel sheet
facilities <- read_excel("N of 440 Final Dialysis Facility Population_For Maps.xlsx", 
                         sheet = "Selected Facilities for QIP", range = "A1:O441",
                         col_names = TRUE, #first row is the column name
                         col_types = col_types_dialfacil) #the vector specifying column types

#load dialysis center Excel sheet
centers <- read_excel("N of 440 Final Dialysis Facility Population_For Maps.xlsx", 
                      sheet = "Network 6 Transplant Centers", range = "A1:G10",
                      col_names = TRUE, #first row is the column name
                      col_types = col_types_transpcenter) #the vector specifying column types

names(facilities)
names(centers)
dim(facilities)
dim(centers)

View(facilities)
View(centers)

facilities_1 = facilities %>%
  #skip the PHYS_SECOND_LINE column
  mutate(facility_address = paste0(PHYS_FIRST_LINE, ", ",   PHYS_CITY, ", " ,   PHYS_STATE, ", ",   PHYS_ZIP_CODE))

#check
View(facilities_1)

centers_1 = centers %>%
  mutate(center_id = row_number()) %>%
  
  #can make a new variable that represents the fixed addresses.
  #I'm using case_when syntax, which is similar to an if-then statement in SAS.
  mutate(center_address = case_when(
    #if center_id is 7, fix the address.
    center_id == 7        ~  "160 Dental Cir, Chapel Hill, NC 27514",
    
    #if else, set to the original values.
    TRUE                  ~  Center_Full
  ))

#check
#View(centers_1)

facilities_GA = facilities_1 %>%
  filter(PHYS_STATE == "GA") #note the == rather than =

dim(facilities_GA) #152 rows, so 152 facilities in GA

count_facilities_by_state = facilities_1 %>%
  #now, every row is a state.
  group_by(PHYS_STATE) %>%
  #perform some operations for those rows.
  summarise(
    state_count = n()
  )

print(count_facilities_by_state) #152 in Georgia


register_google(key = "AIzaSyCOYQyC4ioRocHqbi_xyDfQ-3tMrPQvbMo")
#Note, if this didn't work, try installing the develoment version of ggmap. See section 2.1.

geocodeQueryCheck() #limit is 2500 per day


by_facility_geocode  = facilities_1 %>%
  dplyr::select(CCN, facility_address) %>%
  
  #the function to perform the geocode
  mutate_geocode(facility_address, force = TRUE) %>%
  
  #drop the address field to avoid duplication when it's merged back with the original data
  dplyr::mutate(facility_address = NULL)

#Check to make sure they were all geocoded.
View(by_facility_geocode)

##### DID NOT GEOCODE
###### to put in coordinates

lat <- building_data_clean[1:440,4]
lon <- building_data_clean[1:440,5]

by_facility_geocode$lat <- lat
by_facility_geocode$lon <- lon

by_center_geocode  = centers_1 %>%
  dplyr::select(center_id, center_address) %>%
  mutate_geocode(center_address, force = TRUE) %>%
  dplyr::mutate(center_address = NULL)

lat1 <- building_data_clean[1:9,4]
lon1 <- building_data_clean[1:9,5]

by_center_geocode$lon <- lon1
by_center_geocode$lat <- lat1

#Check to make sure they were all geocoded.
View(by_center_geocode)
3.2. Save the geocoded f


save(by_facility_geocode, file = "by_facility_geocode.RData")
save(by_center_geocode, file = "by_center_geocode.RData")

load(file = "by_facility_geocode.RData")
load(file = "by_center_geocode.RData")

by_facility_sf = by_facility_geocode %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  left_join(facilities_1, by = "CCN") 

by_center_sf = by_center_geocode %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  left_join(centers_1, by = "center_id") 

mapview(by_facility_sf)
mapview(by_center_sf)

mapview(by_facility_sf, map.type = "OpenStreetMap", 
        color = "orangered", col.regions = "orangered") +
  mapview(by_center_sf, map.type = "OpenStreetMap", 
          color = "blue", col.regions = "blue") 

mapview(by_facility_sf, map.type = "OpenStreetMap", cex = "Patient_Census2", legend = FALSE)

mapview(by_facility_sf, 
        map.type = "OpenStreetMap", 
        zcol = "Treatment",
        legend =TRUE ,#add a legend
        layer.name = "Intervention facilities (1); No (0)" #label the legend
)


by_facility_sf1 = by_facility_sf %>% 
  
  #sort the data by CCN ascending.
  arrange(CCN) %>%
  
  mutate(facility_id = row_number())

fac_distance_matrix = st_distance(by_facility_sf1, by_element = FALSE)

#Convert it to a data frame (a tibble).
fac_distance_df = as_tibble(fac_distance_matrix)

#convert to numeric. Units are meters.
#the 1 means apply the function row wise. you can say 2 for column wise. I don't think it would make a difference, here. See this blog post for info about the apply family of functions https://www.r-bloggers.com/apply-lapply-rapply-sapply-functions-in-r/
fac_distance_df_numeric_m = as_tibble(apply(fac_distance_df, 1, as.numeric))

#There are zeros along the middle diaganol. We need to set them all to NA. Otherwise, the minimum for every facility will be zero.
fac_distance_df_numeric_m[fac_distance_df_numeric_m==0] <- NA

#Find the minimum of each column using sapply. using sapply because I want a vector in return.
min_fac_distance = sapply(fac_distance_df_numeric_m, min, na.rm = T)

#Now convert it to a tibble (tidy table) and do some more tidying up.
min_fac_dist_tibble = min_fac_distance %>%
  as_tibble() %>%
  mutate(facility_id = row_number()) %>%
  rename(min_distance_m = value) %>%
  
  #create new variables for min distance in kilometers and miles
  mutate(min_distance_km = min_distance_m / 1000,
         min_distance_mi = min_distance_m / 1609.34
  )

by_facility_sf2 = by_facility_sf1 %>%
  left_join(min_fac_dist_tibble, by = "facility_id")

#Look at the new columns we made
by_facility_sf2 %>%
  dplyr::select(facility_id, min_distance_m, min_distance_mi, min_distance_km) %>%
  View()

#and check on the map.
mapview(by_facility_sf2)

trans_centers_one  = by_center_sf %>% st_union(by_feature=FALSE)

mapview(trans_centers_one)


#calculate distance between each facility and the single feature
dist_fac_center_440x1 = st_distance(by_facility_sf2, trans_centers_one)

#Is this the same as the lowest value in each row of the 440x9 matrix?
dist_fac_center_440x9 = st_distance(by_facility_sf2, by_center_sf)
#View(dist_fac_center_440x1)
#View(dist_fac_center_440x9) #yes.

min_cent_dist_tibble = dist_fac_center_440x1 %>%
  as_tibble() %>%
  mutate(facility_id = row_number()) %>%
  #the variable is V1 this time rather than 'value'. 
  #take away the units and then delete the V1 variable.
  mutate(min_distance_center_m = as.numeric(V1), #######################ERROR##########CANNOT FIND V1 OBJECT
         v1 = NULL) %>%
  
  #min distance in kilometers and miles
  mutate(min_distance_center_km = min_distance_center_m / 1000,
         min_distance_center_mi = min_distance_center_m / 1609.34
  )
