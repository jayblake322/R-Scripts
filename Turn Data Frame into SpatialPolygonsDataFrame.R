# Load Library
library(sp)

##### REMEMBER TO DOUBLE CHECK THE ORDER OR THE POLYGONS, NAMES AND ID ARE INPUT CORRECTLY SO THAT POLYGON ID AND NAME HAVE THE RIGHT COORDINATES

suburbs <- read.csv("Brisbane Suburb Names with List.csv") # Need a list of unique Polygon ID names with an ID number corresponding to them
names(suburbs) <- c("Names", "Number") # Set Names 

# join the ID Names of each polygon to the dataframe with the coordinates 
brisbane_polygonsx <- left_join(brisbane_polygons, suburbs, by = c("SA2_Name" = "Names"))

#extract values for list
ID <- brisbane_polygonsx$Number # make sure its an integer
Lat <- brisbane_polygons$Lat # make sure its numeric
Lon <- brisbane_polygons$Lon # make sure its numeric
Label <- as.character(suburbs$Names) # make sure its character

# Create list of data from data frame

brisbane_polygons_list <- structure(list(lon = Lon, 
                                         lat = Lat,  
                                         id = structure(ID, 
                                         .Label = Label,
                                         class = "factor")),
                                         .Names = c("lon", "lat", "id"), 
                                         row.names = c(NA, -8218), 
                                         class = "data.frame")
    
dput(brisbane_polygons_list)  

# make a list
brisbane_list <- split(brisbane_polygons_list, brisbane_polygons_list$id)

# only want lon-lats in the list, not the names
brisbane_list <- lapply(brisbane_list, function(x) { x["id"] <- NULL; x })

# create SpatialPolygons Object, convert coords to polygon
ps <- lapply(brisbane_list, Polygon)

# add id variable 
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(brisbane_list)[i]  ))

# create SpatialPolygons object
brisbane_spatial_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") )  

brisbane_spatial_polys_df <- SpatialPolygonsDataFrame(brisbane_spatial_polys, 
                                                data.frame(id = unique(brisbane_polygons_list$id), 
                                                           row.names = unique(brisbane_polygons_list$id)))

plot(brisbane_spatial_polys_df)



