# This script is used to determine locations of electrical towers in Bulimba and construct polygons for analysis in Tableau.

Tower_One <- c(-27.447159, 153.051726)
Tower_Tow <- c(-27.448824, 153.063136)


# 8 Circle example at 450m


data = data.frame(
  ID = as.numeric(c(1:2)), # number of points and there coordinates below
  longitude = as.numeric(c(153.063136, 153.051726)),
  latitude = as.numeric(c(-27.448824, -27.447159))
)

#################################################################################
# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 200){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$latitude)
  # length per longitude changes with lattitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

# here is the data frame for all circles
myCircles5 <- make_circles(data, 0.1) # this is in km therefore 0.1 is 100m

library(ggmap)
library(ggplot2)

key<-"AIzaSyCc8qDD_PMFHTQkZb3Pm0sykFqqIXAnX0s" # Set API Key
register_google(key = key) # Register API Key

# Get map
Bulimba <- c(lon = 153.058641, lat = -27.448194)
 
# Get map at zoom level 14
Bulimba_Map <- get_map(location = Bulimba, zoom = 15,
                       maptype = "satellite")

ggmap(Bulimba_Map)

polygons <- read.csv("C:/Users/Jay Blake/Desktop/Brisbane Street Polygons.csv") 
Sales <- read.csv("C:/Users/Jay Blake/Desktop/House Sales Coords.csv") 

# Plot Map with Polygons
ggmap(Bulimba_Map,
      base_layer = ggplot(polygons, aes(Lon, Lat)), extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = Polygon_Name, fill = Polygon_Name), alpha = 0.5) +
  geom_point(data = Sales, aes(x = PLON, y = PLAT, colour = Brisbane.Street), size = 3) +
  scale_x_continuous(limits=c(153.049, 153.065)) +
  scale_y_continuous(limits = c(-27.45, -27.445)) +
  theme(legend.title=element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
  xlab('') +
  ylab('') +
  labs(title = "Brisbane Street, Bulimba and Electrical Tower Locations") +
  guides(fill=FALSE) +
  scale_colour_manual(values=c("slateblue1","darkred"), guide=FALSE)
  


