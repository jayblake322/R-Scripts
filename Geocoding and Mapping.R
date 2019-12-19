############ Geocoding ###########

### MY API KEY  ### AIzaSyCc8qDD_PMFHTQkZb3Pm0sykFqqIXAnX0sE ###

##### Check monthly limit $200 free per month. Approximately 40,000 address or 28,000 static maps

#### Check pricing at https://cloud.google.com/maps-platform/pricing/sheet/

#### Check Account at https://console.cloud.google.com/google/maps-apis/overview?consoleUI=CLOUD&project=geocoding-property-sales


#### Useful course at https://campus.datacamp.com/courses/working-with-geospatial-data-in-r/basic-mapping-with-ggplot2-and-ggmap?ex=1


# Mandatory Set Up --------------------------------------------------------

library(ggmap) # Load Library
key<-"AIzaSyCc8qDD_PMFHTQkZb3Pm0sykFqqIXAnX0s" # Set API Key
register_google(key = key) # Register API Key

# Load A Map --------------------------------------------------------------

# Obtain coordinates 
geocode("Brisbane")

# Set coordinates
brisbane <- c(lon = 153.0251,lat = -27.46977)

# Get map of coordinates
brisbane_map <- get_map(brisbane, zoom = 15, scale = 1)

# Show map
ggmap(brisbane_map)

# Batch Geocode Address ---------------------------------------------------

library(readxl)
# Load test data
geocode_test_data <- read.csv("R:/Projects/Market Analysis and Database Records/Sales Data/Sales Data CSV Files/Geocoded Unit Buyer Address Inner Ring 10 Years to September 2018.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
dim(geocode_test_data)

#geocode_test_data$Unit_Buyer_Address <- geocode_test_data$ 

gedata <- unique(geocode_test_data$Unit_Buyer_Address)
gedata1 <- as.data.frame(gedata)
names(gedata1) <- "Unit_Buyer_Address"
gedata1$Unit_Buyer_Address <- as.character(gedata1$Unit_Buyer_Address)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# geocode_test_data data frame in new columns lat and lon

for(i in 1:nrow(geocode_test_data))
{
  # Print("Working...")
  result <- geocode(gedata1$Unit_Buyer_Address[i], output = "latlona", source = "google")
  gedata1$lon[i] <- as.numeric(result[1])
  gedata1$lat[i] <- as.numeric(result[2])
  gedata1$geoAddress[i] <- as.character(result[3])
}

# Write a CSV file containing origAddress to the working directory
write.csv(gedata1, "geocoded test output.csv", row.names=FALSE)

getwd()
