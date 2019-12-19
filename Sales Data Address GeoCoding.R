library(ggmap) # Load Library
library(dplyr)
key<-"AIzaSyCc8qDD_PMFHTQkZb3Pm0sykFqqIXAnX0s" # Set API Key
register_google(key = key) # Register API Key

# Load test data
geocode_data <- read.csv("Definite Investor Address Inner Ring Houses Sales 10 Years to September 2018.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Check the data classes
glimpse(geocode_data)


# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# geocode_test_data data frame in new columns lat and lon

for(i in 1:nrow(geocode_data))
{
  # Print("Working...")
  result <- geocode(geocode_data$Purchaser.Address[i], output = "latlona", source = "google")
  geocode_data$lon[i] <- as.numeric(result[1])
  geocode_data$lat[i] <- as.numeric(result[2])
  geocode_data$geoAddress[i] <- as.character(result[3])
}

# Write a CSV file containing origAddress to the working directory
write.csv(geocode_data, "geocoded house investor123.csv", row.names=FALSE)
