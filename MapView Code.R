library(mapview)
library(sf)

data <- read.csv("Insert File Path Here", sep = ",", header = TRUE)

data <- data %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(data)
        #zcol = "Status",
        #legend = TRUE , # add a legend
        #layer.name = "Profit & Loss") # label the legend)
