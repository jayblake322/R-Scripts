library(readxl)
library(treemapify)
library(ggplot2)
library(ggplotify)
library(reshape2)
library(plyr)
library(dplyr)
library(scales)
library(RColorBrewer)


rental <- read_excel("C:/Users/Jay Blake/Desktop/Rental Update June 2019.xlsx", sheet = 3)
names <- c("V1", "V2", "V3", "V4")
names(rental) <- names

glimpse(rental)
rental$Precinct <- as.factor(rental$Precinct)

treeMapPlot <- ggplot(rental, aes(area = V2, 
                                  fill = V4,
                                  label = V1, 
                                  subgroup = V4)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white") +
  geom_treemap_subgroup_text(place = "centre",
                             grow = T,
                             colour = "#FAFAFA",
                             min.size = 0, 
                             alpha = 0.5) +
  geom_treemap_text(fontface = "italic",
                    colour = "white",
                    place = "centre",
                    grow = F,
                    reflow = T, 
                    alpha = 0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Number of Vacanct Properties in Brisbane: June 2019")

treeMapPlot
