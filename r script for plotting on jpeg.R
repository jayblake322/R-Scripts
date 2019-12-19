library(jpeg)
library(rasterImage)
library(grid)
library(ggplot2)
library(ggrepel)

                      
# Load the data for the plot                   
                      
img <- readJPEG("Inner Brisbane Map.jpg")
                      
xxx <- ggplot(xy_coords, aes(x,y)) + 
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                                          -Inf, Inf, -Inf, Inf) +
      geom_point(aes(x,y, size = c, colour = "red")) +
  labs(title = "Inner Brisbane Interstate Buyer Analysis",
       x = NULL,
       y = NULL) 
xxx + geom_label_repel(aes(label = filter(xy_coords$Locality),
      box.padding   = 0.35, 
      point.padding = 0.5,
      segment.color = 'grey50') 



#geom_text_repel(data          = subset(nba, PTS > 25),
 #               nudge_y       = 32 - subset(nba, PTS > 25)$PTS,
  #              segment.size  = 0.2,
   #             segment.color = "grey50",
    #            direction     = "x")