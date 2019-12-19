library(scales)   
scale_y_continuous(labels = scales::percent) # option 1 with library - scales
scale_y_continuous(labels = function(x) paste0(x*100, "%")) # option 2 - no decimals