
https://blog.rstudio.com/2015/04/14/interactive-time-series-with-dygraphs/
  


library(dplyr)
library(dygraphs)

dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector()


dygraph(nhtemp, main="New Haven Temperatures") %>%
  dySeries(label="Temp (F)", color="black") %>%
  dyShading(from="1920-1-1", to="1930-1-1", color="#FFE6E6") %>%
  dyShading(from="1940-1-1", to="1950-1-1", color="#CCEBD6")
