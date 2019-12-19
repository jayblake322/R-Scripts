# This script creates the visualisations for section one of the body of the Buyer Sentiment Report for 2019.

source("Market Overall.R")
library(ggplot2)
library(arulesViz)
library(scales)
library(cowplot)

# Market Performance Past 12 Months ---------------------------------------

last_year_sentiment <- ggplot(data = Mimprove_summ2, aes(reorder(response, percent), percent, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() + # clear unnessecary clutter
  theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "bottom") + # remove further clutter and adjsut legend
  scale_y_continuous(name = "Proportion of Respondents", labels = percent_format(accuracy = 1L)) + # format y-axis
  scale_fill_brewer(palette = "Blues", labels = c("2018 Responses", "2019 Responses"))  # colour and change legend values


last_year_sentiment

# Market Next 12 Months ---------------------------------------------------

market_improv <- ggplot(data =  next_year, aes(X6, perc)) +
  geom_bar(stat = "identity", fill = "lightskyblue3") +
  theme_classic() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(name = "Proportion of Respondents", labels = percent_format(accuracy = 1L)) 
market_improv

# Biggest Concern ---------------------------------------------------------

ggplot(data = big_concern, aes(reorder(X21, -perc), perc)) +
  geom_bar(stat = "identity")


# Property Type and Where -------------------------------------------------


ggplot(ProType2019, aes(reorder(category, perc), perc)) +
  geom_point(cex = 5.5, col = "skyblue3") +
  geom_point(data = ProType2019, aes(category, type_2018), col = "red", cex = 5.5, alpha = 0.1) +
  geom_segment(aes(x = category, xend = category, y = 0, yend = perc), col = "grey59", lwd = 1) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12), legend.title = element_blank(), legend.position = "none") +
  scale_y_continuous(name = "Proportion of Respondents", labels = percent_format(accuracy = 1L), limits = c(0, 0.2)) 


# Close to What? ----------------------------------------------------------

plot(extract1, method = "graph")


# When Looking to Buy -----------------------------------------------------

xaxis <- c("Unsure", "Not interested in purchasing property", "More than 5 Years", "5 Years",  "2 Years", "12 Months", "6 Months")

ggplot(data = When, aes(X4, custom_cumsum)) +
  geom_bar(stat = "identity", fill = "lightskyblue3") + 
  geom_bar(aes(X4, perc), fill = "red", stat = "identity", alpha = 0.2) +
  coord_flip() +
  scale_x_discrete(limits = xaxis) +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15)) + # remove further clutter and adjsut legend
  scale_y_continuous(name = "Proportion of Respondents", labels = percent_format(accuracy = 1L))  # format y-axis
  scale_fill_manual(value = "Blue", labels = "Actual Responses") 



