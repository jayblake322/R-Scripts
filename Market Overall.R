# This script carries out the analysis and data manipulation for section one of the body of the Buyer Sentiment Report for 2019.


# source data load and preprocessing

source("LoadDataPreProcess.R")
library(tidyr)
library(stringr)
library(arules)


# Market Improvement Last 12 Months? --------------------------------------

# Market compared to the previous 12 months
Mimprov2019 <- table(Res2019$X5)
Mimprov2018 <- table(Res2018$X4)

# Set the same names
names <- names(Mimprov2019)[4:2]
names(Mimprov2018) <- names

# Set as Dataframes
Mimprov2019 <- as.data.frame(Mimprov2019)
Mimprov2018 <- as.data.frame(Mimprov2018)

  
# Combine
MimprovComp <- rbind(Mimprov2019, Mimprov2018) %>%
  mutate(Year = as.factor(c(rep(2018, 4), rep(2019, 3))))

# spread and assign better colume names
Mimprove_summ <- spread(MimprovComp, Year, Freq) 
names(Mimprove_summ) <- c("response", "num2019", "num2018") 
  
# replace the nas
Mimprove_summ[1, 3] <- 0

# summarise/calculate percentages/structure for plot  
Mimprove_summ <- Mimprove_summ %>%
  mutate(perc2018 = num2018/sum(num2018)) %>%
  mutate(perc2019 = num2019/sum(num2019)) %>%
  select(1,4:5) %>%
  gather(year, percent, 2:3)
  
# manual adjust for graph manipulation
Mimprove_summ2 <- Mimprove_summ[-1, ] %>%
  mutate(response = as.character(response))


Mimprove_summ2[c(1, 4), 1] <- "The Same or Better"
Mimprove_summ2[4, 3] <-  Mimprove_summ2[4, 3] + Mimprove_summ2[5, 3]
Mimprove_summ2 <- Mimprove_summ2[-5, ] %>%
  mutate(response = as.factor(response))




# Market Next 12 Months ---------------------------------------------------

table(Res2019$X6)

next_year <- Res2019 %>%
  group_by(X6) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n))
             
# Biggest Concern ---------------------------------------------------------

table(Res2019$X21)

big_concern <- Res2019 %>%
  group_by(X21) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n))

big_concern <- big_concern[-1, ] # remove na 

# Preferred Property  -----------------------------------------------------

table(Res2019$X9)

ProType2019 <- Res2019 %>%
  group_by(X8, X9) %>%
  summarise(n = n()) %>%
  filter(X8 != "") %>%
  mutate(perc = n/nrow(Res2019)) %>%
  mutate(category = paste(as.character(X8), " ", as.character(X9))) %>%
  filter(perc >= 0.05)

Pro_Type_2018 <- Res2018 %>%
  group_by(X7, X8) %>%
  summarise(n = n()) %>%
  mutate(perc = n/nrow(Res2018)) %>%
  mutate(category = paste(as.character(X7), " ", as.character(X8))) %>%
  filter(perc >= 0.05)

ProType2019$type_2018 <- 0
ProType2019[1, 6] <- Pro_Type_2018[7, 4]
ProType2019[2, 6] <- Pro_Type_2018[8, 4]
ProType2019[3, 6] <- Pro_Type_2018[1, 4]
ProType2019[4, 6] <- Pro_Type_2018[2, 4]
ProType2019[5, 6] <- Pro_Type_2018[3, 4]
ProType2019[6, 6] <- Pro_Type_2018[6, 4]


table(Res2019$X16)
table(Res2019$X17)
table(Res2019$X19)


# Close to What ------------------------------------------------------

table(Res2019$X13)

# Split locational drivers and put into a list of lists 
Locational_Drivers <- str_split(Res2019$X13, ";")
Local_trans_obj <- as(Locational_Drivers, "transactions")

# inspect object
summary(Local_trans_obj)
image(Local_trans_obj)

extrac_main <- apriori(Local_trans_obj)
# Extract information - Support atleast 20%
extract1 <- apriori(Local_trans_obj, parameter = list(support = 0.25, target = "frequent itemsets"))
inspect(sort(extract1, by = "support"))

# Extract information - Support atleast 20% and confidence atleast 40%
extract2 <- apriori(Local_trans_obj, parameter = list(support = 0.2, confidence = 0.4, minlen = 2, target = "rules"))
inspect(sort(extract2, by = "confidence"))

# only public transport on the left hand side
extract3 <- subset(extrac_main, subset = lhs %ain% c("Proximity to public transport"))
inspect(extract3)

# Purchase When -----------------------------------------------------------

table(Res2019$X4)

When <- Res2019 %>%
  group_by(X4) %>%
  summarise(n = n()) %>%
  mutate(perc = n/sum(n)) %>%
  arrange(desc(perc)) %>%
  mutate(custom_cumsum = 0)

# customising the cumulative sum for ggplot
When[1,4] <- When[1,3]
When[2,4] <- sum(When[1,4], When[2,3])
When[3,4] <- sum(When[2,4], When[3,3])
When[6,4] <- sum(When[3,4], When[6,3])
When[7,4] <- sum(When[6,4], When[7,3])
When[4,4] <- When[4,3]
When[5,4] <- When[5,3]


# Capital Growth and Rental Yield -----------------------------------------

table(Res2019$X11)





