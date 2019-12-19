# This script analyses the holding times of each existing property in Brisbane and identifies those that are most likely to sell in the near future.

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)


# Get Data ----------------------------------------------------------------


library(RODBC)

# Check database sources available
odbcDataSources()

# create a connection chanell to the database I want
channel <- odbcConnect("Brisbane Property Market")

# see the list of tables in the database
sqlTables(channel)

# load in a table as a datafrom object
sales <- sqlFetch(channel, "All_Sales_Data")

# close the connection
close(channel)


# Get all existing properties and their latest sale date ------------------

glimpse(sales)
dim(sales)

# select and change data types
sales2 <- sales %>%
  select(1:2, 7:11, 14:17, 25) %>%
  mutate(EntryID = as.character(EntryID)) %>%
  mutate(GeocodePropertyAddress = as.character(GeocodePropertyAddress)) %>%
  mutate(Postcode = as.factor(Postcode)) %>%
  mutate(LotNumber = as.character(LotNumber)) %>%
  mutate(BuildingLotNumber = as.character(BuildingLotNumber)) %>%
  mutate(PurchaserName = as.character(PurchaserName)) %>%
  mutate(PurchaserAddress = as.character(PurchaserAddress)) %>%
  mutate(newID = paste(LotNumber, PropertyType)) %>%
  mutate(SaleDate = as.Date(SaleDate)) %>%
  mutate(PropertyType2 = ifelse(PropertyType == "Apartments", "Apartments",
                                ifelse(PropertyType == "House", "House",
                                      ifelse(PropertyType == "Unit", "Unit", "Townhouses"))))
table(sales2$PropertyType2)
glimpse(sales2)

# Get last sale date of all properties

sales_last <- sales2 %>%
  group_by(newID) %>%
  summarise(lastsale = max(SaleDate)) 

glimpse(sales_last)

# match detailes back to this new dataframe based on last sale only

sales_last2 <- sales_last %>%
  left_join(sales2, by = c("newID" = "newID", "lastsale" = "SaleDate"))

glimpse(sales_last2)

# calculate current holding period
todays_date <- ymd(Sys.Date())
sales_last2$CurrentHoldingTime <- todays_date - sales_last2$lastsale

glimpse(sales_last2)

# Exploration of Holding times --------------------------------------------

sales_last2$CurrentHoldingYears <- sales_last2$CurrentHoldingTime/365.25

ggplot(sales_last2, aes(CurrentHoldingYears)) +
  geom_histogram() + theme_classic()

ggplot(sales_last2, aes(PropertyType2, CurrentHoldingYears)) +
  geom_boxplot()

write.csv(sales_last2, "current holding times.csv")
getwd()

# get median prices per 6 months for each suburbs and property type -------

sales_medians <- sales2 %>%
  group_by(Suburb, PropertyType2, SixMonthJuneDec) %>%
  summarise(medprice = median(SalePrice))

write.csv(sales_medians, "median_prices.csv")

# join medians at time of sale
sales_last3 <- sales_last2 %>%
  left_join(sales_medians, by = c("Suburb" = "Suburb", "PropertyType2" = "PropertyType2", "SixMonthJuneDec" = "SixMonthJuneDec"))

glimpse(sales_last3)

# filter and join current medians
current_medians <- filter(sales_medians, SixMonthJuneDec == "2019 June") 
names(current_medians)[4] <- "current_medprice"

sales_last4 <- sales_last3 %>%
  left_join(current_medians, by = c("Suburb" = "Suburb", "PropertyType2" = "PropertyType2")) %>%
  select(-17) %>%
  mutate()

glimpse(sales_last4)
# calculate growth rate
hist(sales_last4$current_medprice)
summary(sales_last4)
sales_last4$CurrentHoldingYears <- sales_last4$CurrentHoldingTime/365.25
sales_last4$yearsint <- as.double(sales_last4$CurrentHoldingYears)

source("C:/Users/Jay Blake/Desktop/CAGR Function.R")

sales_last4$growth <- CAGR(sales_last4$medprice, sales_last4$current_medprice, sales_last4$yearsint)

# calculate estimated equity
sales_last4$currentpriceestimate <- sales_last4$SalePrice * (1+sales_last4$growth)^sales_last4$yearsint
sales_last4$est_equity_growth <- sales_last4$currentpriceestimate - sales_last4$SalePrice

glimpse(sales_last4)

bins <- seq(-500000, 2000000, 100000)

ggplot(sales_last4, aes(est_equity_growth)) + 
     geom_histogram(breaks = bins, col = "black", fill = "skyblue") +
scale_x_continuous(labels = comma) +
  theme_classic()

sales_last5 <- sales_last4 %>%
  filter(est_equity_growth <= 1500000 & est_equity_growth >= -300000)

ggplot(sales_last5[1:10000, ], aes(yearsint, est_equity_growth, alpha = 0.1, col = PropertyType2)) +
  geom_point()+
  scale_colour_manual(values = c("black", "red", "skyblue", "purple"))+
  geom_smooth(se = FALSE) +
  theme_classic()


# write the datefile
 write.csv(sales_last4, "Likelytosell.csv")

            