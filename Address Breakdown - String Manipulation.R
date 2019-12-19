# this script will breakdown buyer addresses of inner brisbane units over the past 10 years to September 2018 using the Rebus and StringR packages
# This is not completely reproducible

# Set Up ------------------------------------------------------------------


# Load Data
buyers <- read.csv("R:/Projects/Market Analysis and Database Records/Sales Data/Sales Data CSV Files/Geocoded Unit Buyer Address Inner Ring 10 Years to September 2018.csv",
                            header = TRUE, stringsAsFactors = FALSE, sep = ",")

# Load libraries
library(dplyr)
library(tidyr)
library(rebus)
library(stringr)
library(stringi)
library(ggplot2)

# Inspect Data
glimpse(buyers)
head(buyers)
tail(buyers)


# Match and Extract country ---------------------------------------------

# pattern
country <- optional(SPACE) %R% one_or_more(WRD) %R% END


# check match
str_view(buyers$geoAddress, country, match = TRUE)

# replace numbers at the end with spaces

# end number pattern
numbers <- SPACE %R% one_or_more(DGT) %R% END

# check match
str_view(buyers$geoAddress, numbers, match = TRUE)

# duplicate geoaddresses
buyers$geoAddress2 <- buyers$geoAddress

# replace end numbers with spaces
buyers$geoAddress2 <- str_replace(buyers$geoAddress2, numbers, " ")
comma <- "," %R% optional(one_or_more(SPACE)) %R% END #left comma
buyers$geoAddress2 <- str_replace(buyers$geoAddress2, comma, " ")
buyers$geoAddress2 <- str_trim(buyers$geoAddress2, side = "right")

# extract countries
country_rows <- str_extract(buyers$geoAddress2, country)

# check for NA results
which(is.na(country_rows))

# Check for NA Result
sum(is.na(country_rows))

# find NA
na_results <- which(is.na(country_rows))
buyers$geoAddress2[na_results] # Entry is an NA Address

# check countrys in tabular format
length(country_rows)

# replace country names to one uniform value
country_rows <- str_replace(country_rows, or("AUS", "australia", "AUSTRALIA", "4680", "QLD"), "Australia") # Australia
country_rows <- str_replace(country_rows, "AustraliaTRALIA", "Australia") # Australia
country_rows <- str_replace(country_rows, "africa", "South Africa") # South Africa
country_rows <- str_replace(country_rows, "arabia", "Saudi Arabia") # Saudi Arabia
country_rows <- str_replace(country_rows, "caledonia", "New Caledonia") # New Caledonia
country_rows <- str_replace(country_rows, or("china", "CHINA", "2", "4", "7", "shi", "fengtai", "guomao", "middle", "qiao", "qingdao", "qu", "rd", "tianlin"), "China") # China
country_rows <- str_replace(country_rows, "emirates", "United Arab Emirates") # UAB
country_rows <- str_replace(country_rows, or("guinea", "PNG"), "Papua New Guinea") # PNG
country_rows <- str_replace(country_rows, or("IDNonesia", "indonesia"), "Indonesia") # Indonesia
country_rows <- str_replace(country_rows, or("italy", "ITALY"), "Italy") # Italy
country_rows <- str_replace(country_rows, or("kingdom", "uk"), "United Kingdom") # United Kingdom
country_rows <- str_replace(country_rows, or("Kong", "kong", "aberdeen", "hom", "Hong Hong Kong", "Hong Hong Kong", "wan"), "Hong Kong") # Hong Kong
country_rows <- str_replace(country_rows, or("KOREA", "korea"), "Korea") # Korea
country_rows <- str_replace(country_rows, or("macau", "MACAU"), "Macau") # Macau
country_rows <- str_replace(country_rows, or("MALAYSIA", "malaysia"), "Malaysia") # Malaysia
country_rows <- str_replace(country_rows, or("singapore", "SINGAPORE"), "Singapore") # Singapore
country_rows <- str_replace(country_rows, or("states", "usa"), "United States") # United States
country_rows <- str_replace(country_rows, or("thailand", "THAILAND"), "Thailand") # Thailand
country_rows <- str_replace(country_rows, "zealand", "New Zealand") # New Zealand
country_rows <- str_replace(country_rows, "zealand", "New Zealand") # New Zealand
country_rows <- str_replace(country_rows, "usvi", "Virgin Islands") # Virgin Islands
country_rows <- str_replace(country_rows, "islands", "Solomon Islands") # Solomon Islands
country_rows <- str_replace(country_rows, or("road", "taiwan"), "Taiwan") # Taiwan
country_rows <- str_replace(country_rows, "joChinaan", "China") # China
country_rows <- str_replace(country_rows, "United Kingdomraine", "United Kingdom") # UK
country_rows <- str_replace(country_rows, "0032", "Japan") # Japan

buyers <- buyers %>%
  select(1:4) %>%
  mutate(Country = as.factor(country_rows))

# check levels for anomalies for further fixing
levels(buyers$Country)

# Left Trim
buyers$Country <- str_trim(buyers$Country, side = "right")

# make changes
buyers$Country <- str_replace(buyers$Country, "003China", "China")
buyers$Country <- str_replace(buyers$Country, or(" abeChinaeen", " botsHong Konga"), "Hong Kong")
buyers$Country <- str_replace(buyers$Country, " taiHong Kong", "Taiwan")
buyers$Country <- str_replace(buyers$Country, " China", "China")

str_which(buyers$Country, "botsHong Konga")
buyers$Country[c(32663, 32672, 32694, 32695)]
buyers$Country[32676] <- "Australia"
buyers$Country[32676]
buyers$Country[4] <- "Japan"

countries <- buyers$Country %>%
  str_trim(side = "left")

table(countries)

# Convert to proper text format
buyers <- buyers %>%
  mutate(Country = stri_trans_totitle(buyers$Country))

# create summary for ordering in graph

buyers_country_summary <- buyers %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))
  
# complete cases only
buyers_country_summary_complete <- buyers_country_summary[complete.cases(buyers_country_summary),]


# Plot in Barplot
ggplot(buyers_country_summary_complete, aes(reorder(Country, Total), Total)) +
  geom_bar(stat = "identity") +
  coord_flip()


write.csv(buyers, "Unit buyer Locations.csv")

getwd()

# Extract State -----------------------------------------------------------

# load new file 
unit_buyer_addresses <- read.csv("Unit buyer Locations.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, colClasses=c("NULL",NA,NA,NA, NA, NA))

# get vector of rows where country is Australia
australia_rows <- str_which(unit_buyer_addresses$Country, "Australia")

states <- or("QLD", "qld", "queensland", "NSW", "nsw", "new south wales", "VIC", "vic", "victoria", "ACT", "act", "TAS", "tas", "SA", "sa", "south australia", "NT", "nt", "WA", "wa", "norfolk island")

# Extract states
states_extract <- str_extract(unit_buyer_addresses$geoAddress[australia_rows], or(START,SPACE) %R% states %R% or(SPACE, ","))

#count Na's
sum(is.na(states_extract))
 
# detect na placing
state_na <- which(is.na(states_extract))

# row in data frame that corresponds to the na
states_na2 <- australia_rows[state_na]

#show the rows that returned an na
unit_buyer_addresses$geoAddress[states_na2]

# improve the states vector

# show tabular form to see any anomalies
table(states_extract)

# create new state column
unit_buyer_addresses$states <- NA

# fill the state column with state variables where relevant
unit_buyer_addresses$states[australia_rows] <- states_extract

unit_buyer_addresses$states <- str_trim(unit_buyer_addresses$states, side = "both")


unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("QLD", "qld", "queensland"), "QLD")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("NSW", "nsw", "new south wales", "norfolk island"), "NSW")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("VIC", "vic", "victoria"), "VIC") 
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("ACT", "act"), "ACT")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("TAS", "tas"), "TAS")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("SA", "sa", "south australia"), "SA")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("NT", "nt", "NT,"), "NT")
unit_buyer_addresses$states <- str_replace(unit_buyer_addresses$states, or("WA", "wa"), "WA")


table(unit_buyer_addresses$states)

unit_buyer_addresses$states[unit_buyer_addresses$states == "WA,"] <- "WA"
unit_buyer_addresses$states[unit_buyer_addresses$states == "VICtoria"] <- "VIC"
unit_buyer_addresses$states[unit_buyer_addresses$states == "VIC,"] <- "VIC"
unit_buyer_addresses$states[unit_buyer_addresses$states == "SA,"] <- "SA"
unit_buyer_addresses$states[unit_buyer_addresses$states == "QLD,"] <- "QLD"
unit_buyer_addresses$states[unit_buyer_addresses$states == "NT,"] <- "NT"
unit_buyer_addresses$states[unit_buyer_addresses$states == "NSW,"] <- "NSW"
unit_buyer_addresses$states[unit_buyer_addresses$states == "ACT,"] <- "ACT"
unit_buyer_addresses$states[is.na(unit_buyer_addresses$states)] <- "OverSeas"

# plot graph

ggplot(unit_buyer_addresses, aes(states)) +
  geom_bar(stat = "count")

# write the file 
write.csv(unit_buyer_addresses, "unit_buyer_address_c_s.csv")


# Extract Postcode --------------------------------------------------------

# load data
unit_buyer_addresses2 <- read.csv("unit_buyer_address_c_s.csv")

# postcode rebus
postcode <- SPACE %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(or(END, ","))

# check the code
str_view(unit_buyer_addresses$geoAddress[australia_rows], postcode, match = TRUE)

# Extract Postcode
postcode_extraction <- str_extract(unit_buyer_addresses$Unit_Buyer_Address[australia_rows], postcode)

#check for nas
sum(is.na(postcode_extraction))

# detect NA placing
nas <- which(is.na(postcode_extraction))

# match original place in full list
nas2 <- australia_rows[nas]

# view na returns
unit_buyer_addresses2$Unit_Buyer_Address[nas2] # no postode in these results in address

# check original buyer address
unit_buyer_addresses2$Unit_Buyer_Address[nas2]

# create postcode column

unit_buyer_addresses2$postcode <- NA
unit_buyer_addresses2$postcode[australia_rows] <- postcode_extraction

# clean commas

unit_buyer_addresses2$postcode <- str_replace(unit_buyer_addresses2$postcode, ",", "")
unit_buyer_addresses2$postcode <- str_trim(unit_buyer_addresses2$postcode, side = "both")

write.csv(unit_buyer_addresses2, "Geocoded_Inner_City_Unit_Buyer_Addresses.csv")

# Plots -------------------------------------------------------------------

# Country Plot
buyers_country_summary <- unit_buyer_addresses2 %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

buyers_country_summary_complete <- buyers_country_summary[complete.cases(buyers_country_summary),]

ggplot(buyers_country_summary_complete, aes(reorder(Country, Total), Total)) +
  geom_bar(stat = "identity") +
  coord_flip()


# State Plot
ggplot(unit_buyer_addresses2, aes(states)) +
  geom_bar(stat = "count") 


# Filter only Queensland
QLD_only <- filter(unit_buyer_addresses2, states == "QLD")

postcode_summary <- QLD_only %>%
  group_by(postcode) %>%
  summarise(Total = n())

# Postcode Plot QLD
ggplot(postcode_summary, aes(reorder(postcode, Total), Total)) +
  geom_bar(stat = "identity") +
  coord_flip()


