x <- "Jan-01"
y <- as.Date(paste("01-", x, sep = ""), format = "%d-%b-%y")
class(y)

class(x)
as.character.Date(x)
class(x)


# If you want a POSIXct class, consider using lubridate:
  
  library(lubridate)
t <- "Jan-01"

dmy(paste("01-", x , sep =""))

# If you want to just rely on base:
  

z <- as.POSIXct(paste("01-", x, sep = ""), format = "%d-%b-%y")
class(z)

c <- 18-11-2001
c <- as.Date(paste(c, sep = ""), format = "%d-%b-%y")
class(c)
