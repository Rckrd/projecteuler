
#problem 19
## Not run: 
(today <- Sys.Date())
format(today, "%d %b %Y")  # with month as a word
(tenweeks <- seq(today, length.out=10, by="1 week")) # next ten weeks
weekdays(today)
months(tenweeks)
as.Date(.leap.seconds)

dates <- seq(as.Date("1901-01-01"), as.Date("2000-12-31"), by ="1 day")

sdates <- dates

sdates <- sdates[weekdays(sdates)=="söndag"]

sdates <- as.character(sdates)
length(sdates[substr(sdates,nchar(sdates)-1,nchar(sdates)) == "01"])
