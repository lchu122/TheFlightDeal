# steps taken
## indicates not necessary for future scraped data

# cleaning
## used or.fix for error in first scraping not catching certain values
# remove leading / and commas
# add missing years
# swap back dates to day month year to coincide better with early, mid,late etc.


# parsing
# take dates and make consistent whether it's early, mid, late, month of

# calculate date range by assigning values to early, mid, late, month of and 
# compare to see if a date is within a range

# remove ones with NA. weird scrape error
flightdeal.data <- na.omit(flightdeal.data)


library(dplyr)
flightdeal.data$TravelDates <- lapply(flightdeal.data$TravelDates, dateParser)
testasd[795:806]
flightdeal.data$TravelDates[492]
# both work
trueones <- which(lapply(flightdeal.data$TravelDates, isDateIn, date.entry = 'late January 2017' ) == TRUE)
flightdeal.data[trueones, '']
filter(flightdeal.data, sapply(TravelDates, isDateIn, date.entry = 'early January 2017' ))$Airline
?filter
