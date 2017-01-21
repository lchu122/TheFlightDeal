# left to do

#  scrape airports

# apply and clean airports and get pathway of longitudes and latitudes
# calculate cents per mile




# get pathway 

# create leaflet
# plot points
# filter by airline, hold x fixed
# filter by locaiton, hold y fixed
# filter by time, hold j fixed



# calculate lowest?


# color markers ? maybe. png file. # johnassta'ss file? green yellow radii?



flightdeal.data <- read.csv('FlightDealCleaned.txt', sep = '|', header = FALSE,
                            col.names = c('PostingDate','Title','Airline','Price','isBothWays','TravelDates','Routing','Miles','AdditionalInfo'), 
                            stringsAsFactors = FALSE)



# Routing -----------------------------------------------------------------


library(ggmap)
library(tidyr)
flightdeal.data <- separate_rows(flightdeal.data, Routing, sep = "/")

# note we manually removed abbr for Canada Provinces
# trivial to do manually

# read in airport codes as list
# no idea why it doesnt capture all the slashes. hidden text?
airport.codes.raw <- scan('AirportCodes.txt', what= 'character', sep ='|')

# remember split uses regular expressions 
split.codes <- unlist(lapply(airport.codes.raw, strsplit, split = '|', fixed = TRUE))

# inconsistency was found in that we lost a city when we tried to split by parantheses below
# find the culprit
# another typo in seemingly innocuous data set...
# added this manually.
which(!grepl("\\(\\w\\w\\w\\)", split.codes))

# more inconsistency. this time it's a missing space before parentheses...
for (i in which(grepl("\\w\\(", split.codes))) {
  split.codes[i] <- sub('(', ' (', split.codes[i], fixed = TRUE)
}
which(grepl("\\w\\(", split.codes))

# checks out. 3623 airorts from python
length(split.codes) 

# this is written incorrectly. doesn't actual check double parentheses.
# manually found which ones were the culprits.
# (Keeling) and (FYROM)
# which(length(gregexpr(" (?=\\()", split.codes, perl = TRUE)[[1]]) == 2)

split.codes2 <- unlist(lapply(split.codes, strsplit, split = " (?=\\()", perl = TRUE))
?gsub
# keep validating proper split
length(split.codes2) == length(split.codes) * 2

airport.codes <- data.frame('Location' = split.codes2[seq(1,length(split.codes2), 2)],
                            'Code' = split.codes2[seq(2,length(split.codes2), 2)])

airport.codes$Code <- sapply(airport.codes$Code, sub, pattern = '(', replacement ='',
                             fixed = TRUE)
airport.codes$Code <- sapply(airport.codes$Code, sub, pattern = ')', replacement ='',
                             fixed = TRUE)

airport.codes$Location <- as.character(airport.codes$Location)
airport.codes$Code <- as.character((airport.codes$Code))
# geocode takes the format cleanly!

# TravelDates -------------------------------------------------------------



# missed certain values, use this. No need in future scraping


re1 <-"(?<=or )((early |mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \\d+(th|st|nd|rd))?(, \\d{4})?)"
re2 <- "(?<=or )((early |mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \\d+(th|st|nd|rd))?(, \\d{4})?)(?= -)"

# some tests
# note for some reason negative lookahead assertion doesn't work. Works in Python
test1 <- 'Valid for travel in early December or January, 2017 - March, 2017.'
test2 <- 'Valid for travel in early December or January, 2017.'
grep(re1, test1, perl = TRUE)
grep(re1, test2, perl = TRUE)

or.fix <- function(or.man, date) {
  if(grepl(re1, or.man, perl = TRUE) && !grepl(re2, or.man, perl = TRUE)){
    
    or.date.raw <- regmatches(or.man, regexpr(re1, or.man, perl=TRUE))
    
    or.date <- paste(or.date.raw, sep='/', collapse ='')
    print(or.date)
    return(paste(date, or.date, sep="/"))
    
    
  }
  
}

date.strings <- flightdeal.data$AdditionalInfo
date.strings.fixed.or <- mapply(or.fix, date.strings, flightdeal.data$TravelDates)

# check results
unlisted.dates <- unlist(date.strings.fixed.or)
unlisted.dates



# we could also use by matching names of our unlisted.dates to get index, but that would require applying
# through data frame again
unlisted.dates.idx <- which(unlist(lapply(date.strings.fixed.or, is.null)) == FALSE)
unlisted.dates.idx

# tired of mapplys
for( i in 1:length(unlisted.dates.idx)) {
  index = unlisted.dates.idx[i]
  flightdeal.data$TravelDates[index] <- unlisted.dates[i]
}
flightdeal.data$TravelDates[1]


# end fix

# remove leading /
flightdeal.data$TravelDates <- sapply(flightdeal.data$TravelDates, function(x){sub('^/', '', x)})

# remove commas
flightdeal.data$TravelDates <- sapply(flightdeal.data$TravelDates, function(x) { gsub(',', '', x)})





# need to decide whether to split into multiple columns for each separate travel date or
# somehow capture all of them in one.
# hopefully I can make it into a list and access it as a list



# complicated function to add missing years
# currently works on a string that is not split
# can be made easier if we later choose to split dates into multiple
addYears <- function(travel.dates, posting.date){
  years.added = travel.dates
  # May - June 2016, year should precede dashes
  years.idx <- gregexpr('\\d{4} - ', travel.dates)[[1]]
  # May - June/July - August 2016, year should precede slashes
  years.idx2 <- gregexpr('\\d{4}/', travel.dates)[[1]]
  # year should precede end of string
  years.idx3 <- gregexpr('\\d{4}$', travel.dates)[[1]]
  
  years.idx.all <- c(years.idx , years.idx2, years.idx3)
  
  
  possible.years.idx <- gregexpr(' - ', travel.dates)[[1]]
  possible.years.idx2 <- gregexpr('/', travel.dates)[[1]]
  
  # because not every date has mutiples indicated by /, and dashes -
  if(possible.years.idx2[1] == -1){
    possible.years.idx2 = integer()
  }
  
  if(possible.years.idx[1] == -1){
    possible.years.idx = integer()
  }
  
  # plus one because when we shift later we need to move 1 less
  possible.years.idx3 <- nchar(travel.dates) + 1
  possible.years.idx.all <- sort(c(possible.years.idx, possible.years.idx2, possible.years.idx3))
  
  
  years.idx.shift <- possible.years.idx.all - 4
  missing.years <- years.idx.shift %in% years.idx.all
  k <- which(missing.years == FALSE)
  
  
  # bad practice to update in loop. it's accounted for
  # how to add them all at once instead? easy in python...
  j <- 0
  # posting.date is formatted "DD Mon YYYY"
  year <- substr(posting.date, 7, nchar(posting.date))
  
  # k == integer(0) if all years are already present
  if(length(k) > 0){
    for(i in k){
      
      # adding the ' year' which is at index 7 to end of string
      years.added <- paste0(substr(years.added,1, possible.years.idx.all[i] - 1 + j),
                            year,
                            substr(years.added, possible.years.idx.all[i] + j, nchar(years.added)))
      # catch the shift due to each add
      j <- j + 5
      
      
    }
  }
  return(years.added)
}  
test.dates <- 'March 20th/early December - late January 2017/December/early March - late June'
addYears(test.dates, '11 Nov 2016')

# syntax starts with function for mapply
flightdeal.data$TravelDates <- mapply(addYears, flightdeal.data$TravelDates, flightdeal.data$PostingDate)


# swap back dates to day month year
# takes in a date.string that has arleady been split by spaces
# returns a split string still
monDaySwap <- function(date.string.split) {
  split.string = date.string.split
  
  if(grepl('\\b\\d{1,2}\\b', split.string[2])) {
    temp = split.string[2]
    split.string[2] = split.string[1]
    split.string[1] = temp
  } else if(grepl('\\b\\d{1,2}(st|nd|rd|th)', split.string[2])) {
    temp = split.string[2]
    split.string[2] = split.string[1]
    # drop the suffix on the date
    split.string[1] = regmatches(temp, regexec('\\d{1,2}', temp))[[1]]
  }
  
  # return(paste(split.string, collapse =' '))
  return(split.string)
  
}

test.dates.split <- c("Jan", "10", "2017")
test.dates.split2 <- c("Jan", "10th", "2018")
monDaySwap(test.dates.split)
monDaySwap(test.dates.split2)


# returning it in decimal places so we can use "all" filter wisely
month2Num <- function(month.string) {
  
  if(nchar(month.string) == 3){
    month.num <- which(month.abb == month.string)*.01
  }
  else {
    month.num <- which(month.name == month.string)*.01
    
  }
  
  return(month.num)
}


# turn dates into consistent format we can use for comparison
# some dates have days, others just indicate early/mid/late month etc.
# others simply have month
dateParser <- function(date.string){
  # complicated, deal via cases.
  
  # split into the multiple ranges, make into list again later
  dates.raw <- strsplit(date.string, '/')[[1]]
  
  # replace early mid and late with arbitrary dates
  # dates.raw <- gsub('early', '1', dates.raw)
  # dates.raw <- gsub('mid', '11', dates.raw)
  # dates.raw <- gsub('late', '21', dates.raw)
  
  # keep it at early, mid, late, or all
  # convert days into a range
  # 1-1 is early, 11-20 is mid, 21-30 is late. No date is assumed to be the whole month.
  # would have to set days as 1 to eom if there's no date.
  
  # split the dash to check the above
  dates.raw.split <- strsplit(dates.raw, ' - ')
  # note we may have a list of multiple elements now
  
  # get the length of each. this keeps track of which ones were split by dash or not
  length.splits <- sapply(dates.raw.split, length)

  # check each individual string. 1st element is either early, mid, late, or a day, or a month
  dates.raw.split2 <- lapply(dates.raw.split, strsplit, split = ' ')
  
  # unlist once for easier access
  dates.raw.split3 <- unlist(dates.raw.split2, recursive = FALSE)


  # initialize vector for recombining
  dates.recombined <- character(length(dates.raw.split3))
  
  # ensure dd mm yyyy format
  # remember to use lapply here, because we are doing it on a list
  dates.raw.split4 <- lapply(dates.raw.split3, monDaySwap)

  
  # by now every element has at least a month and a year.
  # assume any one with only 2 elements has no day. Set to "the month of"
  has.no.daterange <- which(sapply(dates.raw.split4,length) == 2)

  
  
  for( i in has.no.daterange){
    dates.recombined[i] <- paste("month of", paste(dates.raw.split4[[i]], collapse = ' '), collapse = '') 
  }
  

  # we couldn't just take the complement because some already have early, mid, late
  has.day <- grep('\\b\\d{1,2}\\b', dates.raw.split4)
  

  # day by now should be in 1st position.
  for( i in has.day){
      if(as.numeric(dates.raw.split4[[i]][1]) < 11 ){
        dates.raw.split4[[i]][1] = "early"
      } else if(as.numeric(dates.raw.split4[[i]][1]) < 21) {
        dates.raw.split4[[i]][1] = "mid"
      } else if(as.numeric(dates.raw.split4[[i]][1]) >=21){
        dates.raw.split4[[i]][1] = "late"
      }
    dates.recombined[i] <- paste(dates.raw.split4[[i]], collapse = ' ')
    
  } 

  
  # for lack of a better word, with early, mid, late etc.
  has.specifier <- setdiff(seq(1,length(dates.raw.split4)), has.no.daterange)
  has.specifier <- setdiff(has.specifier, has.day)

  
  for(i in has.specifier){
    dates.recombined[i] <- paste(dates.raw.split4[[i]], collapse = ' ')
  }
  
  dates.recombined2 <- character(length(length.splits))

  j = 1
  k = 1
  for( i in length.splits ) {
    
    if( i == 1){
      dates.recombined2[j] <- dates.recombined[k]
      k = k + 1
    }
    if( i == 2){
      dates.recombined2[j] <- paste(dates.recombined[k], dates.recombined[k+1], sep = ' - ')
      k = k + 2
    }
    j <- j + 1
  }
  
  # a vector of dates
  return(dates.recombined2)
  
  
  # for converting to actual dates following the vein of changing early to 1-11, etc.
  # dates.parsed <-  as.Date(sapply(dates.raw, dateFormat), origin ='1970-01-01')
  # return(dates.parsed)  
  
}
test.dates <- '20 December 2017 - 15 January 2018/March 2018/April 10th 2018/Nov 10 2016'
test.dates2 <- "Nov 12 2016 - early December 2016/January 2017 - May 2017"
test.dates3 <- "10 January 2017 - April 2017"
dateParser(test.dates)
dateParser(test.dates2)
dateParser(test.dates3)
x <- dateParser(flightdeal.data$TravelDates)
flightdeal.data$TravelDates[1]
x

# have to generate all the months in between still in the presecense of dash.
# second element is month and get range in between

isDateIn <- function(date.range, date.entry){
  # 2 scenarios
  # single month, meaning start-end of motnh
  # start month - end month
  # note we have year + .01*month + range in .00d 
  
  if(grepl('Not Found', date.range)){
    print('Not Found')
    return(FALSE)
  }
  
  date.range.split <- strsplit(date.range, ' - ')[[1]]
  date.start <- date.range.split[1]
  

  
  date.start.split <- strsplit(date.start, ' ')[[1]]
  # because we have 'month of' which would add an extra index
  num.date.start <- month2Num(date.start.split[length(date.start.split) - 1])
  
  
  if(length(date.range.split) == 2) {
    date.end <- date.range.split[2]
    date.end.split <- strsplit(date.end, ' ')[[1]]
    num.date.end <- month2Num(date.end.split[length(date.end.split) - 1]) +
      switch(date.end.split[1],
             "early" = .002,
             "mid" = .004,
             "late" = .006,
             "month" = .009) +
      as.numeric(date.end.split[length(date.end.split)])
  }
    
  else {
    num.date.end <- num.date.start + .009 + as.numeric(date.start.split[length(date.start.split)])
  }
    
  num.date.start <- num.date.start +
    switch(date.start.split[1],
            "month" = .001,
            "early" = .002,
            "mid" = .004,
            "late" = .006) +
    as.numeric(date.start.split[length(date.start.split)])
    

  # 2nd to last value should be month
  date.entry.split <- strsplit(date.entry, ' ')[[1]]
  month.entry <- date.entry.split[length(date.entry.split) - 1]
  
  num.month.entry <- month2Num(month.entry) +
    switch(date.entry.split[1],
           "early" = .002,
           "mid" = .004,
           "late" = .006,
           "month"= .009)
  
  if(date.entry.split[length(date.entry.split)] == 'allYears'){
      num.date.start <- num.date.start - trunc(num.date.start)
      num.date.end <- num.date.end - trunc(num.date.end)
    
  }
  else{
    num.month.entry <- num.month.entry + as.numeric(date.entry.split[length(date.entry.split)])
  }
  tryCatch({
    if(num.month.entry >= num.date.start && num.month.entry <= num.date.end){
      return(TRUE)
    }
    else {
      return(FALSE)
    }

  },
  error = function(cond) {
    message('this caused an error ', date.range)
    message(num.date.start)
    message(num.date.end)
  },
  finally = {
    return(FALSE)
  }
  )
}

test.dates <- c("mid December 2017 - mid January 2018", "the month of March 2018", "early April 2018", "early Nov 2016")          
sapply(test.dates, isDateIn, date.entry = "early April 2018")



# Shelved -----------------------------------------------------------------



# creates last day of month by adding a month then subtracting one day
eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

# only apply after  / split to capture Nov 10th, etc.. Basically use when as.Date returns NA
# was combined with monDaySwap
monDaySwap2 <- function(date.string) {
  split.string = strsplit(date.string, split = ' ')[[1]]
  
  if(grepl('\\b\\d{1,2}(st|nd|rd|th)', split.string[2])) {
    temp = split.string[2]
    split.string[2] = split.string[1]
    # drop the suffix on the date
    split.string[1] = regmatches(temp, regexec('\\d{1,2}', temp))[[1]]
  }
  
  return(paste(split.string, collapse =' '))
  
}


# convert date string into type Date
dateFormat <- function(date.string){
  formatted.date <- as.Date(date.string, '%d %B %Y')
  
  if(is.na(formatted.date)) {
    formatted.date <- as.Date(monDaySwap2(date.string), '%d %B %Y')
  }
  return(formatted.date)
}


# testing bitwise Xor to swap elements
# af <- c(1,2,3)
# 
# af[1] <- bitwXor(af[1], af[2])
# af[2] <- bitwXor(af[1], af[2])
# af[1] <- bitwXor(af[1], af[2])
# af