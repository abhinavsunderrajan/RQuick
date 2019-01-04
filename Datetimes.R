#working with dates and times

#Times are represented by the POSIXct or the POSIXlt class.

#POSIXct is just a very large integer under the hood.It use a useful class when you want to store times in something like a data frame.
#POSIXlt is a list underneath and it stores a bunch of other useful information like the day of the week, day of the year, month, day of the month. This is useful when you need that kind of information.

x<-Sys.time()
x
x<-as.POSIXlt(x)
names(unclass(x))
ls=unclass(x)

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
y<-as.POSIXlt(x)
unclass(y[1])

x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 01:00:00", tz = "GMT")
y-x