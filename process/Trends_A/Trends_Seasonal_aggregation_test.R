

library(data.table)


## Create dummy data  ----------------------------------------------------------
START <- as.POSIXct("1993-11-01", tz = "UTC")
END   <- as.POSIXct("1995-11-01", tz = "UTC")

DT <- data.table(Date = seq(START, END, by = "min"))
DT[, Var := .I ]

DT[, month := month(Date)]
DT[, year  := year(Date)]


DT


dd <- data.table(m = rep(1:12,4))
dd[, y := unlist(lapply(1:4, function(x) rep(x,12)))]



library(zoo)

dd <- transform(dd, season  = as.yearqtr( as.yearmon(paste(y, m, sep = "-")) + 1/12) )



as.yearqtr(as.yearmon(paste(dd$y, dd$m, sep = "-")))
as.yearqtr(as.yearmon(paste(dd$y, dd$m, sep = "-"))) + 1/12




