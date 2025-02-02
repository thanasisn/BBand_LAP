

####_ Notes _####

##
## Try to define obstacles in the view of the instrument to remove while filtering
##

source("~/CODE/FUNCTIONS/R/data.R")


TRACKER_STP <- 0.6 * 12 / 70



## get data to analyze obstacles

data <- DATA[ !is.na(QCF_BTH_06_1) | !is.na(QCF_BTH_06_2) ]
data <- DATA[ !is.na(QCF_BTH_06_2)  ]


plot(data$Azimuth, data$Elevat, main = paste(unique(year(data$Date)), collapse = " "),
     xlim = c(85,130), ylim = c(6,14), cex = 0.1)
plot(data$Azimuth, data$Elevat, main = paste(unique(year(data$Date)), collapse = " "),
     xlim = c( 200, 270), cex = 0.1)

plot(data$Azimuth, data$Elevat, main = "Rayleigh diffuse limit (18)")



#### Detect βιολογικό ##########################################################

## use these hard limit to constrain any results
bBox = c( X = c(89,127),
          Y = c( 5, 13) )

## subset bio data
biol_ori <- data[Azimuth > bBox[1] &
                 Azimuth < bBox[2] &
                 Elevat  > bBox[3] &
                 Elevat  < bBox[4]  ]

biol <- as.data.frame(biol_ori[, Azimuth, Elevat])
## expand outer limits with some tracker steps
biol <- data.frame(
  rbind(
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat + TRACKER_STP     ),
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat - TRACKER_STP     ),
    cbind(Azimuth = biol$Azimuth + TRACKER_STP     , Elevat = biol$Elevat                   ),
    cbind(Azimuth = biol$Azimuth - TRACKER_STP     , Elevat = biol$Elevat                   ),
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat + TRACKER_STP     ),
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat - TRACKER_STP     ),
    cbind(Azimuth = biol$Azimuth + TRACKER_STP * 2 , Elevat = biol$Elevat                   ),
    cbind(Azimuth = biol$Azimuth - TRACKER_STP * 2 , Elevat = biol$Elevat                   ),
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat + TRACKER_STP * 2 ),
    cbind(Azimuth = biol$Azimuth                   , Elevat = biol$Elevat - TRACKER_STP * 2 )
  ))
biol <- data.table(biol)


## get azimuth limits
BIO_AZ_lim <- c( min(biol$Azimuth,na.rm = T),
                 max(biol$Azimuth,na.rm = T))



plot(biol_ori$Azimuth, biol_ori$Elevat,
     xlim  = bBox[1:2], ylim = bBox[3:4], pch = 19, cex = .5, main = "Biology" )
abline(v = BIO_AZ_lim[1])
abline(v = BIO_AZ_lim[2])

plot(biol_ori$Azimuth, biol_ori$Elevat,
     xlim  = c(89,95), ylim = bBox[3:4], pch = 19, cex = .5, main = "Biology left" )
abline(v = BIO_AZ_lim[1])

plot(biol_ori$Azimuth, biol_ori$Elevat,
     xlim  = c(125,127), ylim = bBox[3:4], pch = 19, cex = .5, main = "Biology right" )
abline(v = BIO_AZ_lim[2])



## consider this as a magic number
## round azimuth to create bins of elevation angles to study
ast = .7

LIM_azel <- biol[ , .(UpElev = max(Elevat,na.rm = T)), by = ( Azimuth = round_by_num(Azimuth, ast)) ]
setorder(LIM_azel, Azimuth)

## actual data
plot(biol_ori$Azimuth, biol_ori$Elevat,
     xlim  = bBox[1:2], ylim = bBox[3:4], pch = 19, cex = .5, col = "blue" )

## upper bin limits after jittering
segments( x0 = LIM_azel$Azimuth,
          y0 = LIM_azel$UpElev,
          x1 = LIM_azel$Azimuth + ast,
          y1 = LIM_azel$UpElev, col = "red")

dlim <- data.frame(Azimuth = c(LIM_azel$Azimuth, LIM_azel$Azimuth + ast),
                   UpElev = c(LIM_azel$UpElev,  LIM_azel$UpElev ))


## this is almost good as a filter
BIO <- approxfun( x = dlim$Azimuth,
                  y = dlim$UpElev,
                  method = "constant")
# BIO <- approxfun( x = dlim$Azimuth,
#                   y = dlim$UpElev)

## test data for the fitted line
az <- seq( bBox[1], bBox[2], 0.05 )
el <- BIO(az)
lines(az,el)


## test data for plotting
dt <- data.frame(
  xd = seq( bBox[1], bBox[2], 0.05 ),
  yd = 8
)
dt$yd <- runif(length(dt$yd), 8,12 )


biolog_build <- function( Data_x, Data_y) {
  stopifnot(length(Data_x) == length(Data_y))
  dt <- data.frame( xd = Data_x, yd = Data_y )

  ## load data in the function
  x0 <- LIM_azel$Azimuth
  y0 <- LIM_azel$UpElev
  x1 <- LIM_azel$Azimuth + ast
  y1 <- LIM_azel$UpElev

  data <- data.frame()
  for ( i in 1:nrow(dt) ) {
    ## a data point to characterize
    dp <- dt[i,]

    ## check if we have explicit limit
    ind <- which( dp$xd >= x0 & dp$xd <= x1 )

    ## have to resolve cases when no limit
    if ( length(ind) == 0 ) {

      ## check if there are any limits around the poind
      if ( any( dp$xd <= x1 ) & any( dp$xd >= x0 ) ) {

        ## interpolate between limits
        from  <- tail( which( x1 - dp$xd < 0 ), 1 )
        until <- which( x0 - dp$xd > 0 )[1]

        y_lim <- approx( x = c(x1[from], x0[until]),
                         y = c(y0[from], y0[until]),
                         xout = dp$xd )$y

        ## apply interpolated limits
        if ( dp$yd > y_lim) {
          # points(dp, pch=19, col="magenta", cex =.5 )
          data <- rbind(data, cbind(dp, type = "normal"))
          next()
        } else {
          # points(dp, pch=19, col="cyan", cex =.5 )
          data <- rbind(data, cbind(dp, type = "bellow"))
          next()
        }
      ## no limit applied
      } else {
        # points(dp, pch=19, col="blue", cex =.5 )
        data <- rbind(data, cbind(dp, type = "normal"))
        next()
      }
    }

    ## cases with existing limit
    if ( length(ind) > 0 ) {
      stopifnot( y0[ind] == y1[ind])

      ## apply infered limits
      if (dp$yd <= y0[ind]) {
        # points(dp, pch=19, col="yellow", cex =.5 )
        data <- rbind(data, cbind(dp, type = "bellow"))
        next()
      } else {
        # points(dp, pch=19, col="green", cex =.5 )
        data <- rbind(data, cbind(dp, type = "normal"))
        next()
      }
    }
  }
  ## all input must be characterized
  stopifnot(length(data[,1]) == length(dt[,1]))
  ## results
  return(data)
}
## test function
filt_data <- biolog_build( dt$xd, dt$yd)

points(filt_data$xd, filt_data$yd, col= as.factor(filt_data$type), pch = 19, cex = .5)
title(main = "Biology building masking test")


#### Another more mathematical approach that may work ####

# library(alphahull)

# dd <- copy(LIM_azel)
# dd[ , UpElev := 5 ]
# LIM_azel <- rbind( LIM_azel, dd )

# plot(ahull(LIM_azel$Azimuth, LIM_azel$UpElev, alpha = 3))
# points(biol$Azimuth, biol$Elevat, pch = 19, cex = .5, col = "red" )

# dd <- copy(biol)
# dd[ , Elevat := 5 ]
# biol <- rbind( biol, dd )

# ss <- ahull( biol$Azimuth, biol$Elevat + TRACKER_STP , alpha = 2.5 )
# plot(ss, pch = 19, cex = .5)
# points( biol$Azimuth, biol$Elevat, pch = 19, cex = .5, col = "red" )

# tt <- ashape(biol$Azimuth, biol$Elevat , alpha = 2)
# plot(tt, pch = 19, cex = .5)
# points( LIM_azel$Azimuth, LIM_azel$UpElev, pch = 19, cex = .5, col = "red" )



####  Detect pole  #############################################################

## use these hard limit to constrain any results
pBox = c( X = c(217,280),
          Y = c( 1, 20) )

## get flagged data
pole_ori <- data[ Azimuth >= pBox[1] &
                    Azimuth <= pBox[2] &
                    Elevat  >= pBox[3] &
                    Elevat  <= pBox[4]  ]

pdates <- unique(pole_ori[, as.Date(Date)])


## get all effected data

## get data to analyze obstacles
data <- data.table()
for (afile in filelist) {
  data <- data.table(readRDS(afile))

  ## select dates
  # data <- data[ as.Date(Date) %in% pdates ]
  ## selct bounds
  data <- data[ Azimuth >= 270 &
                Azimuth <= 290  ]

  # data <- data[ month(Date) %in% c(8,9)]

  data <- rbind(data, data)
}

plot(pole_ori$Azimuth, pole_ori$Elevat, pch = 19, cex = .8,
     col = clr_smth(as.numeric(pole_ori$Date))$cols, main = "WHY ?"  )

plot(pole_ori$Azimuth, pole_ori$Elevat,
     xlim  = c(220,240), pch = 19, cex = .8,
     col = clr_smth(as.numeric(pole_ori$Date))$cols, main = "WHY ?")


hist( pole_ori[ , wattDIR ] )

plot( pole_ori[ Azimuth > 200 & wattDIR_tmp_cr < 400, Azimuth ],
      pole_ori[ Azimuth > 200 & wattDIR_tmp_cr < 400, Elevat ]  )

for (aday in unique(as.Date(data$Date))) {
  data <- data[ as.Date(Date) == aday ]

  if (all(is.na(data$wattDIR))) { next() }
  # plot(data$Azimuth, data$wattDIR, "l")
  plot(data$Azimuth, data$wattDIR, "l", main = paste(aday) )
  points(pole_ori$Azimuth, pole_ori$wattDIR, col = "green")

}


ylim = range(data$wattHOR, na.rm = T)
xlim = range(data$Azimuth, na.rm = T)
plot(1,1, xlim = xlim, ylim = ylim)
for (aday in unique(as.Date(data$Date))) {
  data <- data[ as.Date(Date) == aday ]

  # plot(data$Azimuth, data$wattDIR, "l")
  # plot(data$Date, data$wattDIR, "l", main = paste(aday) )
  # points(pole_ori$Date, pole_ori$wattDIR, col = "green")
  lines(data$Azimuth, data$wattHOR)

}


## Test proposed search space for signal drop
## prominent months 4 5  7 8
Pole_az_lim <- c( 271, 285 )

for (m in 1:12) {
  temp <- data[ month(Date) == m ]
  cat(paste(m, nrow(temp)), sep = "\n")
  if ( nrow(temp) < 10 ) {next()}

  ylim = range(temp$wattDIR, na.rm = T)
  xlim = range(temp$Azimuth, na.rm = T)
  plot(1,1, xlim = xlim, ylim = ylim, main = m)
  for (aday in unique(as.Date(temp$Date))) {
    data <- temp[ as.Date(Date) == aday ]

    # plot(data$Azimuth, data$wattDIR, "l")
    # plot(data$Date, data$wattDIR, "l", main = paste(aday) )
    # points(pole_ori$Date, pole_ori$wattDIR, col = "green")
    lines(data$Azimuth, data$wattDIR)

  }
  abline(v = Pole_az_lim[1], col = "red" )
  abline(v = Pole_az_lim[2], col = "red" )
}


####  Store the limits for usage in QCRad  #####################################
# save( biolog_build, LIM_azel, bBox, ast, Pole_az_lim,
#       file = "~/Aerosols/source_R/Obstacles.Rda" )


