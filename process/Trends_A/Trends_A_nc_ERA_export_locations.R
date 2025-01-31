#!/usr/bin/env Rscript
#
# Read all nc files from ERA5 and get data for specific locations.
#


closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name = c("nc_ERA_export_locations.R")



library(RNetCDF)
library(data.table)
library(akima)
library(purrr)

source("~/FUNCTIONS/R/data.R")


locations <- data.frame(
    Name  = c("Thessaloniki"),
    LongX = c(22.956055),
    LatiY = c(40.633610)
)



####    ERA5 exporter    #######################################################
nc_folder <- "/home/athan/DATA/Clouds ERA5/"

## for each nc file will interpolate and produce a timeseries of each location

filelist <- list.files(path        = nc_folder,
                       pattern     = "*.nc",
                       full.names  = T,
                       ignore.case = T,
                       recursive   = F
)
filelist <- sort(filelist)


for (afile in filelist) {
    sfile <- sub("nc$", "Rds", afile)
    cat(paste(basename(afile), "\n"))

    ## dont overwrite existing exports
    if (file.exists(sfile)) {
        cat("output exist\n")
        next()
    }

    ## load file and read data
    anc  <- open.nc(afile,write = F)
    data <- read.nc(anc, unpack=TRUE)
    ## store data for saving
    gather          <- list()
    gather$filename <- basename(afile)

    ## variables in file
    variables <- grep("time|longitude|latitude|tp|expver", names(data), ignore.case = T, invert = T,value = T)

    #### get variables info #####
    var_info <- data.table()
    for (avar in variables) {
        var_info <- rbind(var_info,
                          data.table(sort_name = avar,
                                     long_name = att.get.nc(anc,avar,"long_name"),
                                     units     = att.get.nc(anc,avar,"units"))
        )
    }
    gather$var_info <- var_info

    #### get data from nc ####

    ## create proper dates from nc file
    time_desc <- att.get.nc(anc, "time", "units")
    Dates     <- utcal.nc(time_desc, data$time, type = "s")

    ## match indexes with coordinates
    x_inx <- data$longitude
    ## we have to reverse the y axis values and matrix
    y_inx <- rev(data$latitude)

    for (avvv in variables) {
        tempd <- data[[avvv]]

        if (length(dim(tempd)) == 4) {
            tempd <- tempd[,,1,]
            cat("Reduced dims\n")
            warning("Reduced dims\n")
        }

        ## era5 data are points on locations

        x_near_in <- which.min(abs(x_inx - locations$LongX))
        y_near_in <- which.min(abs(y_inx - locations$LatiY))

        cat("Nearest coordinates", x_inx[x_near_in], y_inx[y_near_in])

        near_tcc  <- tempd[x_near_in, y_near_in, ]

        ## Change NA to -9999
        tempd[is.na(tempd)] <- -99999

        ## interpolate temperature data to location
        out <- apply(tempd, MARGIN = c(3), function(z) bilinear(x = x_inx, y = y_inx, flip_matrix_v(z), locations$LongX, locations$LatiY))


        ## get data coordinates
        x_long <- out[[1]]$x
        y_lat  <- out[[1]]$y

        ## prepare data
        outt <- lapply( out , "[[" , "z" )
        outt <- do.call(rbind, outt)
        hist(outt)
        hist(near_tcc)

        outt[outt < -10]         <- NA
        near_tcc[near_tcc < -10] <- NA

        if (any(outt < -1, na.rm = TRUE) | any(near_tcc < -1, na.rm = TRUE)) stop()

        dum1 <- data.frame(outt)
        names(dum1) <- paste0("bilin_", avar)

        dum2 <- data.frame(near_tcc)
        names(dum2) <- paste0("near_", avar)

        temp <- data.frame(Date = as.POSIXct(Dates), dum1, dum2)

        # plot(temp$Date, temp$bilin_tcc)
        # plot(temp$Date, temp$near_tcc)
        # plot(temp$bilin_tcc/temp$near_tcc)

        ## create proper structure
        store <- data.table(x_long = x_long,
                            y_lat  = y_lat,
                            x_long_near = x_inx[x_near_in],
                            y_lat_near = y_inx[y_near_in],
                            name   = locations$Name,
                            temp)
    }
    saveRDS(object   = store,
            file     = sfile,
            compress = "xz")
    close.nc(anc)
}
# image(x_inx, y_inx, flip_matrix_v(data$t2m[,,1]), col = heat.colors(9))
dummy <- gc()




## read data ----------

filelist <- list.files(path        = nc_folder,
                       pattern     = "adaptor.*.Rds",
                       full.names  = T,
                       ignore.case = T,
                       recursive   = F
)
filelist <- sort(filelist)


vars <- c("tcc", "tcwv", "tclw")
## init empty data tables
for (avar in vars) {
    assign(avar, data.table())
}
## fill data tables
gather <- data.table()
for (af in filelist) {
    temp <- readRDS(af)
    for (avar in vars) {
        if (any(grepl(avar, names(temp)))) {
            assign(avar, rbind(get(avar), temp))
        }
    }
}

DATA <- merge(tcc, tclw, all = T)
DATA <- merge(DATA, tcwv)
DATA <- DATA[apply(DATA, MARGIN = 1, function(x) sum(is.na(x))) < 6, ]


hist(DATA$bilin_tcc)
hist(DATA$near_tcc)

hist(DATA$bilin_tclw)
hist(DATA$near_tclw)

hist(DATA$bilin_tcwv)
hist(DATA$near_tcwv)

range(DATA$Date)


plot(DATA[, bilin_tcc, Date])
lm1 <- lm(DATA[, Date, bilin_tcc])
abline(lm1, col = "red")

plot(DATA[, bilin_tclw, Date])
lm1 <- lm(DATA[, Date, bilin_tclw])
abline(lm1, col = "red")

plot(DATA[, bilin_tcwv, Date])
lm1 <- lm(DATA[, Date, bilin_tcwv])
abline(lm1, col = "red")


plot(DATA[, near_tcc, Date])
lm1 <- lm(DATA[, Date, near_tcc])
abline(lm1, col = "red")

plot(DATA[, near_tclw, Date])
lm1 <- lm(DATA[, Date, near_tclw])
abline(lm1, col = "red")

plot(DATA[, near_tcwv, Date])
lm1 <- lm(DATA[, Date, near_tcwv])
abline(lm1, col = "red")


saveRDS(DATA, "~/DATA/Clouds ERA5/LAP_tcc_tclw_tcwv.Rds", compress = "xz")

cat(paste("\n\n    DONE \n\n"))
