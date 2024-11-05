#!/usr/bin/env Rscript
#' Copyright (C) 2016 Athanasios Natsis <natsisphysicist@gmail.com>
#'
#' parse sounding data from airport
#'

#### Clear environment
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/uwyo/parse_uwyo.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/LOGs/uwyo/", basename(sub("\\.R$",".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/uwyo/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}

source("~/CODE/R_myRtools/myRtools/R/write_.R")

## INPUTS
inputdir       <- "~/DATA_RAW/uwyo/LGTS"
knownvariables <- 31

## OUTPUT
graphfl   <- "~/Aerosols/DATA/Graphs/Weather/LGTS_soundings.pdf"
DATAOUT   <- "~/DATA/WEATHER/LGTS_soundings"
tabledir  <- "~/DATA/SOUNDINGS/uwyo"


## files to read
infiles <- list.files(path       = inputdir,
                      pattern    = "^LGTS_[0-9]{4}-.*.txt.xz",
                      full.names = T )

## subset of all files for test
# infiles <- grep("_20",infiles, value = T)

## we assume we know all variables used
variablenames <- c(
  "Stationidentifier"                  ,   "Stationnumber"                         ,
  "Observationtime"                    ,   "Stationlatitude"                       ,
  "Stationlongitude"                   ,   "Stationelevation"                      ,
  "Showalterindex"                     ,   "Liftedindex"                           ,
  "LIFTcomputedusingvirtualtemperature",   "SWEATindex"                            ,
  "Kindex"                             ,   "Crosstotalsindex"                      ,
  "Verticaltotalsindex"                ,   "Totalstotalsindex"                     ,
  "ConvectiveAvailablePotentialEnergy" ,   "CAPEusingvirtualtemperature"           ,
  "ConvectiveInhibition"               ,   "CINSusingvirtualtemperature"           ,
  "EquilibrumLevel"                    ,   "EquilibrumLevelusingvirtualtemperature",
  "LevelofFreeConvection"              ,   "LFCTusingvirtualtemperature"           ,
  "BulkRichardsonNumber"               ,   "BulkRichardsonNumberusingCAPV"         ,
  "Temp_K_oftheLiftedCondensationLevel",   "Pres_hPa_oftheLiftedCondensationLevel" ,
  "Equivalentpotentialtemp_K_oftheLCL" ,
  "Meanmixedlayerpotentialtemperature" ,   "Meanmixedlayermixingratio"             ,
  "P1000hPato500hPathickness"          ,   "Precipitablewater_mm_forentiresounding"
)

stopifnot(length(variablenames) == knownvariables)
## empty df of char
gather <- data.frame(matrix(ncol = knownvariables, nrow = 0))
colnames(gather) <- variablenames


for (afile in infiles) {
  print(afile)

  if (file.size(afile) < 500) next()

  ## get indexes to separate data in monthly file
  lines      <- readLines(afile)
  tablestart <- grep("16622 LGTS Thessaloniki \\(Airport\\) Observations at", x = lines)
  ## start fo list
  liststart  <- grep("Station information and sounding indices",    x = lines) + 2
  ## last line of list
  lass       <- grep("Description of the \\[[0-9]\\]data columns or \\[[0-9]\\]sounding indices.", x = lines)
  # ## not safe end of list
  # listend    = grep("Precipitable water \\[mm\\] for entire sounding", x = lines)

  # lines[tablestart]
  # lines[liststart]
  # lines[lass]

  # if (tablestart - 2 < 1) {
  #     cat( paste("Need better method to read:",afile))
  #     warnings( paste("Need better method to read:",afile))
  #     next()
  # }

  ## safer partial list end
  listendff  <- (tablestart - 2)[2:(length(tablestart))]
  ## safe list end
  listend    <- c(listendff, lass - 2 )

  # lines[lass-2]
  # length(listend)
  # listend[5] -    liststart[5]
  # which( listend - liststart + 1 > knownvariables )
  # lines[liststart[5]]

  if (any(listend - liststart + 1 > knownvariables)) {
    cat("New variables! have to check source code.\nHave to add them manual to continue\n")
    stop("New variables, have to check source code.")
  }

  ## loop all blocks of text in monthly file
  for (i in 1:length(tablestart)) {

    ## get table
    tt        <- lines[tablestart[i]]
    ## table datetime
    tabledate <- strptime(unlist(strsplit(tt, "tions at "))[2], format = "%HZ %d %b %Y")
    outfile   <- paste0(tabledir, "/LGTS_table_", strftime(tabledate, "%F_%H.dat.xz"))

    ## write if file is missing
    if (! file.exists(outfile)){
      # head = lines[ (tablestart[i]+3):(tablestart[i]+4) ]
      ## read table for export no completly parsed
      tablef <- read.fwf(textConnection(lines[ (tablestart[i]+3):(liststart[i]-3)]),
                         widths = c(8,7,7,7,7,7,7,7,7,7,7)
      )

      # ## read table for import correct data parse
      # table = read.fwf( textConnection(lines[ (tablestart[i]+6):(liststart[i]-2)]),
      #           widths = c(8,7,7,7,7,7,7,7,7,7,7),
      # )

      write.table(x         = tablef,
                  file      = xzfile(outfile),
                  quote     = F,
                  na        = "       ",
                  row.names = F,
                  col.names = F)

      # ## can be read with
      # read.fwf(outfile,widths = c(8,7,7,7,7,7,7,7,7,7,7))
    }

    ## get total of each sounding
    ff      <- lines[(liststart[i]):listend[i]]
    # print( ( listend[i] - liststart[i]  )  )

    ## collapse spaces
    ff      <- gsub(" ", "", ff)
    ## spit var name and value
    gg      <- strsplit(ff, split = ":")
    ## separate names from values
    vec     <- unlist(gg)
    variab  <- vec[seq(1, to = length(vec), 2)]
    data    <- vec[seq(2, to = length(vec), 2)]
    ## fix variable names this must be used and elsewhere in the code
    variab  <- gsub(pattern = "^1000", replacement = "P1000", variab)
    variab  <- gsub(pattern = '\\[|\\]', replacement = "_", variab)

    rectemp <- data.frame()
    rectemp <- rbind(rectemp, data)
    names(rectemp) <- variab

    # data = t( data.frame(data,  stringsAsFactors = F) )
    # names(data) <- variab

    # gather = rbind(gather,data)
    gather  <- merge(x = gather, y = rectemp, all = T)
  }##END loop block of text in afile
}##END loop all infiles


# ## fix variable names this must be used and elsewhere in the code
# variab = gsub(pattern = "^1000", replacement = "P1000", variab)
# variab = gsub(pattern = '\\[|\\]', replacement = "_", variab)
# colnames(gather) <- variab

## columns with data
wecare <- grep("Station*|Observationtime", variablenames, invert = T, value = T)

## format data
LGTS_dat                   <- apply( gather[,wecare], MARGIN = 2,
                                     function(x) as.numeric(as.character(x)) )
LGTS_dat                   <- as.data.frame(LGTS_dat)
LGTS_dat$Stationidentifier <- as.character( gather$Stationidentifier )
LGTS_dat$Date              <- as.POSIXct( as.character(gather$Observationtime),
                                          format = "%y%m%d/%H%M")


## remove some bad days
rmdates <- c( "2016-05-16 06:00" )

for (bd in rmdates) {
  LGTS_dat <- LGTS_dat[ ! LGTS_dat$Date == as.POSIXct(bd), ]
}

#### write sounding data to file ####
write_RDS( LGTS_dat,  DATAOUT)
write_prqt(LGTS_dat, DATAOUT)



#### make plots for out data range ####

dddd <- LGTS_dat$Date > as.POSIXct("2016-04-01")
## all variables
pdf(file = graphfl, onefile = T )
ss <- par("mar"); par(mar = c(3,3,1,.5))

for (cc in wecare) {
  plot(LGTS_dat$Date[dddd], LGTS_dat[dddd, cc],
       main = cc, cex.main = .8, cex.axis = .9,
       pch = 19, cex = .3)
}

for (cc in wecare) {
  plot(LGTS_dat$Date, LGTS_dat[, cc],
       main = cc, cex.main = .8, cex.axis = .9,
       pch = 19, cex = .3)
}

par(mar = ss)


tac = Sys.time()
cat(sprintf("\n%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
