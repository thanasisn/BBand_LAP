# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Functions to calculate the dark signal and apply corrections.

Sys.setenv(TZ = "UTC")



#' Calculate dark statistics for a day
#'
#' @param elevatio   Vector record of sun elevation
#' @param nightlimit Sun elevation limit to consider night time
#' @param dates      Vector record of dates
#' @param dstretch   Duration of dark signal sample
#' @param values     Vector of signal values
#'
#' @return           Data frame with dark signal statistical values
#' @export
#'
dark_calculations <- function(elevatio,
                              nightlimit,
                              dates,
                              dstretch,
                              values) {

    require(zoo,   quietly = TRUE, warn.conflicts = FALSE)

    ## Check input requirements
    if (!all(!is.na(elevatio))) {
        cat("\n\nSTOP: NAs in elevation vector!!\n\n")
        stop("NAs in elevation vector!!")
    }
    if (!all(!is.na(dates))) {
        cat("\n\nSTOP: NAs in dates vector!!\n\n")
        stop("NAs in dates vector!!")
    }


    ## suppress warnings zoo::index
    # suppressWarnings({
        ## find local noun
        nounindex    <- match( max(elevatio), elevatio )
        ## split day in half
        selectmorn   <- elevatio < nightlimit & index(elevatio) < nounindex
        selecteven   <- elevatio < nightlimit & index(elevatio) > nounindex
        ## all morning and evening dates
        morning      <- dates[selectmorn]
        evening      <- dates[selecteven]

        ## morning selection with time limit
        mornigend    <- morning[max(index(morning))]
        mornigstart  <- mornigend - dstretch
        ## selection for morning dark
        morningdark  <- selectmorn & dates <= mornigend & mornigstart < dates

        ## evening selection with time limit
        eveningstart <- evening[min(index(evening))]
        eveningend   <- eveningstart + dstretch
        ## selection for evening dark
        eveningdark  <- selecteven & dates >= eveningstart & dates < eveningend
    # })

    return(
        data.frame(
            Mavg = mean(      values[morningdark],  na.rm = TRUE ),
            Mmed = median(    values[morningdark],  na.rm = TRUE ),
            Msta = max(       dates[ morningdark],  na.rm = TRUE ),
            Mend = min(       dates[ morningdark],  na.rm = TRUE ),
            Mcnt = sum(!is.na(values[morningdark])),
            Eavg = mean(      values[eveningdark],  na.rm = TRUE ),
            Emed = median(    values[eveningdark],  na.rm = TRUE ),
            Esta = min(       dates[ eveningdark],  na.rm = TRUE ),
            Eend = max(       dates[ eveningdark],  na.rm = TRUE ),
            Ecnt = sum(!is.na(values[eveningdark]))
        )
    )
}




#' Create dark correction offset signal
#'
#' @param dark_day     Data.frame from `dark_calculations`
#' @param DCOUNTLIM    Minimum number of points for a valid dark calculation.
#' @param type         Use  "median" or "mean" method for dark calculation
#' @param adate        Date for message
#' @param test         Message text to insert
#' @param missfiles    File for message
#' @param missingdark  Use this value when no dark can be calculated
#'
#' @return
#' @export
#'
#' @examples
dark_function <- function(dark_day,
                          DCOUNTLIM,
                          type,
                          adate,
                          test,
                          missfiles,
                          missingdark) {
    ## ignore dark signal if too low counts
    if (dark_day$Mcnt < DCOUNTLIM) { dark_day$Mmed = dark_day$Mavg = NA }
    if (dark_day$Ecnt < DCOUNTLIM) { dark_day$Emed = dark_day$Eavg = NA }

    if (type == "mean") {
        # print("use mean for dark correction")
        ## Values to use
        dar_x = c(dark_day$Msta, dark_day$Esta)
        dar_y = c(dark_day$Mavg, dark_day$Eavg)

    } else if (type == "median") {
        # print("use median for dark correction")
        ## Values to use
        dar_x = c(dark_day$Msta, dark_day$Esta)
        dar_y = c(dark_day$Mmed, dark_day$Emed)

    } else {
        print("Don't know what to do")
        stop("Invalid argument in Dark correction")
    }


    ## Choose scenario based on available dark information
    ## with left and right dark
    if (sum(is.na( dar_y )) == 0) {
        dark_line = approxfun(x = dar_x,
                              y = dar_y,
                              rule = 2,
                              method = "linear")
    }

    ## with only left dark
    if ( sum(is.na( dar_y )) == 1 ) {
        dark_line = approxfun(x = dar_x[!is.na( dar_y )],
                              y = dar_y[!is.na( dar_y )],
                              method = "constant",
                              rule = 2 )
    }

    ## with out dark should break!
    if ( sum(is.na( dar_y )) >= 2 ) {
        text = paste( as.POSIXct(adate, origin = "1970-01-01" ), test, "No dark don't know what to do!! using ", missingdark, " for Dark!!" )
        # cat(text, sep = "\n\n")
        warning(text)
        cat(text, sep = "\n", file = missfiles, append = TRUE)

        ## protect from empty variable
        if (is.na(missingdark)) {
            stop("no value for dark given")
        }

        ## create dummy dark signal for correction
        dark_line = approxfun(x = dar_x,
                              y = c(missingdark, missingdark),
                              rule = 2,
                              method = "linear")

    }
    return(dark_line)
}
