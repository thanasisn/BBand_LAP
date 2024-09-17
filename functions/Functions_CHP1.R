
Sys.setenv(TZ = "UTC")

## Check date help function
is.POSIXct <- function(x) inherits(x, "POSIXct")


##  Compute radiation from signal  ---------------------------------------------

##  Calibration values for CHP1
chp1_calibration_data <- matrix(
##        Date,         Sensitivity [μV/W/m^2], Acquisition Gain []
       c("2016-04-01",  8.02,                   2000,
         "2016-04-02",  8.02,                   2000,  ## <- fake, just for interpolation
          NULL),
       byrow = TRUE,
       ncol  = 3)

## Format to data frame
chp1_calibration_data <- data.frame(Date        = as.POSIXct(chp1_calibration_data[,1]),
                                    Sensitivity = as.numeric(chp1_calibration_data[,2]),
                                    Gain        = as.numeric(chp1_calibration_data[,3]))

## Interpolation functions with extend to right rule
chp1_sensitivity <- approxfun(
    x      = chp1_calibration_data$Date,
    y      = chp1_calibration_data$Sensitivity,
    rule   = 1:2
)

chp1_gain        <- approxfun(
    x      = chp1_calibration_data$Date,
    y      = chp1_calibration_data$Gain,
    method = "constant",
    rule   = 1:2
)



## Conversion factor from volt to watt for CHP1 --------------------------------
#' Conversion factor from volt to watt for CHP1
#'
#' @details    This uses both the sensitivity of the instruments and the gain
#'             factor of the measurement device.
#'
#' @param date POSIXct date for the factor
#'
#' @return     (numeric) A conversion factor as ${Gain}/{Sensitivity}$
#' @export
chp1factor <- function(date) {
    if (is.POSIXct(date)) {
        return(chp1_gain(date) / chp1_sensitivity(date))
    } else {
        stop("input must be POSIXct.\n you gave : ",date)
    }
}





##  Get possible signal range on acquisition  ----------------------------------
#'
#' Be careful !! those values are used to define an envelope for the accepted
#' range for the dark signal also.
#'

## Upper and lower limits of the signal by period
chp1_signal_physical_limits <- matrix(
    c("2016-04-01 00:00:00",  -0.2,   5.0,
       NULL
    ),    byrow = TRUE,
    ncol = 3)

## Format to data frame
chp1_signal_physical_limits <- data.frame(
    Date      = as.POSIXct(chp1_signal_physical_limits[,1]),
    Lower_lim = as.numeric(chp1_signal_physical_limits[,2]),
    Upper_lim = as.numeric(chp1_signal_physical_limits[,3])
)


## Test the values in watts
# signal_physical_limits$Lower_radiation_lim <- cm21factor(signal_physical_limits$Date) * signal_physical_limits$Lower_lim
# signal_physical_limits$Upper_radiation_lim <- cm21factor(signal_physical_limits$Date) * signal_physical_limits$Upper_lim


##  Functions of upper and lower limits for acquisition signal  ----------------
chp1_signal_lower_limit <- approxfun(
    x      = chp1_signal_physical_limits$Date,
    y      = chp1_signal_physical_limits$Lower_lim,
    method = "constant",
    rule   = 1:2
)

chp1_signal_upper_limit <- approxfun(
    x      = chp1_signal_physical_limits$Date,
    y      = chp1_signal_physical_limits$Upper_lim,
    method = "constant",
    rule   = 1:2
)



## Temperature dependency of CHP-1  --------------------------------------------
#' Temperature dependency of CHP-1
#'
#' @param fun     Method of interpolation. "lin" Performing the linear interpolation.
#'                "spl_natural" Perform cubic (spline interpolation with "natural" method.
#' @details       The returned function take a temperature vector and returns
#'                a new vector of the CHP 1 signal dependency for each temperature given.
#' @return        A function of temperature dependency
#'
#' @family CHP 1 functions
#' @export
CHP_temp_dep <- function(fun = 'lin'){

    temperature_dependency <- data.frame(
        temperature = c( 50,  40,  30,  20,  10,   0, -10,  -20),
        dependency  = c(.08, .05, .03, .00, .01, .28, .12, -.26)
    )

    if (fun == "lin") {
        tem_dep <- approxfun(x = temperature_dependency$temperature,
                             y = temperature_dependency$dependency,
                             method = "linear", rule = 1, f = 0, ties = mean)
    } else if (fun == "spl_natural") {
        tem_dep <- splinefun(x = temperature_dependency$temperature,
                             y = temperature_dependency$dependency,
                             method = "natural", ties = mean)
    } else {
        cat("unknown option for CHP1 temperature dependency used\n")
        stop("unknown option for CHP1 temperature dependency used")
    }

    return(tem_dep)
}


## Thermistor resistance to temperature for CHP-1 ------------------------------
#' Convert Thermistor resistance to temperature for CHP-1.
#'
#' @details   Convert resistance in temperature using the equation:
#'            \deqn{ T = \left( a\cdot \left( b \cdot \ln(R) + c \cdot \ln^3(R) \right)  \right)^{-1} - 273.15}{T = ( a + ( b*log(res) + c*(log(res))^3 ) )^-1 - 273.15}
#'            \deqn{ a = 1.0295\cdot10^{-3},\quad b=2.391\cdot10^{-4},\quad c=1.568\cdot10^{-7}}{a = 1.0295*10^-3, b = 2.391*10^-4, c = 1.568*10^-7}
#' @param res Resistance in Ohm
#' @return    Temperature in Celsius
#'
#' @export
#' @family CHP 1 functions
CHP_thermistor_R_to_T <- function(res){
    if ( !is.na(res) & res > 0 ) {
        a    <- 1.0295 * 10^-3
        b    <- 2.391  * 10^-4
        c    <- 1.568  * 10^-7
        return( ( a + ( b*log(res) + c*(log(res))^3 ) )^-1 - 273.15 )
    } else {
        return(NA)
    }
}
CHP_thermistor_R_to_T <- Vectorize(CHP_thermistor_R_to_T)



## Uncertainty for the function `CHP_thermistor_R_to_T` ------------------------
#' Calculate uncertainty with error propagation theory for \code{\link{CHP_thermistor_R_to_T}}
#'
#' @details      Use the error propagation to implement the following equation:
#'               \deqn{ T_{unc} = \sqrt{ \left(\frac{\partial T_{therm}}{\partial R_{therm}}\right)^2 \cdot (\Delta R)^2} }
#'               \deqn{ T_{unc} = \sqrt{  \left(\frac{b + 3\cdot c\cdot \ln^2(R_{therm}) }{R_{therm}( a + b\cdot \ln(R_{therm}) + c\cdot \ln^3(R_{therm}))^2 }\right)^2 \cdot R_{unc}^2 } }
#'               \deqn{ a = 1.0295\cdot10^{-3},\quad b=2.391\cdot10^{-4},\quad c=1.568\cdot10^{-7} }
#' @param res    Resistance in Ohm
#' @param resUnc Resistance uncertainty
#' @return       Temperature uncertainty
#'
#' @export
#' @family CHP 1 functions
CHP_thermistor_ResUnc_to_TempUnc <- function(res, resUnc){
    if ( !is.na(res) & res > 0) {
        a    <- 1.0295 * 10**-3
        b    <- 2.391  * 10**-4
        c    <- 1.568  * 10**-7
        Tunc <- sqrt( ( - ( b + 3 * c * log(res)^2 ) / ( res * ( a + b * log(res) + c * log(res)^3 )^2 ) )^2 * (resUnc)^2 )
    } else {
        Tunc <- NA
    }
    return(Tunc)
}
CHP_thermistor_ResUnc_to_TempUnc <- Vectorize(CHP_thermistor_ResUnc_to_TempUnc)




## Protek 506 uncertainty for a resistance measurement -------------------------
#' Gives the Protek 506 uncertainty for a resistance measurement
#'
#' @details   In the range of 400 to 400kOhm, Protek 506 has uncertainty: \eqn{ \pm(0.5\% + 2d)}.
#'            We implement the following formula.
#'            \deqn{ R_{unc} = R [{\Omega}] \cdot\frac{0.5}{100} + 20 [{\Omega}] }{ R_{unc} = R [\Omega] 0.5/100 + 20 [\Omega]}
#'            Our measurement are converted to [Ohm]. And the thermistors range is 135200 [Ohm] to 2854 [Ohm]
#'            for corresponding temperature -30[°C] to 59[°C].
#' @param res The measured resistance
#' @return    The uncertainty for this resistance measurement with Protek 506
#'
#' @family CHP 1 functions
#' @export
Protek_506_R_error <- function(res){
    if ( !is.nan(res) & !is.na(res) & 2850 < res & res < 135200){
        Merr <- res * 0.5 / 100 + 20
    } else {
        # warning(paste("Protek 506 resistance value out of bound: ",res))
        Merr <- NA
    }
    return(Merr)
}
Protek_506_R_error               <- Vectorize(Protek_506_R_error)



