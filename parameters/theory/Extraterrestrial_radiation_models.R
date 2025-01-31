# /* Copyright (C) 2016-2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "Extraterrestrial radiation models."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Description of extraterrestrial radiation models. Taken from *Identification of Periods of Clear Sky Irradiance in
#'  Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#'
#' output:
#'   html_document:
#'     toc: true
#' ---

#+ include=F, echo=F

#'
#' ### 1. Simple geometric description. ###
#'
#' Extraterrestrial radiation is calculated with a yearly varying term.
#'
#' $$ I_0 = 1367.7 \times \left( 1 + 0.033 \times \cos\left( \frac{2 \pi}{365}
#' \times \text{DOY}  \right)\right) $$
#'
#' or
#' $$ I_0 = 1367.7 \times \left( 1 + 0.033 \times \cos\left( \frac{2 \pi}{365}
#' \times \text{DOY}-81  \right)\right) $$
#'
EI_simple <- function(doy){
    # I_O = 1367.7 * ( 1 + 0.033 * cos( (doy - 81) * 2*pi / 365 )  )
    I_O = 1367.7 * ( 1 + 0.033 * cos( (doy) * 2*pi / 365 )  )
    return(I_O)
}


#'
#' ### 2. Spencer created a more detailed model through Fourier series. ###
#'
#' $$ I_0 = I_{SC} ( 1.00011 + 0.034221 \times \cos( x ) + 0.00128 \times \sin(x)
#' - 0.000719 \times \cos( 2 x) + 0.000077 \times \sin(2 x) ) $$
#' $$ \text{where: }\ I_{SC} = 1366.1 \tfrac{W}{m^2} $$
#' $$ \text{and }\  x = \frac{360^{\circ}}{365} (\text{DOY} - 81 ) $$
#'
EI_spencer <- function(doy, I_SC = 1366.1){
    x    = (360 / 365) * (doy)
    # x    = (360/365)*(doy - 81)

    I_O  = I_SC *
        (1.00011 + 0.034221 * cosde(x) + 0.00128 * sinde(x) - 0.000719 * cosde(2*x) + 0.000077 * sinde(2*x))
    return(I_O)
}

