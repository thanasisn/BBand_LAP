# /* Copyright (C) 2016-2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "Clear sky irradiance models."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Description of clear sky irradiance models. Taken from *Identification of Periods of Clear Sky Irradiance in
#' Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#' output:
#'   bookdown::pdf_book:
#'     keep_tex: no
#'     toc     : no
#'     extra_dependencies:
#'       placeins: null
#'       float:    null
#'       flafter:  null
#'     latex_engine: xelatex
#'   html_document:
#'     toc: true
#' ---

#+ include=FALSE, echo=FALSE

## This script is for building a document of to be sourced in another.
## depends on: "Extraterrestial_radiation_models.R"

knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = "!ht"   )

library(pander)

source("~/CODE/R_myRtools/myRtools/R/trigonometric.R")
source("~/BBand_LAP/parameters/theory/Air_mass_models.R")
source("~/BBand_LAP/parameters/theory/Extraterrestrial_radiation_models.R")


alpha_models <- data.frame(
    DPP      = c( 0.9  , 1.25 , 1.049038 ),
    KC       = c( 1    , 1.25 , 1.154508 ),
    HAU      = c( 0.8  , 1.20 , 0.991353 ),
    BD       = c( 0.95 , 1.25 , 1.07257 ),
    ABCG     = c( 0.95 , 1.30 , 1.100368 ),
    RS       = c( 0.90 , 1.25 , 1.002708 ),
    KASTEN   = c( 0.95 , 1.25 , 1.125461 ),
    INEICHEN = c( 0.85 , 1.25 , 1.155573 ),
    row.names = c("lower","upper","best") )


#'
#' ## Very Simple Models for Determining the Irradiance on clear days. ##
#'
#' These very simple clear sky models are essentially empirical correlations
#' based on measurements for a site location and the astronomical parameters.
#'
#' ### 1. Daneshyar–Paltridge–Proctor (DPP) model (1978) ##
#'
#' $$ \text{DNI}     = 950.2 ( 1 - \exp( - 0.075 ( 90^{\circ} - z ))) $$
#' $$ \text{Diffuse} = 14.29 + 21.04 \left( \frac{\pi}{2} - z \frac{\pi}{180} \right) $$
#' $$ \text{GHI}     = \text{DNI} \times \cos(z) + \text{Diffuse} $$
#'
#+ include=T, echo=F
DPP <- function( sza,
                 a = 950.2,
                 b = 0.075,
                 c = 14.29,
                 d = 21.04 ) {
    DNI     <- a * ( 1 - exp(-b * ( 90 - sza )))
    Diffuse <- c + d * ( (pi / 2) - sza * (pi / 180))
    GHI     <- DNI * cosde(sza) + Diffuse
    return(GHI)
}



#'
#' ### 2. Kasten–Czeplak (KC) model (1980) ###
#'
#' $$ \text{GHI} = 910 \times \cos( z ) - 30 $$
#'
#+ include=T, echo=F
KC <- function( sza,
                a = 910,
                b = 30   ) {
    GHI <- a * cosde( sza ) - b
    return(GHI)
}



#'
#' ### 3. Haurwitz model (HAU) (1945)  ###
#'
#' $$ \text{GHI} = 1098 \times \cos( z ) \times \exp \left( \frac{ - 0.059}{\cos(z)} \right) $$
#'
#+ include=T, echo=F
HAU <- function( sza,
                 a = 1098,
                 b = 0.059 ) {
    GHI <- a * cosde(sza) * exp(-b / cosde(sza))
    return(GHI)
}
## Reno  0.057 -> Haurwitz 0.059 ??


#'
#' ### 4. Berger–Duffie (BD) model (1979)  ###
#'
#' $$ \text{GHI} = I_0 \times 0.70 \times \cos(z) $$
#'
#+ include=T, echo=F
BD <- function(sza,
               I_0 = EI_spencer(172),
               b   = 0.70             ) {
    GHI <- I_0 * b * cosde(sza)
    return(GHI)
}
warning("Make CSid I_O variable?")


#'
#' ### 5. Adnot–Bourges–Campana–Gicquel (ABCG) model (1979)  ###
#'
#' $$ \text{GHI} = 951.39 \times \left( \cos(z) \right)^{1.15} $$
#'
#+ include=T, echo=F
ABCG <- function(sza,
                 a = 951.39,
                 b = 1.15    ) {
    GHI <- a * (cosde(sza))^b
    return(GHI)
}



#'
#' ### 6. Robledo-Soler (RS) (2000)  ###
#'
#' $$ \text{GHI} = 1159.24 \times \left( \cos(z) \right)^{1.179} \times \exp(-0.0019 \times (90^{\circ}-z)) $$
#'
#'
#+ include=T, echo=F
RS <- function(sza,
               a = 1159.24,
               b = 1.179,
               c = 0.0019) {
    GHI <- a * cosde(sza)^b * exp( -c * ( 90 - sza ))
    return(GHI)
}



#'
#' ## Simple Models for Determining the Irradiance on clear days. ##
#'


#'
#' ### 7.  Kasten model ###
#'
#' $$ \text{GHI} =  0.84 \cdot I_0 \cdot \cos(z) \cdot \exp(-0.027 \cdot {AM} \cdot
#'                                                           (f_{h1} + f_{h2} ( {TL} - 1))) $$
#' $$  f_{h1} = \exp (-h / 8000 ) , \quad
#'     f_{h2} = exp (-h /1250 )  $$
#'
#+ include=T, echo=F
KASTEN <- function(sza,
                   h   = 63,
                   I_0 = 1366.1 ,
                   AM  = AM_simple,
                   TL  = 4.5        ) {
    f_h1 <- exp( -h / 8000 )
    f_h2 <- exp( -h / 1250 )

    GHI <- 0.84 * I_0 * cosde(sza) * exp(-0.027 * AM(sza) * (f_h1 + f_h2 * ( TL - 1)))
    return(GHI)
}

#'
#' ### 8. Ineichen and Perez ###
#' $$ \text{GHI} = c_{g1} \cdot I_0 \cdot \cos(z) \cdot \exp( -c_{g2} \cdot {AM} \cdot
#'                                                            ( f_{h1} + f_{h2} ( {TL} - 1))) \cdot
#'                                                            \exp(0.01 \cdot {AM}^{1.8} ) $$
#' $$ c_{g1} = 5.09 \cdot 10^{-5} \cdot h + 0.868  , \quad
#'    c_{g2} = 3.92 \cdot 10^{-5} \cdot h + 0.0387   $$
#'
#+ include=T, echo=F
INEICHEN <- function(sza,
                     h   = 63,
                     I_0 = 1366.1,
                     AM  = AM_simple,
                     TL  = 4.5        ## Linke factor?
                     ) {
    f_h1 <- exp( -h / 8000 )
    f_h2 <- exp( -h / 1250 )
    c_g1 <- 5.09e-5 * h + 0.868
    c_g2 <- 3.92e-5 * h + 0.0387
    AM   <- AM(sza)

    GHI <- c_g1 * I_0 * cosde(sza) * exp( -c_g2 * AM * (f_h1 + f_h2 * ( TL - 1)) ) * exp( 0.01 * AM**1.8)

    ## Mathematical break down prevention
    GHI[sza > 88] <- NA
    return(GHI)
}



#' ## Models comparison ##
#'
#' Here are plotted all the models with correction to fit the clear sky measured
#' irradiance.
#'


#+ include=T, echo=F

sza <- seq(0,90,.1)

plot(sza, DPP(sza), "l",
     col  = 2,
     lty  = 1,
     xlab = "SZA",
     ylab = expression(W %.% m^-2),
     ylim = c(0,1150))

lines(sza, DPP(sza),      lty = 1, col = 2 )
lines(sza, KC(sza),       lty = 1, col = 3 )
lines(sza, HAU(sza),      lty = 1, col = 4 )
lines(sza, BD(sza),       lty = 1, col = 5 )
lines(sza, ABCG(sza),     lty = 1, col = 6 )
lines(sza, RS(sza),       lty = 1, col = 7 )
lines(sza, KASTEN(sza),   lty = 1, col = 8 )
lines(sza, INEICHEN(sza), lty = 1, col = 9 )

# lines(sza, KC(sza)       * alpha_models$KC[3],       col = 3, lwd = 1.5)
# lines(sza, HAU(sza)      * alpha_models$HAU[3],      col = 4, lwd = 1.5)
# lines(sza, BD(sza)       * alpha_models$BD[3],       col = 5, lwd = 1.5)
# lines(sza, ABCG(sza)     * alpha_models$ABCG[3],     col = 6, lwd = 1.5)
# lines(sza, RS(sza)       * alpha_models$RS[3],       col = 7, lwd = 1.5)
# lines(sza, KASTEN(sza,   I_0 = EI_simple(182.5)) * alpha_models$KASTEN[3],   col = 8, lwd = 1.5)
# lines(sza, INEICHEN(sza, I_0 = EI_simple(182.5)) * alpha_models$INEICHEN[3], col = 9, lwd = 1.5)

legend("bottomleft",
       legend = c("DPP","KC","HAU","BD","ABCG","RS","KASTEN","INEICHEN"),
       lty = 1,
       col = c(2:9), bty = "n", cex = .8)


sza <- seq(17, 85,.1)

plot(sza, DPP(sza), "l",
     col  = 2,
     lty  = 1,
     xlab = "SZA",
     ylab = expression(W %.% m^-2),
     ylim = c(0,1010))

lines(sza, DPP(sza),      lty = 1, col = 2 )
lines(sza, KC(sza),       lty = 1, col = 3 )
lines(sza, HAU(sza),      lty = 1, col = 4 )
lines(sza, BD(sza),       lty = 1, col = 5 )
lines(sza, ABCG(sza),     lty = 1, col = 6 )
lines(sza, RS(sza),       lty = 1, col = 7 )
lines(sza, KASTEN(sza),   lty = 1, col = 8 )
lines(sza, INEICHEN(sza), lty = 1, col = 9 )

legend("bottomleft",
       legend = c("DPP","KC","HAU","BD","ABCG","RS","KASTEN","INEICHEN"),
       lty = 1,
       col = c(2:9), bty = "n", cex = .8)


