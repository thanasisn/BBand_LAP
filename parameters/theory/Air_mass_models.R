# /* Copyright (C) 2016-2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "Air mass models."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Description of air mass models. Taken from *Identification of Periods of Clear Sky Irradiance in
#'  Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#' output:
#'   html_document:
#'     toc: true
#' ---

#+ include=F, echo=F

## This script is for building a document of to be sourced in another.

# require(RAerosols)
source("~/CODE/R_myRtools/myRtools/R/trigonometric.R")



#'
#' ### 1. Simple ###
#'
#' The simple approach is adequate for zenith angles as large as 80$^\circ$.
#'
#' $$ AM = \frac{1}{\cos(z)} $$
#'
AM_simple <- function( sza ){
    1 /
    cosde( sza )
}


#'
#' ### 2. Kasten and Young ###
#'
#' $$ AM = \dfrac{1}{\cos(z) + 0.50572 (96.07995 - z )^{-1.6354} } $$
#'
AM_kasten_young <- function( sza ) {
    1 /
    ( cosde(sza) + 0.50572 * (96.07995 - sza )**(-1.6354) )
}


#'
#' ### 3. Young ###
#'
#' $$ AM = \dfrac{1.002432 \cdot \cos^2(z) + 0.148386 \cdot \cos(z) + 0.0096467 }
#'               {\cos^3(z) + 0.149864 \cdot \cos^2(z) + 0.0102963 \cdot \cos(z) + 0.000303978} $$
#'
AM_young <- function( sza ) {
    ( 1.002432 * cosde(sza)**2 + 0.148386 * cosde(sza) + 0.0096467 ) /
    ( cosde(sza)**3 + 0.149864 * cosde(sza)**2 + 0.0102963 * cosde(sza) + 0.000303978)
}


#'
#' ### 4. Rodgers ###
#'
#' $$ AM = \dfrac{35}{ \sqrt{1224 \cdot \cos^2(z) + 1} } $$
#'
AM_rodgers <- function(sza) {
    35 /
    ( sqrt( 1224 * cosde(sza)**2 + 1 ) )
}


#'
#' ### 5. Gueymard ###
#'
#' $$ AM = \dfrac{1}
#'               { \cos(z) + 0.00176759 \cdot z \cdot (94.37515 - z )^{-1.21563} } $$
#'
#'
AM_gueymard <- function( sza ) {
    1 /
    ( cosde(sza) + 0.00176759 * sza * ( 94.37515 - sza )**-1.21563 )
}


par(mar = c(4,4,.5,.5))

#+ airmas_sza_0-90, include=T, echo=F
par(mar = c(4,4,.5,.5))
sssss = seq(0,90,.1)
plot( sssss, AM_simple(sssss) , "l", ylim = c(0,40), col = 1, lwd = 2,
      xlab = "Zenith Sun angle", ylab = "Air mass")
lines(sssss, AM_kasten_young(sssss), col = 2, lwd = 2)
lines(sssss, AM_young(sssss),        col = 3, lwd = 2)
lines(sssss, AM_rodgers(sssss),      col = 4, lwd = 2)
lines(sssss, AM_gueymard(sssss),     col = 5, lwd = 2)

legend("topleft", legend = c("Parallel Plate Approximation",
                             "Kasten Young", "Young", "Rodgers", "Gueymard"),
       col = c(1,2,3,4,5),
       lwd = 2, lty = 1, bty = "n")


#+ airmas_sza_70-90, include=T, echo=F
par(mar = c(4,4,.5,.5))
sssss = seq(70,90,.1)
plot( sssss, AM_simple(sssss) , "l", ylim = c(2,40), col = 1, lwd = 2,
      xlab = "Zenith Sun angle", ylab = "Air mass")
lines(sssss, AM_kasten_young(sssss), col = 2, lwd = 2)
lines(sssss, AM_young(sssss),        col = 3, lwd = 2)
lines(sssss, AM_rodgers(sssss),      col = 4, lwd = 2)
lines(sssss, AM_gueymard(sssss),     col = 5, lwd = 2)

legend("topleft", legend = c("Parallel Plate Approximation",
                             "Kasten Young", "Young", "Rodgers", "Gueymard"),
       col = c(1,2,3,4,5),
       lwd = 2, lty = 1, bty = "n")


#+ airmas_sza_0-70, include=T, echo=F
par(mar = c(4,4,.5,.5))
sssss = seq(0,70,.1)
plot( sssss, AM_simple(sssss) , "l", ylim = c(0.9,3), col = 1, lwd = 2,
      xlab = "Zenith Sun angle", ylab = "Air mass")
lines(sssss, AM_kasten_young(sssss), col = 2, lwd = 2)
lines(sssss, AM_young(sssss),        col = 3, lwd = 2)
lines(sssss, AM_rodgers(sssss),      col = 4, lwd = 2)
lines(sssss, AM_gueymard(sssss),     col = 5, lwd = 2)

legend("topleft", legend = c("Parallel Plate Approximation",
                             "Kasten Young", "Young", "Rodgers", "Gueymard"),
       col = c(1,2,3,4,5),
       lwd = 2, lty = 1, bty = "n")


