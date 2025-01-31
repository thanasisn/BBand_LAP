# /* Copyright (C) 2016-2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "Linke Turbidity Models."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Description of Linke turbidity models. Taken from *Identification of Periods of Clear Sky Irradiance in
#'  Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#'
#' output:
#'   html_document:
#'     toc: true
#' ---
#+ include=F, echo=F

source("~/BBand_LAP/parameters/theory/Air_mass_models.R")

#'
#' ### Linke Turbidity Definition ${TL}$ ###
#'
#' $$ I = I_0 \cdot \exp(-\delta_{cda}\cdot {TL} \cdot {AM} ) $$
#' $$ {TL} = \ln\left(\dfrac{I_0}{I} \right) \cdot \dfrac{1}{\delta_{cda} {AM}} $$
#'
#' Where ${AM}$ is an Air mass model.
#'
#' ### For $\delta_{cda}$  ###
#'


#'
#' #### Linke used: ####
#' $$ \delta_{cda} = 0.128 - 0.054 \cdot \log(AM) $$
#'
delta_linke <- function( AM ) {
    0.128 - 0.054 * log10(AM)
}


#'
#' #### Kasten used: ####
#' $$ \delta_{cda} = (9.4 + 0.9 \cdot AM)^{-1} $$
#'
delta_kasten <- function( AM ) {
    (9.4 + 0.9 * AM )**(-1)
}


#'
#' #### Louche used: ####
#' $$ \delta_{cda} = ( 6.5567 + 1.7513 \cdot AM   - 0.1202  \cdot AM^2 +
#'                              0.0065 \cdot AM^3 - 0.00013 \cdot AM^4 )^{-1} $$
delta_louche <- function( AM ) {
    ( 6.5567 + 1.7513 * AM - 0.1202 * AM**2 + 0.0065 * AM**3 - 0.00013 * AM**4 )**(-1)
}


#'
#' #### Grenier used: ####
#' $$\delta_{cda} = ( 5.4729 + 3.0312 \cdot AM   - 0.6329  \cdot AM^2 +
#'                               0.091 \cdot AM^3 - 0.00512 \cdot AM^4 )^{-1}$$
#'
delta_grenier <- function( AM ) {
    ( 5.4729 + 3.0312 * AM - 0.6329 * AM**2 + 0.091 * AM**3 - 0.00512 * AM**4 )**(-1)
}

#'
#' ### Linke turbidity from satellite data ###
#'
#' From **SoDa** project of Meteotest [www.meteotest.ch](http://www.meteotest.ch), we retrieve
#' Montly Means of Linke turbidity factor for 2003.
#'
TL_montly = c(3.0, 4.3, 4.6, 5.2, 6.1, 4.4, 6.0, 4.5, 4.0, 4.2, 3.8, 4.2)
#' Which we modify for better fit of clear sky models. Changed value for May.
TL_montly = c(3.0, 4.3, 4.6, 5.2, 4.5, 4.4, 6.0, 4.5, 4.0, 4.2, 3.8, 4.2)



sza <- seq(0, 90, .1)

plot(sza, delta_grenier(AM_simple(sza)))
plot(sza, delta_kasten(AM_simple(sza)))
plot(sza, delta_linke(AM_simple(sza)))
plot(sza, delta_louche(AM_simple(sza)))

