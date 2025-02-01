# /* Copyright (C) 2016-2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "Diffuse fraction models."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' date: "`r format(Sys.time(), '%F')`"
#' abstract: "Based on: *Minute resolution estimates of the diffuse fraction of global
#'   irradiance for southeastern Australia* N.A. Engerer."
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


knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'    )

# rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(mgcv,       warn.conflicts = FALSE, quietly = TRUE)



#'
#' ## Definitions
#'
#' ### Diffuse fraction $K_d$ ###
#' $$ K_d = \dfrac{E_{dh}}{E_{gh}}   = \dfrac{\text{diffuse horizontal irradiance}}{\text{global horizontal irradiance}} $$
#'
#' ### Clearness index $K_t$ ###
#' $$ K_t = \dfrac{E_{gh}}{E_{exth}} = \dfrac{\text{global horizontal irradiance}}{\text{horizontal component of extraterrestrial radiation}} $$
#'
#'
#'
#' ## Models
#'
#' ### Orgill and Hollands (1977) ###
#' Used: Orgill1977, Engerer2015
#'
#' 4 year of hourly data at Toronto
#'
#' $$ K_d =   1.0 - 0.249 \cdot K_t \ ,\qquad  0    \le K_t \le 0.35 $$
#' $$ K_d = 1.557 - 1.84  \cdot K_t \ ,\qquad  0.35 \le K_t \le 0.75 $$
#' $$ K_d = 0.177                   \ ,\qquad           K_t \ge 0.75 $$
#'
#+ include=T, echo=F
Org_Hol <- function(clearness_index,
                    a  = 0.249,
                    b  = 1.557,
                    c  = 1.84,
                    d  = 0.177,
                    l1 = 0.35,
                    l2 = 0.75 ) {
    diffuse_fraction <- clearness_index * NA

    p1 = 0  <= clearness_index & clearness_index  < l1
    p2 = l1 <= clearness_index & clearness_index <= l2
    p3 = l2 <  clearness_index

    diffuse_fraction[which(p1)] = 1 - a * clearness_index[which(p1)]
    diffuse_fraction[which(p2)] = b - c * clearness_index[which(p2)]
    diffuse_fraction[p3] = d

    return(diffuse_fraction)
}
Org_Hol_info = data.frame( latitude  = 43.70011,
                           longitude = -79.4163,
                           elevation = 175,
                           datastep  = "hourly",
                           date      = "1977")




#'
#' ### Erbs (1982) ###
#' Used: Erbs1982
#'
#' There is also seasonal models
#'
#' $$ K_d = 1.0 - 0.09 \cdot K_t         \ ,\qquad         K_t \le 0.22 $$
#' $$ K_d = 0.9511 -  0.1604 \cdot K_t
#'                 +  4.388  \cdot K_t^2
#'                 - 16.638  \cdot K_t^3
#'                 + 12.336  \cdot K_t^4 \ ,\qquad 0.22 \le K_t \le 0.80 $$
#' $$ K_d = 0.165                        \ ,\qquad          K_t \ge 0.80 $$
#'
#+ include=T, echo=F
Erbs <- function(clearness_index,
                 a  = 0.09,
                 b  = 0.9511,
                 c  = 0.1604,
                 d  = 4.388,
                 e  = 16.638,
                 f  = 12.336,
                 g  = 0.165,
                 l1 = 0.22,
                 l2 = 0.80) {
    diffuse_fraction <- clearness_index * NA

    p1 = which(                         clearness_index <= l1 )
    p2 = which( l1 <= clearness_index & clearness_index <= l2 )
    p3 = which( l2 <  clearness_index                         )

    diffuse_fraction[p1] = 1     - a * clearness_index[p1]
    diffuse_fraction[p2] = b -
        c * clearness_index[p2]    +
        d * clearness_index[p2]**2 -
        e * clearness_index[p2]**3 +
        f * clearness_index[p2]**4
    diffuse_fraction[p3] = g

    return(diffuse_fraction)
}
Erbs_info = data.frame( latitude  = c(  31.08,  37.70,  35.87,  42.42,  35.05  ),
                        longitude = c( -97.85, -121.7, -78.78, -71.48, -106.62 ),
                        elevation = c(  329.1,  148.1,  134.4,  61.9,   1619.7 ),
                        datastep  = "hourly, monthly",
                        date      = "1982" )



#'
#' ### Reindl model (1990) ###
#'
#' #### Reindl Full model ####
#' Used: Reindl1990
#'
#' The clearness index is the most important variable in the low and middle intervals
#' but at the high interval, the significance of the clearness index decreases
#' dramatically. The solar altitude effects are not as strong under cloudy
#' skies (low values of $K_t$,) but under clear skies (high values of $K_t$),
#' the solar altitude becomes the dominant predictor variable. For clear sky
#' conditions, the diffuse fraction increases for decreasing solar altitude
#' angles due to the longer path length required for radiation to travel.
#' These results are consistent with those found by Skartveit and Olseth.
#'
#' $$ K_d = 1.000 - 0.232 \cdot K_t + 0.0239 \cdot \sin(a) - 0.000682 \cdot T_a + 0.0195 \cdot \Phi \
#' ,\qquad   0 \le K_t \le 0.3,\  \text{ constraint: } K_d \le 1.0   $$
#' $$ K_d = 1.329 - 1.716 \cdot K_t + 0.267  \cdot \sin(a) - 0.00357  \cdot T_a + 0.106  \cdot \Phi \
#' ,\qquad 0.3 \le K_t \le 0.78,\ \text{ constraint: } K_d \le 0.97 \text{ and } K_d \ge 0.1  $$
#' $$ K_d =         0.426 \cdot K_t - 0.256  \cdot \sin(a) + 0.00349  \cdot T_a + 0.0734 \cdot \Phi \
#' ,\qquad         K_t \ge 0.78,\ \text{ constraint: } K_d \ge 0.1  $$
#'
#' $T_a$: \ Ambient temperature
#'
#' $a$: \ Solar altitude angle
#'
#' $\Phi$: Relative humidity
#'
Rein_FULL <- function( clearness_index, elevationS, temperatureS, relHumidityS ){
    diffuse_fraction <- clearness_index * NA
}



#'
#' #### Reindl Reduced model ####
#' Used: Reindl1990
#'
#' It would be desirable to provide a reduced form of the current correlation
#' for use when hourly ambient temperature and/or relative humidity data are not available.
#'
#' $$ K_d = 1.020 - 0.254 \cdot K_t + 0.0123 \cdot \sin(a) \ ,\qquad   0 \le K_t \le 0.3,\  \text{ constraint: } K_d \le 1.0  $$
#' $$ K_d = 1.400 - 1.749 \cdot K_t + 0.177  \cdot \sin(a) \ ,\qquad 0.3 \le K_t \le 0.78,\ \text{ constraint: } K_d \le 0.97 \text{ and } K_d \ge 0.1 $$
#' $$ K_d =         0.486 \cdot K_t - 0.182  \cdot \sin(a) \ ,\qquad         K_t \ge 0.78,\ \text{ constraint: } K_d \ge 0.1  $$
#'
#+ include=T, echo=F
Rein_REDUC <- function( clearness_index, elevationS,
                        a  = 1.020, b = 0.254, c = 0.0123,
                        d  = 1.400, e = 1.749, f = 0.177,
                        g  = 0.486, h = 0.182,
                        l1 = 0.3,
                        l2 = 0.78,
                        l3 = 0.1,
                        l4 = 0.97 ){
    diffuse_fraction <- clearness_index * NA

    part1 = a - b * clearness_index + c * sinde( elevationS )
    p1 = which( ( 0 <= clearness_index &  clearness_index <= l1 ) & ( part1 <= 1.0 ) )

    part2 = d - e * clearness_index + f * sinde( elevationS )
    p2 = which( ( l1 <  clearness_index & clearness_index <  l2 ) &
                    ( l3 <= part2           &           part2 <= l4 )   )

    part3 =     g * clearness_index - h * sinde( elevationS )
    p3 = which( (                         clearness_index >= l2 ) &
                    (                                   part3 >= l3  )  )

    diffuse_fraction[p1] = part1[p1]
    diffuse_fraction[p2] = part1[p2]
    diffuse_fraction[p3] = part1[p3]
    return(diffuse_fraction)
}

#'
#' #### Reindl ktcorr model ####
#' Used: Reindl1990
#'
#' This correlation will allow direct comparison of the new correlations
#' to the Liu and Jordan type correlations.
#'
#' $$ K_d = 1.020 - 0.248 \cdot K_t \ ,\qquad   0 \le K_t \le 0.3,\  \text{ constraint: } K_d \le 1.0 $$
#' $$ K_d = 1.45  - 1.67  \cdot K_t \ ,\qquad 0.3 \le K_t \le 0.78  $$
#' $$ K_d = 0.147                   \ ,\qquad         K_t \ge 0.78  $$
#'
#+ include=T, echo=F
Rein_ktcorr <- function(clearness_index,
                        a = 1.020, b = 0.248,
                        c = 1.45,  d = 1.67,
                        e = 0.147,
                        l1 = 0.3,
                        l2 = 0.78 ) {
    diffuse_fraction <- clearness_index * NA

    part1 = a - b * clearness_index
    p1 = which( ( 0 <= clearness_index &  clearness_index <= l1 ) & ( part1 <= 1.0 ) )

    part2 = c - d * clearness_index
    p2 = which( ( l1 <  clearness_index & clearness_index <  l2 ) )

    part3 = e
    p3 = which(                         clearness_index >= l2 )

    diffuse_fraction[p1] = part1[p1]
    diffuse_fraction[p2] = part2[p2]
    diffuse_fraction[p3] = part2[p3]

    return(diffuse_fraction)
}
Rein_info = data.frame( latitude  = c(  42.7,  28.4, 55.7, 53.5, 51.95, 59.56 ),
                        longitude = c( -73.8, -80.6, 12.6, 10.0, 10.22, 10.41 ),
                        elevation = c(  NA,    NA,   NA,   NA,   NA,    NA ),
                        datastep  = "hourly",
                        date      = "1990" )




#+ include=T, echo=F

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

LAP <- tbl(con, "LAP")

#' ## Data manipulations ##

CS_data <- LAP |>
  filter(GLB_strict > 5)      |> ## Drop too low global
  filter(HOR_strict > 5)      |> ## Drop too low direct
  filter(!is.na(HOR_strict))  |> ## Drop data without direct
  filter(SZA < 89)            |>
  select(
    Date,
    DiffuseFraction_kd,
    Transmittance_GLB,
    Elevat,
    SZA,
    CSRHv14_2_flag,
    month
  ) |>
  collect() |>
  data.table()


#' ## All data inspection ##
#+ include=T, echo=F
plot(CS_data$Transmittance_GLB, CS_data$DiffuseFraction_kd, pch = 20, cex = .4, xlab = "Clearness Index", ylab = "Diffuse Fraction")
title("All data")
curve( Org_Hol(clearness_index), add = T, xname = "clearness_index", col = 2 )
curve(    Erbs(clearness_index), add = T, xname = "clearness_index", col = 3 )
points(CS_data$Transmittance_GLB, Rein_REDUC(CS_data$Transmittance_GLB, CS_data$Elevat), pch = 20, cex = .4, col=4)
plot(CS_data$Transmittance_GLB, Rein_REDUC(CS_data$Transmittance_GLB, CS_data$Elevat), pch = 20, cex = .4, col=4)


#' ## Clear sky data inspection ##
#+ include=T, echo=F
clear_data = CS_data[CSRHv14_2_flag == 0, ]

plot(clear_data$Transmittance_GLB, clear_data$DiffuseFraction_kd, pch = 20, cex = .4, xlab = "Clearness Index", ylab = "Diffuse Fraction" )
title('"Clear sky" data')
curve(     Org_Hol(clearness_index), add = T, xname = "clearness_index", col = 2 )
curve(        Erbs(clearness_index), add = T, xname = "clearness_index", col = 3 )
# points(clear_data$Transmittance_GLB, Rein_REDUC(clear_data$Transmittance_GLB, clear_data$Elevat), pch = 20, cex = .4, col=4)
curve( Rein_ktcorr(clearness_index), add = T, xname = "clearness_index", col = 4 )

plot(clear_data$Transmittance_GLB, Rein_REDUC(clear_data$Transmittance_GLB, clear_data$Elevat), pch = 20, cex = .4, col=4)




# #+ include=T, echo=T
# CRSS(     Org_Hol(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# CRSS(        Erbs(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# CRSS(  Rein_REDUC(clear_data$Transmittance_GLB, clear_data$Elevat), clear_data$DiffuseFraction_kd )
# CRSS( Rein_ktcorr(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
#
# RMSE(     Org_Hol(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# RMSE(        Erbs(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# RMSE(  Rein_REDUC(clear_data$Transmittance_GLB, clear_data$Elevat), clear_data$DiffuseFraction_kd )
# RMSE( Rein_ktcorr(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
#
# MAE(     Org_Hol(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# MAE(        Erbs(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# MAE(  Rein_REDUC(clear_data$Transmittance_GLB, clear_data$Elevat), clear_data$DiffuseFraction_kd )
# MAE( Rein_ktcorr(clear_data$Transmittance_GLB), clear_data$DiffuseFraction_kd )
# #+ include=T, echo=F



for (amon in unique(CS_data$month) ) {
    monthly = CS_data$month == amon

    plot(CS_data$Transmittance_GLB[monthly], CS_data$DiffuseFraction_kd[monthly], pch = 20, cex =.4)
    ss = seq(0,1.1,.01)
    lines(ss, Org_Hol(ss), col = 2 )
    lines(ss, Erbs(ss),    col = 3 )
    title(main = amon)

    plot(CS_data$SZA[monthly], CS_data$DiffuseFraction_kd[monthly], pch = 20, cex =.4)
    title(main = amon)
}


for (amon in unique(CS_data$month) ) {
    # monthly = CS_data$month == amon

    clear_sky = CS_data$CSRHv14_2_flag == 0 & CS_data$month == amon
    mdata = CS_data[clear_sky,]

    plot(CS_data$Transmittance_GLB[clear_sky], CS_data$DiffuseFraction_kd[clear_sky], pch = 20, cex =.4)
    ss = seq(0,1.1,.01)
    #lines(ss, Org_Hol(ss), col=2 )
    lines(ss, Erbs(ss),   col=3 )
    title(main = amon)
    curve( Org_Hol(clearness_index),add = T,xname = "clearness_index",col=2)


    Org_Hol_MY <- nls( Transmittance_GLB ~ Org_Hol(DiffuseFraction_kd,a,c,d) ,data = CS_data,start = list(a = 0.249,
                                                                           c = 1.84,
                                                                           d = 0.177) )
    curve(Org_Hol(clearness_index,a=1.2694,b=0.8580,d=0.5888),add = T,xname = "clearness_index",col=2)

    plot(CS_data$SZA[clear_sky], CS_data$DiffuseFraction_kd[clear_sky], pch = 20, cex =.4)

}



# lm.l <- lm( DiffuseFraction_kd ~ Transmittance_GLB, data = clear_data)
# anova(lm.l)
# summary(lm.l)
#
#
#
#
#
#
# Org_Hol_opt <- function(x){
#     X_a <- x[1]
#     X_b <- x[2]
#     X_c <- x[3]
#     X_d <- x[4]
#
#     cost = mean(( Org_Hol(clearness_index = clear_data$Transmittance_GLB,
#                    a = X_a, b = X_b, c = X_c, d = X_d ) - clear_data$DiffuseFraction_kd)**2, na.rm = T )
#
#     # print(c(x,cost))
#     return(cost)
# }
#
#
# Org_Hol_opt_F <- function(x){
#     X_a  <- x[1]
#     X_b  <- x[2]
#     X_c  <- x[3]
#     X_d  <- x[4]
#     X_l1 <- x[5]
#     X_l2 <- x[6]
#
#
#     cost = mean(( Org_Hol_F(clearness_index = clear_data$Transmittance_GLB,
#                           a = X_a, b = X_b, c = X_c, d = X_d, l1 = X_l1, l2 = X_l2  ) - clear_data$DiffuseFraction_kd)**2, na.rm = T )
#
#     # print(c(x,cost))g
#     return(cost)
# }
#
#
#
# dfsfsf <- optim(c(0.249, 1.557, 1.84, 0.177) ,Org_Hol_opt)
#
# dfsfsf_F <- optim(c(0.249, 1.557, 1.84, 0.177, 0.35, 0.75) ,Org_Hol_opt_F, method = "BFGS")
#
# dfsfsf_F <- optim(c(0.249, 1.557, 1.84, 0.177, 0.35, 0.75) ,Org_Hol_opt_F, method = "CG")
#
# dfsfsf_F <- optim(c(0.249, 1.557, 1.84, 0.177, 0.35, 0.75) ,Org_Hol_opt_F, method = "L-BFGS-B")
#
#
#
# dfsfsf$par[1]
#
# plot(clear_data$Transmittance_GLB, clear_data$DiffuseFraction_kd, pch = 20, cex =.4)
# ss = seq(0,1.1,.01)
# title(main = amon)
# curve( Org_Hol(clearness_index),add = T,xname = "clearness_index",col=2)
# # curve(    Erbs(clearness_index),add = T,xname = "clearness_index",col=3)
# # plot( lm.l , pch = 20, cex =.4)
# # curve( Org_Hol(clearness_index, a =dfsfsf$par[1], b=dfsfsf$par[2],c=dfsfsf$par[3],d=dfsfsf$par[4]),add = T,xname = "clearness_index",col=2)
# curve( Org_Hol_F(clearness_index, a =dfsfsf_F$par[1],
#                                   b =dfsfsf_F$par[2],
#                                   c =dfsfsf_F$par[3],
#                                   d =dfsfsf_F$par[4],l1 =dfsfsf_F$par[5],l2 =dfsfsf_F$par[6] ),add = T,xname = "clearness_index",col=2)
#
#
#
# stop()
#
# xxx <- gam(DiffuseFraction_kd ~ s(Transmittance_GLB)+s(cosde(SZA)) ,data = mdata)
# xxx <- gam(DiffuseFraction_kd ~ s(Transmittance_GLB)+s(Transmittance_GLB)**2+s(Transmittance_GLB)**3+s(Transmittance_GLB)**4,data = mdata)
#
# summary(xxx)
# plot(xxx,pages=1,residuals=TRUE)  ## show partial residuals
# plot(xxx,pages=1,seWithMean=TRUE) ## `with intercept' CIs
# plot(xxx)
# ## run some basic model checks, including checking
# ## smoothing basis dimensions...
# gam.check(xxx)
#
#
#
# ###########
#
# # see also examples in ?gam.models (e.g. 'by' variables,
# ## random effects and tricks for large binary datasets)
#
# library(mgcv)
# set.seed(2) ## simulate some data...
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
# summary(b)
# plot(b,pages=1,residuals=TRUE)  ## show partial residuals
# plot(b,pages=1,seWithMean=TRUE) ## `with intercept' CIs
# ## run some basic model checks, including checking
# ## smoothing basis dimensions...
# gam.check(b)
#
# ## same fit in two parts .....
# G <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),fit=FALSE,data=dat)
# b <- gam(G=G)
# print(b)
#
# ## 2 part fit enabling manipulation of smoothing parameters...
# G <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),fit=FALSE,data=dat,sp=b$sp)
# G$lsp0 <- log(b$sp*10) ## provide log of required sp vec
# gam(G=G) ## it's smoother
#
# ## change the smoothness selection method to REML
# b0 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat,method="REML")
# ## use alternative plotting scheme, and way intervals include
# ## smoothing parameter uncertainty...
# plot(b0,pages=1,scheme=1,unconditional=TRUE)
#
# ## Would a smooth interaction of x0 and x1 be better?
# ## Use tensor product smooth of x0 and x1, basis
# ## dimension 49 (see ?te for details, also ?t2).
# bt <- gam(y~te(x0,x1,k=7)+s(x2)+s(x3),data=dat,
#           method="REML")
# plot(bt,pages=1)
# plot(bt,pages=1,scheme=2) ## alternative visualization
# AIC(b0,bt) ## interaction worse than additive
#
# ## Alternative: test for interaction with a smooth ANOVA
# ## decomposition (this time between x2 and x1)
# bt <- gam(y~s(x0)+s(x1)+s(x2)+s(x3)+ti(x1,x2,k=6),
#           data=dat,method="REML")
# summary(bt)
#
# ## If it is believed that x0 and x1 are naturally on
# ## the same scale, and should be treated isotropically
# ## then could try...
# bs <- gam(y~s(x0,x1,k=40)+s(x2)+s(x3),data=dat,
#           method="REML")
# plot(bs,pages=1)
# AIC(b0,bt,bs) ## additive still better.
#
# ## Now do automatic terms selection as well
# b1 <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat,
#           method="REML",select=TRUE)
# plot(b1,pages=1)
#
#
# ## set the smoothing parameter for the first term, estimate rest ...
# bp <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1),data=dat)
# plot(bp,pages=1,scheme=1)
# ## alternatively...
# bp <- gam(y~s(x0,sp=.01)+s(x1)+s(x2)+s(x3),data=dat)
#
#
# # set lower bounds on smoothing parameters ....
# bp<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),
#         min.sp=c(0.001,0.01,0,10),data=dat)
# print(b);print(bp)
#
# # same with REML
# bp<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),
#         min.sp=c(0.1,0.1,0,10),data=dat,method="REML")
# print(b0);print(bp)
#
#
# ## now a GAM with 3df regression spline term & 2 penalized terms
#
# b0 <- gam(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15),data=dat)
# plot(b0,pages=1)
#
# ## now simulate poisson data...
# dat <- gamSim(1,n=2000,dist="poisson",scale=.1)
#
# ## use "cr" basis to save time, with 2000 data...
# b2<-gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+
#             s(x3,bs="cr"),family=poisson,data=dat,method="REML")
# plot(b2,pages=1)
#
# ## drop x3, but initialize sp's from previous fit, to
# ## save more time...
#
# b2a<-gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr"),
#          family=poisson,data=dat,method="REML",
#          in.out=list(sp=b2$sp[1:3],scale=1))
# par(mfrow=c(2,2))
# plot(b2a)
#
# par(mfrow=c(1,1))
# ## similar example using performance iteration
# dat <- gamSim(1,n=400,dist="poisson",scale=.25)
#
# b3<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
#         data=dat,optimizer="perf")
# plot(b3,pages=1)
#
# ## repeat using GACV as in Wood 2008...
#
# b4<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
#         data=dat,method="GACV.Cp",scale=-1)
# plot(b4,pages=1)
#
# ## repeat using REML as in Wood 2011...
#
# b5<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
#         data=dat,method="REML")
# plot(b5,pages=1)
#
#
# ## a binary example (see ?gam.models for large dataset version)...
#
# dat <- gamSim(1,n=400,dist="binary",scale=.33)
#
# lr.fit <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
#               data=dat,method="REML")
#
# ## plot model components with truth overlaid in red
# op <- par(mfrow=c(2,2))
# fn <- c("f0","f1","f2","f3");xn <- c("x0","x1","x2","x3")
# for (k in 1:4) {
#     plot(lr.fit,residuals=TRUE,select=k)
#     ff <- dat[[fn[k]]];xx <- dat[[xn[k]]]
#     ind <- sort.int(xx,index.return=TRUE)$ix
#     lines(xx[ind],(ff-mean(ff))[ind]*.33,col=2)
# }
# par(op)
# anova(lr.fit)
# lr.fit1 <- gam(y~s(x0)+s(x1)+s(x2),family=binomial,
#                data=dat,method="REML")
# lr.fit2 <- gam(y~s(x1)+s(x2),family=binomial,
#                data=dat,method="REML")
# AIC(lr.fit,lr.fit1,lr.fit2)
#
# ## For a Gamma example, see ?summary.gam...
#
# ## For inverse Gaussian, see ?rig
#
# ## now 2D smoothing...
#
# eg <- gamSim(2,n=500,scale=.1)
# attach(eg)
#
# op <- par(mfrow=c(2,2),mar=c(4,4,1,1))
#
# contour(truth$x,truth$z,truth$f) ## contour truth
# b4 <- gam(y~s(x,z),data=data) ## fit model
# fit1 <- matrix(predict.gam(b4,pr,se=FALSE),40,40)
# contour(truth$x,truth$z,fit1)   ## contour fit
# persp(truth$x,truth$z,truth$f)    ## persp truth
# vis.gam(b4)                     ## persp fit
# detach(eg)
# par(op)
#
# ## Not run:
# ##################################################
# ## largish dataset example with user defined knots
# ##################################################
#
# par(mfrow=c(2,2))
# n <- 5000
# eg <- gamSim(2,n=n,scale=.5)
# attach(eg)
#
# ind<-sample(1:n,200,replace=FALSE)
# b5<-gam(y~s(x,z,k=40),data=data,
#         knots=list(x=data$x[ind],z=data$z[ind]))
# ## various visualizations
# vis.gam(b5,theta=30,phi=30)
# plot(b5)
# plot(b5,scheme=1,theta=50,phi=20)
# plot(b5,scheme=2)
#
# par(mfrow=c(1,1))
# ## and a pure "knot based" spline of the same data
# b6<-gam(y~s(x,z,k=64),data=data,knots=list(x= rep((1:8-0.5)/8,8),
#                                            z=rep((1:8-0.5)/8,rep(8,8))))
# vis.gam(b6,color="heat",theta=30,phi=30)
#
# ## varying the default large dataset behaviour via `xt'
# b7 <- gam(y~s(x,z,k=40,xt=list(max.knots=500,seed=2)),data=data)
# vis.gam(b7,theta=30,phi=30)
# detach(eg)
