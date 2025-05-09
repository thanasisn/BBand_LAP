# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */


#+ echo=F, include=T

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")


## __ Source initial scripts  --------------------------------------------------
source("./GHI_enh_00_variables.R")
source("./GHI_enh_00_dictionary.R")

##  Prepare raw data if needed  ------------------------------------------------
if (
    file.exists(raw_input_data) == FALSE |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_00_variables.R") |
    file.mtime(raw_input_data) < file.mtime("./GHI_enh_01_raw_data.R")
) {
    source("./GHI_enh_01_raw_data.R")
    dummy <- gc()
}


## __ Execution control  -------------------------------------------------------
TEST <- FALSE
# TEST <- TRUE

# APPLY_TRANS <- FALSE
APPLY_TRANS <- TRUE


##  Load Enhancement data  -----------------------------------------------------
DATA <- readRDS(raw_input_data)


##  Add CS data from Libradtran  -----------------------------------------------
DATA <- merge(DATA, readRDS("./data/lookuptable_datatable.Rds"))


##  Reset Randomness  ----------------------------------------------------------
RANDOM_SEED <- 333
set.seed(RANDOM_SEED)
## may need to reset seed in each randomness generation below


##  Choose CS data to use ------------------------------------------------------
DATA$TYPE |> unique()
grep("Exact_B", names(DATA), value = T)



##  Get Kurudz Solar constant  -------------------------------------------------

Kurudz <- read.table("~/LibRadTranG/libRadtran-2.0.5/data/solar_flux/kurudz_0.1nm.dat")
Kurudz <- data.table(Kurudz)

# currently we only on set to run Libradtran
spectrum  <- Kurudz[ V1 >= 280 & V1 <= 2500]
Kurudz_SC <- trapz(x = spectrum$V1, y = spectrum$V2) / 1000

summary(DATA[, tsi_1au_comb / Kurudz_SC ])
# plot(DATA[, tsi_1au_comb / Kurudz_SC ])

DATA[, TSI_Kurudz_factor := tsi_1au_comb / Kurudz_SC ]



#'
#'  Alpha * HAU is CS_ref
#'
#+ echo=F, include=TRUE

## __ Enhancement criteria  ----------------------------------------------------
# SelEnhanc <- "Enhanc_C_1"
# SelEnhanc <- "Enhanc_C_2"
# SelEnhanc <- "Enhanc_C_3"

SelEnhanc <- "Enhanc_C_4"


## Mark used criteria for diff rati ench
DATA[, CEC := SelEnhanc ]


#'
#' ## Using criteria **`r SelEnhanc`** for final application
#'
#+ echo=FALSE, include=TRUE


#'
#' ## 1. Use TSI as reference for Cloud-free.
#'
#+ echo=TRUE, include=TRUE



#'
#' ## 2. Use TSI as reference for Cloud-free.
#'
#+ echo=TRUE, include=TRUE



#'
#' ## 3. Use Haurwitz as reference for Cloud-free.
#'
#+ echo=TRUE, include=TRUE



## __ 4. my Criteria  ----------------------------------------------------------

## set values base
csmodel <- "Low_B.Low_W"



if (APPLY_TRANS) {
  cat("USING TRASPARENCY TREND\n")

  # source("./GHI_enh_07_Aerosols.R")
  ## choose function
  # trans_trend <- trend_median
  # trans_trend <- trend_median_adj
  # trans_trend <- trend_55_adj

  source("./GHI_enh_07_Aerosols_BR_CIM.R")
  # trend_polyf(1994:2024)
  # plot(trend_polyf(1994:2024), trend_55_adj(1994:2024))
  trans_trend <- trend_polyf

  trans_trend(2000)

  ## TEST
  ## remove long term change of GHI for testing
  # warning("!! THIS IS A TEST !!")
  # trans_trend <- function(x) {x * 0}
  # trans_trend(2000)

} else {
  trans_trend <- function(x) {x * 0}
}


#'
#' ## 4. Use libradtran **`r csmodel`** as reference for Cloud-free.
#'
#+ echo=TRUE, include=TRUE
cat("\n USING CSMODE:",    csmodel,     "\n\n")
cat("\n APPLY GHI TREND:", APPLY_TRANS, "\n\n")

switch(csmodel,
       Exact_B.Exact_W = { C4_cs_ref_ratio <- 1.02; C4_GLB_diff_THRES <- 55; C4_lowcut_sza <- 60; C4_lowcut_ratio <- 1.12},
       Low_2_B.Low_2_W = { C4_cs_ref_ratio <- 1.03; C4_GLB_diff_THRES <-  5; C4_lowcut_sza <- 60; C4_lowcut_ratio <- 1.12},
       Low_B.Exact_W   = { C4_cs_ref_ratio <- 1.04; C4_GLB_diff_THRES <- 20; C4_lowcut_sza <- 60; C4_lowcut_ratio <- 1.12},
       Low_B.High_W    = { C4_cs_ref_ratio <- 1.05; C4_GLB_diff_THRES <- 20; C4_lowcut_sza <- 60; C4_lowcut_ratio <- 1.12},
       # Low_B.Low_W     = { C4_cs_ref_ratio <- 1.04; C4_GLB_diff_THRES <- 15; C4_lowcut_sza <- 90; C4_lowcut_ratio <- 1.04}, ## Submitted pater
       Low_B.Low_W     = { C4_cs_ref_ratio <- 1.00; C4_GLB_diff_THRES <- 25; C4_lowcut_sza <- 90; C4_lowcut_ratio <- 1.00},   ## Values from validation
                         { C4_cs_ref_ratio <- 1   ; C4_GLB_diff_THRES <-  0; C4_lowcut_sza <- 90; C4_lowcut_ratio <- 1   })


## init flag
DATA[, Enhanc_C_4 := FALSE]

C4_test_cs_ref_ratio   <-  1.05
C4_test_GLB_diff_THRES <-  0
C4_test_lowcut_sza     <- 90  ## Disabled
C4_test_lowcut_ratio   <-  1.18

# DATA[, max(SZA)]

smo <- approxfun(
  x = c(90 - BIO_ELEVA,  C4_lowcut_sza  ),
  y = c(C4_lowcut_ratio, C4_cs_ref_ratio)
)

smo_test <- approxfun(
  x = c(90 - BIO_ELEVA,       C4_test_lowcut_sza  ),
  y = c(C4_test_lowcut_ratio, C4_test_cs_ref_ratio)
)

smo(80:70) * (1/cosd(80:70) / max(1/cosd(80:70)))


cat("C4 factor:", C4_cs_ref_ratio,   "\n")
cat("C4 offset:", C4_GLB_diff_THRES, "\n")


## ____ Create global irradiance W/m^2  ----------------------------------------
DATA[, paste0(csmodel,".glo") := (get(paste0(csmodel,".edir")) + get(paste0(csmodel,".edn"))) / 1000 ]


## ____ Apply sun-earth distance correction  -----------------------------------
DATA[, paste0(csmodel,".glo") := get(paste0(csmodel,".glo")) / sun_dist^2 ]


## ____ Apply adjustment to Kurudz spectrum  -----------------------------------
DATA[, paste0(csmodel,".glo") := get(paste0(csmodel,".glo")) * TSI_Kurudz_factor ]


## ____ Calculate clearness index  ---------------------------------------------
DATA[, ClearnessIndex_C_4 := wattGLB / get(paste0(csmodel,".glo")) ]

hist(DATA$ClearnessIndex_C_4, breaks = 100)
abline(v = C4_cs_ref_ratio, col = "red" )


## ____ Calculate reference and mark data  -------------------------------------

#; ## for most of the data
#; DATA[SZA < C4_lowcut_sza, Enhanc_C_4_ref := (get(paste0(csmodel,".glo")) * C4_cs_ref_ratio) + C4_GLB_diff_THRES ]
#;
#; ## for low sun angles
#; # DATA[SZA > C4_lowcut_sza, Enhanc_C_4_ref := (get(paste0(csmodel,".glo")) * C4_lowcut_ratio) ]
#; DATA[SZA > C4_lowcut_sza, Enhanc_C_4_ref := (get(paste0(csmodel,".glo")) * smo(SZA)) ]


## for most of the data
DATA[SZA < C4_lowcut_sza,
     Enhanc_C_4_ref :=
       (1 + trans_trend(decimal_date(Date))) * (get(paste0(csmodel,".glo")) * C4_cs_ref_ratio) + C4_GLB_diff_THRES ]

# DATA[SZA < C4_lowcut_sza,
#       (1 + trans_trend(decimal_date(Date))) * (get(paste0(csmodel,".glo")) * C4_cs_ref_ratio) + C4_GLB_diff_THRES ]

DATA[SZA < C4_lowcut_sza,
     Enhanc_C_4_ref_test :=                                    (get(paste0(csmodel,".glo")) * C4_test_cs_ref_ratio) ]

## for low sun angles
# DATA[SZA > C4_lowcut_sza, Enhanc_C_4_ref := (get(paste0(csmodel,".glo")) * C4_lowcut_ratio) ]
DATA[SZA > C4_lowcut_sza,
     Enhanc_C_4_ref := (1 + trans_trend(decimal_date(Date))) * (get(paste0(csmodel,".glo")) * smo(SZA)) ]

DATA[SZA > C4_lowcut_sza,
     Enhanc_C_4_ref_test :=                                    (get(paste0(csmodel,".glo")) * smo_test(SZA)) ]



DATA[wattGLB > Enhanc_C_4_ref ,
     Enhanc_C_4 := TRUE]
## use threshold to compute values
if (SelEnhanc == "Enhanc_C_4") {
  DATA[ , GLB_diff :=   wattGLB - Enhanc_C_4_ref                    ] ## enhancement
  DATA[ , GLB_ench := ( wattGLB - Enhanc_C_4_ref ) / Enhanc_C_4_ref ] ## relative enhancement
  DATA[ , GLB_rati :=   wattGLB / Enhanc_C_4_ref                    ]
}

cat("C4_lowcut_sza",      C4_lowcut_sza    , "\n")
cat("C4_cs_ref_ratio",    C4_cs_ref_ratio  , "\n")
cat("C4_GLB_diff_THRES",  C4_GLB_diff_THRES, "\n")


#+ echo=F, include=T

# pyear <- c(1994, 2010, 2022 )
# pyear <- c(2018 )
# p <-
#     ggplot(DATA[year(Date) %in% pyear],
#            aes(get(paste0(SelEnhanc,"_ref")), wattGLB)) +
#     geom_point(data   = DATA[year(Date) %in% pyear & get(SelEnhanc) == FALSE,],
#                colour = "black",
#                na.rm  = TRUE,
#                size   = 0.2) +
#     geom_point(data   = DATA[year(Date) %in% pyear & get(SelEnhanc) == TRUE,],
#                na.rm  = TRUE,
#                size   = 0.2,
#                aes(color = GLB_diff)) +
#     scale_colour_gradient(low      = "blue",
#                           high     = "red",
#                           na.value = NA) +
#     xlab(paste0(SelEnhanc, "_ref")) +
#     labs(color = "Over\nreference") +
#     theme(
#         legend.position      = c(.03, .97),
#         legend.justification = c("left", "top"),
#         legend.box.just      = "right",
#         legend.margin        = margin(6, 6, 6, 6)
#     ) +
#     scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
#     scale_y_continuous(breaks = scales::breaks_extended(n = 6),
#                        expand = expansion(mult = c(0.03, 0.03)))
# print(p)





#+ include=TRUE, echo=FALSE

#' \FloatBarrier
#'
#' # Using creteria: `r SelEnhanc`
#'
#' # Distribution of different metrics
#'
#+ echo=FALSE, include=TRUE


# hist(DATA[GLB_ench > 0, GLB_ench])
# hist(DATA[GLB_diff > 0, GLB_diff])
# hist(DATA[GLB_rati > 1, GLB_rati])

hist(DATA[, GLB_ench], breaks = 100,
     main = varname("GLB_ench"))
abline(v = 0, col = "red")

hist(DATA[, GLB_diff], breaks = 100,
     main = varname("GLB_diff"))
abline(v = 0, col = "red")

hist(DATA[, GLB_rati], breaks = 100,
     main = varname("GLB_rati"))
abline(v = 1, col = "red")



## Mol2023
## activate when +1% and 10w/m from model reference
## near by values with +0.1 are also accepted


# pander(table(DATA$Enhanc_C_1),
#        caption = "Enhanc_C_1")
#
# pander(table(DATA$Enhanc_C_2),
#        caption = "Enhanc_C_2")
#
# pander(table(DATA$Enhanc_C_3),
#        caption = "Enhanc_C_3")

pander(table(DATA$Enhanc_C_4),
       caption = "Enhanc_C_4")



##  Test for low elevation angles  ---------------------------------------------

# DATA[get(SelEnhanc) == TRUE , min(GLB_diff) , by = SZA %/% 1]

testsza <- DATA[GLB_diff > 0,
                .(
                    min    = min   (GLB_diff, na.rm = T),
                    max    = max   (GLB_diff, na.rm = T),
                    mean   = mean  (GLB_diff, na.rm = T),
                    median = median(GLB_diff, na.rm = T)

                ) , by = SZA %/% 1]


plot(testsza[ , median, SZA ] )
plot(testsza[ , min,    SZA ] )
plot(testsza[ , max,    SZA ] )
plot(testsza[ , mean,   SZA ] )


for (aa in 77:60) {
    hist(DATA[(SZA %/% 1) == aa & GLB_diff > -300, GLB_diff],
         breaks = 30,
         main = aa)
}



##  Estimate enhancement daily magnitude  --------------------------------------
enh_days <- DATA[get(SelEnhanc) == TRUE,
                 .(Enh_sum      = sum(GLB_ench, na.rm = TRUE),
                   Enh_max      = max(GLB_ench, na.rm = TRUE),
                   Enh_diff_sum = sum(GLB_diff, na.rm = TRUE),
                   Enh_diff_max = max(GLB_diff, na.rm = TRUE),
                   Enh_N        = sum(get(SelEnhanc))),
                 Day]

hist(enh_days$Enh_sum,      breaks = 100)
hist(enh_days$Enh_max,      breaks = 100)
hist(enh_days$Enh_diff_max, breaks = 100)
hist(enh_days$Enh_diff_sum, breaks = 100)


sunny_days <- DATA[, .(Sunshine = sum(TYPE == "Clear") / max(DayLength, na.rm = TRUE),
                       Energy   = sum(ClearnessIndex_kt, na.rm = TRUE)/sum(TYPE == "Clear"),
                       EC       = sum(get(SelEnhanc)),
                       Cloud    = sum(TYPE == "Cloud")),
                   by = Day]

hist(sunny_days$Sunshine, breaks = 100)
hist(sunny_days$Energy,   breaks = 100)
hist(sunny_days$EC,       breaks = 100)
hist(sunny_days$Cloud,    breaks = 100)



## days with maximum values
setorder(enh_days, -Enh_diff_max)
maxenhd <- enh_days[1:20]

## strong total enhancement days
setorder(enh_days, -Enh_sum)
daylist <- enh_days[1:200]
daylist <- daylist[!Day %in% maxenhd$Day]
enhsnd  <- daylist[sample(1:nrow(daylist), 30)]

## select some sunny days
sunnyd  <- sunny_days[Sunshine > 0.79 & Energy > 0.74]
sunnyd  <- sunnyd[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day]
sunnyd  <- sunnyd[sample(1:nrow(sunnyd), 20)]

## sunny with enhancements
sunnyenh <- sunny_days[Sunshine > 0.77 & Energy > 0.73 & EC > 0]
sunnyenh <- sunnyenh[!Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]

## cloudy days
clouds <- sunny_days[Sunshine > 0.6 & Energy > 0.6 & EC > 2 & Cloud > 5]
clouds <- clouds[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day]
clouds <- clouds[sample(1:nrow(clouds), 20)]

## some random days
all_days <- data.table(Day = unique(DATA[, Day]))
all_days <- all_days[!Day %in% sunnyenh$Day & !Day %in% maxenhd$Day & !Day %in% enhsnd$Day & !Day %in% sunnyd$Day & !Day %in% clouds]
all_days <- all_days[sample(1:nrow(all_days), 30)]

## manual selection
testdays <- data.table(Day = c(
    "2000-07-14",
    "2007-07-06",
    "2013-05-27",
    "2016-08-29"
))


## __  Days with strong enhancement cases  -------------------------------------

#' \FloatBarrier
#'
#' # Plot some days with strong enhancement cases
#'
#+ example-days, echo=F, include=T, results="asis"

vec_days <- matrix(
    ##   Data      Description
    c("maxenhd",  "extreme cases day",
      "enhsnd",   "strong enhancement day",
      "sunnyd",   "sunny day",
      "sunnyenh", "sunny enhancement day",
      "clouds",   "cloudy day",
      "all_days", "random day",
      "testdays", "manual test days",
      NULL),
    byrow = TRUE,
    ncol  = 2)


## Format to data frame
vec_days <- data.frame(Data        = vec_days[,1],
                       Descriprion = vec_days[,2])

for (ii in 1:nrow(vec_days)) {
    cat("\n\\FloatBarrier\n\n")
    cat("\n## Days with", vec_days$Descriprion[ii], "\n\n")
    temp    <- get(vec_days$Data[ii])
    daylist <- sort(temp$Day)

    ##test
    # daylist <- "2019-07-11"

    for (aday in daylist) {
        temp <- DATA[Day == aday]
        par(mar = c(4, 4, 1, 1))
        # ylim <- range(0, temp$ETH, temp$wattGLB, na.rm = TRUE)
        ylim <- range(0, temp$ETH, temp$wattGLB, solar_constant, na.rm = TRUE)

        plot(temp$Date, temp$wattGLB, col = "green",
             pch  = ".", cex = 2,
             ylim = ylim,
             ylab = bquote("GHI" ~ group("[", W/m^2,"]")),
             xlab = "Time (UTC)")

        abline(h = solar_constant, col = "orange2", lty = 1, lwd = 2)

        ## Global
        lines(temp$Date, temp$wattGLB, col = "green")

        ## Direct
        # lines(temp$Date, temp$wattHOR, col = "blue")

        ## TSI on ground
        lines(temp$Date, temp$ETH)

        ## Active model reference
        lines(temp[, get(paste0(SelEnhanc, "_ref")), Date], col = "red" )

        ## Cloud-free ref
        lines(temp[, get(paste0(csmodel,".glo")), Date], col = "darkorchid" )

        # ## add sza axis
        # aaa <- temp[Date %in% c(min(Date), (pretty(Date, 10) + 30), max(Date))  , ]
        # axis(1, at = aaa$Date, labels = round(aaa$SZA,1),
        #      line = 1.2, lwd = 0, lwd.ticks = 0, cex.axis = 0.8)

        ## Enchantment cases
        points(temp[get(SelEnhanc) == TRUE & wattGLB <  ETH, wattGLB, Date], col = "burlywood4")
        points(temp[get(SelEnhanc) == TRUE & wattGLB >= ETH, wattGLB, Date], col = "red")


        ## Cloud cases
        points(temp[TYPE == "Cloud", wattGLB, Date], col = "blue", pch = 3, cex = 0.3)

        ## Decorations
        # title(main = paste(as.Date(aday, origin = "1970-01-01"), temp[get(SelEnhanc) == TRUE, .N], temp[TYPE == "Cloud", .N], vec_days$Descriprion[ii]))
        title(main = paste(as.Date(aday, origin = "1970-01-01")))

        legend("bottomright", ncol = 2,
                     c(  "GHI","CE threshold","TSI at TOA on horizontal plane","Solar Constant", "CE events","ECE events","Identified clouds",  "Cloud-free"),
               col = c("green",         "red",                  "black",       "orange2","burlywood4",       "red",             "blue","darkorchid"),
               pch = c(     NA,            NA,                       NA,              NA,          1 ,          1 ,                  3,           NA),
               lty = c(      1,             1,                        1,               1,          NA,          NA,                 NA,            1),
               lwd = c(      1,             1,                        1,               2,          NA,          NA,                 NA,            1),
               bty = "n",
               cex = 0.8
        )

        ## ggplot

        # cols <- brewer.pal(n = 9, name = 'Set1')
        #  display.brewer.pal(n = 9, name = 'Set1')

        # temp[TYPE == "Clouds", ]
        #
        # mark <- temp[get(SelEnhanc) == TRUE, wattGLB, Date]
        #
        # p <- ggplot(temp, aes(x = Date)) +
        #     geom_point(aes(y = wattGLB,                       color = "wattGLB" ),size = .3 ) +
        #     geom_point(data = mark ,
        #                aes(x = Date, y = wattGLB ),           color = "red", shape = 1 )  +
        #     geom_line( aes(y = CS_low ,                       color = "CS_low"  ))     +
        #     # geom_line( aes(y = CS_2_low,                      color = "CS_2_low"))    +
        #     geom_line( aes(y = CS_exact,                      color = "CS_exact")) +
        #     geom_line( aes(y = get(paste0(SelEnhanc,"_ref")), color = "ref_main")) +
        #     geom_line( aes(y = ETH,                           color = "TSI")) +
        #     labs( title = as.Date(aday)) +
        #     scale_color_manual(
        #         values = c(
        #               cols[3],
        #               "red",
        #               cols[4],
        #               # cols[5],
        #               cols[6],
        #               cols[1],
        #               cols[9]
        #             ),
        #         breaks = c(
        #             "wattGLB" ,
        #             "wattGLB" ,
        #             'CS_low'  ,
        #             # "CS_2_low",
        #             "CS_exact",
        #             "ref_main",
        #             "TSI"
        #         ),
        #         labels = c(
        #             'GLB',
        #             "Enhancement",
        #             'CS -1σ',
        #             # "CS -2σ",
        #             "CS",
        #             "Current ref",
        #             "TSI"),
        #         guide = guide_legend(override.aes = list(
        #             linetype = c(NA, rep( 1, 4)),
        #             shape    = c( 1, rep(NA, 4))))
        #         ) +
        #     theme_bw() +
        #     theme(
        #           # legend.position = c(0.1, .9),
        #           legend.position = "right",
        #           legend.title          = element_blank(),
        #           legend.background     = element_rect(fill = 'transparent'),
        #           legend.box.background = element_rect(fill = 'transparent', color = NA))
        # print(p)
        # plotly::ggplotly(p)


        # overplot clearnesindex
        # par(new = T)
        # plot(temp$Date, temp$ClearnessIndex_kt, "l")
        # abline(h = C1_Clearness_Kt_THRES)

        # plot(temp$Date, temp$Clearness_Kt)
        # abline(h=.8,col="red")
        # plot(temp$Date, temp$DiffuseFraction_Kd)
        # plot(temp$Date, temp$GLB_ench)
        # plot(temp$Date, temp$GLB_diff)
        cat(' \n \n')
    }
}
#+ echo=F, include=T




##  Yearly plots  --------------------------------------------------------------

#' \newpage
#' \FloatBarrier
#' # Plot years with enhancement cases
#'
#+ P-example-years, echo=F, include=T, results="asis"

## TODO plot only enhancement cases
## DO it with base plot
##
yearstodo <- unique(year(DATA$Date))

if (TEST) {
    yearstodo <- sample(yearstodo, 3)
}


# yearstodo <- 2005
for (pyear in yearstodo) {
  p <-
    ggplot(DATA[year(Date) == pyear],
           # aes(get(paste0(SelEnhanc, "_ref")), wattGLB)) + ## threshold
           aes(get(paste0(csmodel,".glo")), wattGLB)) +    ## Cloud-free
    geom_point(data   = DATA[year(Date) == pyear & get(SelEnhanc) == FALSE,],
               colour = "black",
               alpha  = .1,
               na.rm  = TRUE,
               size   = 0.2) +
    geom_point(data   = DATA[year(Date) == pyear & get(SelEnhanc) == TRUE,],
               na.rm  = TRUE,
               size   = 0.2,
               aes(color = GLB_diff)) +
    geom_abline(aes(intercept = 0, slope = 1), colour = "green") +
    # geom_abline(aes(intercept = C4_GLB_diff_THRES, slope = C4_cs_ref_ratio), colour = "green") +
    scale_colour_gradient(low      = "blue",
                          high     = "red",
                          limits   = c(0, NA),  ## always display zero
                          na.value = NA) +
    # labs(title = pyear) +
    ylab(bquote("Measured GHI" ~ group("[", W/m^2,"]"))) +
    xlab(bquote("GHI reference" ~ group("[", W/m^2,"]"))) +
    labs(color = bquote("OI" ~ group("[", W/m^2,"]"))) +
    theme(
      legend.title         = element_text(size = 10),
      legend.position      = c(.03, .97),
      legend.justification = c("left", "top"),
      legend.box.just      = "right",
      legend.key           = element_blank(),
      legend.background    = element_rect(fill = "transparent"),
      legend.margin        = margin(6, 6, 6, 6) ) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                       expand = expansion(mult = c(0.03, 0.03)))

  print(p)
}





if (TEST == TRUE) {
    warning("  TEST IS ACTIVE  !! ")
    cat("\n  TEST IS ACTIVE  !! \n\n")
}

if (TEST == FALSE) {

##  Group continuous values  ---------------------------------------------------


## Init groups logical
DATA[, C1G1 := get(SelEnhanc)]
DATA[, C1G0 := get(SelEnhanc)]

## __ No gap group  ------------------------------------------------------------
DATA[, C1Grp0 := rleid(c(NA,diff(cumsum(C1G0))))]
DATA[C1G0 == FALSE, C1Grp0 := NA]

# ## __ Allow one gap group  -----------------------------------------------------
# DATA[shift(C1G1, n = +1)[[1L]] == TRUE &
#      shift(C1G1, n = -1)[[1L]] == TRUE &
#      C1G1 == FALSE,
#      C1G1 := TRUE]
# DATA[, C1Grp1 := rleid(c(NA,diff(cumsum(C1G1))))]
# DATA[C1G1 == FALSE, C1Grp1 := NA]

## For bigger gaps should use a similar method with the one gap
## for the pattern TTFFTT -> TTTTTT
## and may need these
DATA[, C1G0 := NULL]
DATA[, C1G1 := NULL]


## Slow implementation
# DATA[, cnF := cumsum(Enhanc_C_1 == FALSE)]
# DATA[, cnT := cumsum(Enhanc_C_1 == TRUE) ]
# ## Init groups logical
# DATA[, C1G1  := Enhanc_C_1]
# DATA[, C1G0  := Enhanc_C_1]
# ## Find groups with one gap
# for (i in 1:nrow(DATA)) {
#     p1 <- i - 1
#     n1 <- i + 1
#     if (p1 > 0 & n1 <= nrow(DATA)) {
#         if (DATA$C1G1[p1] == TRUE  &
#             DATA$C1G1[i]  == FALSE &
#             DATA$C1G1[n1] == TRUE  ) {
#             DATA$C1G1[i]  <- TRUE
#         }
#     }
# }
# ## Allow one gap group
# DATA[, C1Grp1 := rleid(c(NA,diff(cumsum(G1))))]
# DATA[C1G1 == FALSE, C1Grp1 := NA]

#  Save processed data  --------------------------------------------------------
# saveRDS(DATA, file = Input_data_ID, compress = "xz")
# cat("\n  Saved raw input data:", Input_data_ID, "\n\n")

#  Save variables from environment  --------------------------------------------
objects <- grep("^tic$|^tac$|^Script.Name$|^tag$", ls(), value = T, invert = T)
objects <- objects[sapply(objects, function(x)
    is.numeric(get(x)) |
        is.character(get(x)) &
        object.size(get(x)) < 1009 &
        (!is.vector(get(x)) |
             !is.function(get(x))), simplify = T)]
## Data
objects <- c(
    objects, "DATA"
)


save(file = paste0("./data/", basename(sub("\\.R", ".Rda", Script.Name))),
     list = objects,
     compress = "xz")
}





#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
