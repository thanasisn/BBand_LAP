#!/usr/bin/env Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "......."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' abstract: "........."
#' output:
#'   html_document:
#'     toc: true
#'     fig_width:  9
#'     fig_height: 4
#'   pdf_document:
#' date: "`r format(Sys.time(), '%F')`"
#' ---


#+ echo=F, include=T
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

Script.Name <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Create_jobs_aod_change.R"

dir.create("./runtime/", showWarnings = FALSE)
filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))))

## standard output
if (!interactive()) {
    dir.create("./runtime/", showWarnings = FALSE)
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}
## error notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})
tic <- Sys.time()



## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = c("pdf", "png"))
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

## __ Set environment  ---------------------------------------------------------
require(data.table, quietly = TRUE, warn.conflicts = FALSE)

# library("RlibRadtran")
library("janitor")


## __ Paths  -------------------------------------------------------------------

## run files
repo_dir     <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/io_repo/"

## empty runs
run_list_rds <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/run.Rds"
run_list_fl  <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/run.list"

## all runs
model_cs     <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/Model_CS_trend_yearly.Rds"





##  Load and prepare data  -----------------------------------------------------
AEin1 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.lev20"
AE1   <- fread(AEin1, skip = 6, fill = T, na.strings = "-999")

names(AE1)[names(AE1) == "Month"] <- "Date"

AE1 <- AE1[, lapply(.SD, function(x) replace(x, which(x < -998), NA))]
AE1 <- remove_constant(AE1)
AE1 <- remove_empty(AE1, which = "cols")
AE1 <- clean_names(AE1)

# AEin2 <- "~/DATA/Aeronet/Thessaloniki_Monthly/20030101_20241231_Thessaloniki.tot_lev20"
# AE2 <- fread(AEin2, skip = 6, fill = T, na.strings = "-999")

AE1[, c("Year", "Month") := tstrsplit(date, "-")]
AE1[, Year  := as.numeric(Year)]
AE1[, Month := match(Month, toupper(month.abb))]
AE1[, tsy := Year + (Month - 1) / 12]

## Change units
AE1$pw_mm                 <- AE1$precipitable_water_cm * 10
AE1$precipitable_water_cm <- NULL







## get sza data for each month

pysol <- data.table(readRDS("~/DATA/SUN/Pysolar_LAP.Rds"))

pysol[, Month := month(V1) ]
pysol[, Year  := year(V1)  ]
# only daytime
pysol <- pysol[V3 >= 0, ]
pysol[, SZA := 90 - V3]

matc <- AE1[, Month, Year]

tes <- merge(matc, pysol, all.x = T)
tes <- tes[!is.na(SZA), ]

add <- tes[, .( SZA_mean   = mean(SZA,   na.rm = T),
                SZA_min    = min(SZA,    na.rm = T),
                SZA_median = median(SZA, na.rm = T)),
    by = .(Month, Year)]
write.csv(add, "~/MANUSCRIPTS/02_enhancement/data/sza_aeronet.csv")


addy <- pysol[Year %in% c(1997, 2005),
              .( SZA_mean   = mean(SZA,   na.rm = T),
                 SZA_min    = min(SZA,    na.rm = T),
                 SZA_median = median(SZA, na.rm = T),
                 Month      = NA),
              by = .(Year)]
write.csv(addy, "~/MANUSCRIPTS/02_enhancement/data/sza_input_brewer.csv")


AE1 <- merge(AE1, add)


# hist(tes$V2)
#
# hist(tes$V3)
#
# tttt <- tes[as.Date(tes$V1) == "2003-06-01"]



## Create a and b for Angstrom exponent  ---------------------------------------
#'
#' $α = \ln(τ_1/τ_2)/\ln(λ_2/λ_1)$
#'
#' $β = τ_1 λ_1 ^α = τ_2 λ_2 ^α$
#'

## Use angstrom exponent
AE1$alpha500    <- AE1$x440_870_angstrom_exponent

## Choose AOD values
AE1$tau500      <- AE1$aod_500nm

## Calculate beta
AE1$beta500     <- AE1$tau500     * ( 500 / 1000 )^AE1$alpha500


tt <- AE1[, mean(pw_mm, na.rm = T ), by = Year]


# Kazadzis2007 - Nine years of UV aerosol optical depth measurements at Thessaloniki, Greece
# From Stelios' paper 2007:
# AOD @340 from Brewer 086
# Period: 1997-2005
# The mean AOD for this period was calculated to 0.403 for the Brewer and 0.422 for the CIMEL.
# A linear regression on the Brewer deseasonalized data reveals a change of −3.8±0.93% per year.
# From the above data AOD @340 changes by 0.0153 per year (or 0.138 for the 9 years).
# Using Angstrom a= 1.6  this translates to a change in Angstrom β=0.00272 per year (or β=0.084 in 1997 and β=0.059 in 2005).

0.084 * 340^(-1.6) - 0.059 * 340^(-1.6)

0.403 / (340**1.6)

## mean year 2001

(0.4 * 0.038)                  ## AOD change per year
(d_t <- (0.4 * 0.038) * 9)     ## AOD change 1997 - 2005

(0.4 * 0.038) * (340/1000)^1.6 ## Beta change per year

b_2001 = 0.4 * (340/1000)^1.6

(b_1997 = (0.4 + d_t/2) * (340/1000)^1.6)
(b_2005 = (0.4 - d_t/2) * (340/1000)^1.6)



# Nine years of UV aerosol optical depth measurements at {T}hessaloniki, {G}reece
BR1 <- data.frame(a         = 1.6,
                  # b         = c(0.084, 0.059),
                  b         = c(b_1997, b_2005),
                  # pw_avg_mm = c(tt[Year == 2003, V1], tt[Year == 2005, V1]),
                  pw_avg_mm = c(15.5, 15.5),
                  Year      = c( 1997,  2005),
                  month     = NA,
                  typer     = "Brewer"
)
merge(BR1, addy)


data.table(merge(BR1, add, all = T))[!is.na(a)]

BR1 <- data.table(rbind(
    # merge(BR1, add),
    merge(BR1, addy)
))






## More data
BR2 <- fread("~/MANUSCRIPTS/02_enhancement/data/dailymean_angstrom9707_refined.dat")

BR2 <- BR2[, .(a         = mean(alpha),
               b         = mean(betta) * (340/1000)^1.6,
               pw_avg_mm = 15.5,
               month     = NA,
               typer     = "Yearly Brewer"),  ## from 340
           by = year ]











## Create table of a, b and water combinations  --------------------------------
COMB <- AE1[!is.na(alpha500) & !is.na(beta500),
    .(b         = beta500,
      a         = alpha500,
      pw_avg_mm = pw_mm,
      year      = Year,
      month     = Month,
      typer     = "Month Exact")]


COMB2 <- rbind(
    # AE1[!is.na(alpha500) & !is.na(beta500),
    #     .(b         = beta500,
    #       a         = alpha500,
    #       pw_avg_mm = pw_mm,
    #       year      = Year,
    #       month     = Month,
    #       sza       = SZA_mean,
    #       typer     = "SZA mean")],
    # AE1[!is.na(alpha500) & !is.na(beta500),
    #     .(b         = beta500,
    #       a         = alpha500,
    #       pw_avg_mm = pw_mm,
    #       year      = Year,
    #       month     = Month,
    #       sza       = SZA_median,
    #       typer     = "SZA median")],
    # AE1[!is.na(alpha500) & !is.na(beta500),
    #     .(b         = beta500,
    #       a         = alpha500,
    #       pw_avg_mm = pw_mm,
    #       year      = Year,
    #       month     = Month,
    #       sza       = SZA_min,
    #       typer     = "SZA min")],
    AE1[!is.na(alpha500) & !is.na(beta500),
        .(b         = beta500,
          a         = alpha500,
          pw_avg_mm = pw_mm,
          year      = Year,
          month     = Month,
          sza       = 55,
          typer     = "SZA 55")],
    # BR1[, .(b,
    #         a,
    #         pw_avg_mm,
    #         year  = Year,
    #         month,
    #         sza   = SZA_min,
    #         typer = "BR SZA min") ],
    # BR1[, .(b,
    #         a,
    #         pw_avg_mm,
    #         year  = Year,
    #         month,
    #         sza   = SZA_median,
    #         typer = "BR SZA median") ],
    # BR1[, .(b,
    #         a,
    #         pw_avg_mm,
    #         year  = Year,
    #         month,
    #         sza   = SZA_mean,
    #         typer = "BR SZA mean") ],
    BR1[, .(b,
            a,
            pw_avg_mm,
            year  = Year,
            month,
            sza   = 55,
            typer = "BR SZA 55") ],
    BR2[, .(b,
            a,
            pw_avg_mm,
            year,
            month,
            sza   = 55,
            typer = "Year BR SZA 55") ]

)



## Create all other iterations  ------------------------------------------------
atmosphere_file   <- c("afglms", "afglmw")
source_solar      <- "kurudz_0.1nm"
# SZA               <- unique(c(seq(15, 80, 5), 17))  ## Thessaloniki sun gets up to SZA ~ 17.1


## just test
# SZA <- SZA[(length(SZA)-3):length(SZA)]

BASE <- expand.grid(
    atmosphere_file        = atmosphere_file,
    source_solar           = source_solar,
    albedo                 = 0.2,
    pressure               = 1013,
    # sza                    = SZA,
    mol_modify_O3          = 300,           # DU
    number_of_streams      = 8,
    aerosol_modify_ssa_set = 0.95,
    aerosol_modify_gg_set  = 0.70,
    mol_abs_param          = "LOWTRAN",
    rte_solver             = "disort",
    geometry               = "pseudospherical",
    wavelength_min         = 280,
    wavelength_max         = 2500
)

## Produce all combination to run
ALLRUNS <- data.table(dplyr::cross_join(BASE, COMB2))

table(ALLRUNS$type)

## Create hash id
# ALLRUNS$ID <- apply(ALLRUNS[, !c("type") ], 1, function(x) digest::digest(x, "md5"))
ALLRUNS$ID <- apply(ALLRUNS, 1, function(x) digest::digest(x, "md5"))


## Create a list of remaining runs to export  ----------------------------------
if (file.exists(model_cs)) {
    storage <- readRDS(model_cs)
    TODO    <- ALLRUNS[ ! ID %in% storage$ID]

    # storage <- storage[!is.na(month), ]
    # saveRDS(storage, model_cs)

    cat("STORAGE\n")

    table(storage$sza)

    table(storage$month)

    table(storage$type)

} else {
    TODO    <- ALLRUNS
}

##;; ## remove old
##;; # storage <- storage[!grepl("BR.*55" ,typer)]
##;; # saveRDS(storage, model_cs)


cat("TODO\n")

table(TODO$sza)

table(TODO$type)

table(TODO$type, TODO$month)


# export todo for R
saveRDS(TODO, run_list_rds)

## Export list for worker  -----------------------------------------------------
WORKER <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/LBT_PBS.sh"

cat("", file = run_list_fl)
# for (ri in 1:100) {
for (ri in sample(1:nrow(TODO))) {
    OptVect = TODO[ri,]

    cat(
        sprintf(""),
        sprintf("%s ",                                      WORKER                                         ),
        sprintf("%s ",                                      OptVect$ID                                     ),
        sprintf("atmosphere_file@@aattmmoo=%s.dat@",        OptVect$atmosphere_file                        ),
        sprintf("source@@solar@@ssoollaa=%s.dat@@per_nm@",  OptVect$source_solar                           ), ## per_nm ?
        sprintf("albedo@@%s@",                              OptVect$albedo                                 ),
        sprintf("pressure@@%s@",                            OptVect$pressure                               ),
        sprintf("sza@@%s@",                                 OptVect$sza                                    ),
        sprintf("mol_modify@@O3@@%s@@DU@",                  OptVect$mol_modify_O3                          ),
        sprintf("mol_modify@@H2O@@%s@@MM@",                 OptVect$pw_avg_mm                              ),
        sprintf("aerosol_default@"                                                                         ),
        sprintf("aerosol_angstrom@@%s@@%s@",                OptVect$a, OptVect$b                           ),
        sprintf("aerosol_modify@@ssa@@set@@%s@",            OptVect$aerosol_modify_ssa_set                 ),
        sprintf("aerosol_modify@@gg@@set@@%s@",             OptVect$aerosol_modify_gg_set                  ),
        sprintf("mol_abs_param@@%s@",                       OptVect$mol_abs_param                          ),
        sprintf("rte_solver@@%s@",                          OptVect$rte_solver                             ),
        sprintf("%s@",                                      OptVect$geometry                               ),
        sprintf("number_of_streams@@%s@",                   OptVect$number_of_streams                      ),
        sprintf("wavelength@@%s@@%s@",                      OptVect$wavelength_min, OptVect$wavelength_max ),
        sprintf("output_process@@%s@",                      "integrate"                                    ),
        sprintf("quiet@"),
        sprintf("\n"),
        sep = "",
        file = run_list_fl ,
        append = TRUE
    )
}


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (difftime(tac, tic, units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'FINISHED'"))
}
