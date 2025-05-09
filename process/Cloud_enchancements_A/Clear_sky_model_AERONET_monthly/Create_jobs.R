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
Script.Name <- "~/MANUSCRIPTS/02_enhancement/Libradtran/Create_jobs.R"
dir.create("./runtime/", showWarnings = FALSE)
d <- filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
Sys.setenv(TZ = "UTC")
## standard output
if (!interactive()) {
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
model_cs     <- "~/LibRadTranG/Clear_sky_model_AERONET_monthly/Model_CS_2.Rds"



## Read Climatology from AERONET  ----------------------------------------------
AER <- fread("Thessaloniki_overall.txt", skip = 6, fill = TRUE, na.strings = "-100")

## remove NA
AER <- remove_empty(AER, which = "cols")
AER <- data.frame(AER)
AER[AER == -100] <- NA
AER <- remove_empty(AER, which = "cols")

AER <- clean_names(AER)
AER <- data.table(AER)

## create month index
AER$month <- match(tolower(AER$month), tolower(month.abb))

## keep only month data
AER <- AER[!is.na(month), ]

## Change units
AER$pw_avg_mm <- AER$pw_avg_cm * 10
AER$pw_std_mm <- AER$pw_std_cm * 10
AER$pw_avg_cm <- NULL
AER$pw_std_cm <- NULL

## Create a and b for Angstrom exponent  ---------------------------------------
#'
#' $α = \ln(τ_1/τ_2)/\ln(λ_2/λ_1)$
#'
#' $β = τ_1 λ_1 ^α = τ_2 λ_2 ^α$
#'

## Use angstrom exponent
AER$alpha500    <- AER$ae_440nm_870nm_avg

## Choose AOD values
AER$tau500      <- AER$aod_500nm_avg

## Choose an best clear case AOD value
AER$tau500_cs   <- AER$aod_500nm_avg - (1 * AER$aod_500_std)
AER$tau500_cs2  <- AER$aod_500nm_avg - (2 * AER$aod_500_std)
AER$tau500_cs3  <- AER$aod_500nm_avg + (1 * AER$aod_500_std)

## Calculate beta
AER$beta500     <- AER$tau500     * ( 500 / 1000 )^AER$alpha500
AER$beta500_cs  <- AER$tau500_cs  * ( 500 / 1000 )^AER$alpha500
AER$beta500_cs2 <- AER$tau500_cs2 * ( 500 / 1000 )^AER$alpha500
AER$beta500_cs3 <- AER$tau500_cs3 * ( 500 / 1000 )^AER$alpha500


## Create water column vars  ---------------------------------------------------
AER$pw_avg_mm_cs  <- AER$pw_avg_mm - (1 * AER$pw_std_mm)
AER$pw_avg_mm_cs2 <- AER$pw_avg_mm - (2 * AER$pw_std_mm)
AER$pw_avg_mm_cs3 <- AER$pw_avg_mm + (1 * AER$pw_std_mm)



## Protect new variables from negative
AER[beta500_cs3   < 0, beta500_cs3   := 0]
AER[beta500_cs2   < 0, beta500_cs2   := 0]
AER[beta500_cs    < 0, beta500_cs    := 0]

AER[pw_avg_mm_cs  < 0, pw_avg_mm_cs  := 0]
AER[pw_avg_mm_cs2 < 0, pw_avg_mm_cs2 := 0]
AER[pw_avg_mm_cs3 < 0, pw_avg_mm_cs3 := 0]


PA <- data.table(
    AODS = c(
        "beta500"   ,
        "beta500_cs"
        # "beta500_cs2",
        # "beta500_cs3"
    ),
    namesA = c(
        "Exact B",
        "Low B"
        # "Low 2 B",
        # "High B"
    ))

PB <- data.table(
    WATR = c(
        "pw_avg_mm"    ,
        "pw_avg_mm_cs"
        # "pw_avg_mm_cs2",
        # "pw_avg_mm_cs3"
    ),
    namesB = c(
        "Exact W",
        "Low W"
        # "Low 2 W"
        # "High W"
    ))

per <- expand.grid(1:nrow(PA), 1:nrow(PB))

cc <- data.table(m = "month", a = "alpha500", PA[per$Var1] ,PB[per$Var2])
cc[, type := paste0( namesA, ".", namesB)]
cc$namesA <- NULL
cc$namesB <- NULL


## Create table of a, b and water combinations  --------------------------------

COMB <- data.table()
for (ii in 1:nrow(cc)) {
    ll <- cc[ii,]
    ## select combination
    temp <-  AER[, .(month     = get(ll$m),
                     pw_avg_mm = get(ll$WATR),
                     a         = alpha500,
                     b         = get(ll$AODS),
                     type      = ll$type) ]
    COMB <- rbind(COMB, temp)
}

# COMB <- rbind(
#     AER[, .(month, pw_avg_mm, a = alpha500, b = beta500    , type = "Exact B")],
#     AER[, .(month, pw_avg_mm, a = alpha500, b = beta500_cs , type = "Low B"  )],
#     AER[, .(month, pw_avg_mm, a = alpha500, b = beta500_cs2, type = "Low 2 B")],
#     AER[, .(month, pw_avg_mm, a = alpha500, b = beta500_cs3, type = "High B" )]
# )


## Create all other iterations  ------------------------------------------------
atmosphere_file   <- c("afglms", "afglmw")
source_solar      <- "kurudz_0.1nm"
SZA               <- unique(seq(15, 80, 0.2))  ## Thessaloniki sun gets up to SZA ~ 17.1

## just test
# SZA <- SZA[(length(SZA)-3):length(SZA)]

BASE <- expand.grid(
    atmosphere_file        = atmosphere_file,
    source_solar           = source_solar,
    albedo                 = 0.2,
    pressure               = 1013,
    sza                    = SZA,
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
ALLRUNS <- data.table(dplyr::cross_join(BASE, COMB))

table(ALLRUNS$type)

## Create hash id
# ALLRUNS$ID <- apply(ALLRUNS[, !c("type") ], 1, function(x) digest::digest(x, "md5"))
ALLRUNS$ID <- apply(ALLRUNS, 1, function(x) digest::digest(x, "md5"))

## Create a list of remaining runs to export  ----------------------------------
if (file.exists(model_cs)) {
    storage <- readRDS(model_cs)
    TODO    <- ALLRUNS[ ! ID %in% storage$ID]

    cat("STORAGE\n")

    table(storage$sza)

    table(storage$month)

    table(storage$type)


} else {
    TODO    <- ALLRUNS
}



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
