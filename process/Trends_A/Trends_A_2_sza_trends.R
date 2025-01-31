# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH]
#' abstract:
#'   "Study of GHI and DNI radiation for 'clear sky' and all sly conditions."
#'
#' documentclass:  article
#' classoption:    a4paper,oneside
#' fontsize:       10pt
#' geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations: yes
#' colorlinks:     yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        yes
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         yes
#'     fig_width:       7
#'     fig_height:      4.5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


## __ Document options ---------------------------------------------------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png"))
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

#+ include=F, echo=F
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./DHI_GHI_2_sza_trends.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
library(lubridate,  quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2,    quietly = TRUE, warn.conflicts = FALSE)
library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)


panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")


## __ Source initial scripts ---------------------------------------------------
source("./DHI_GHI_0_variables.R")

## check previous steps
if (! file.exists(I2_szatrend) |
    file.mtime(I2_szatrend) < file.mtime("./DHI_GHI_0_variables.R") |
    file.mtime(I2_szatrend) < file.mtime("./DHI_GHI_00_raw_data.R") |
    file.mtime(I2_szatrend) < file.mtime("./DHI_GHI_02_Input_szatrends.R") )
{
    # source("./DHI_GHI_02_Input_szatrends.R")
    tryCatch(source("./DHI_GHI_02_Input_szatrends.R"), exit = function(cond) {
        message( conditionMessage(cond) )
    })
    dummy <- gc()
}

## load data
load(I2_szatrend)

tic <- Sys.time()

## notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system("notify-send -u normal -t 30000 'R session' 'An error occurred!'")
    }
    traceback()
})

## __ Flags --------------------------------------------------------------------

DRAFT <- TRUE
DRAFT <- FALSE

## override plot options
par(pch = ".")

## choose to grid some plots
FIGURESGRID <- TRUE
FIGURESGRID <- FALSE

## choose loess criterion for span
LOESS_CRITERIO <-  c("aicc", "gcv")[1]

## cex value for side by side
ccex_sbs <- 1.3

#+ echo=F, include=T
#'
#' ## 2. Long term by SZA
#'
#' ### Data info
#'
#' Time data span `r range(ALL_2_daily_DESEAS$Date)`
#'
#' ### Process
#'
#+ echo=F, include=F


# ## ____ Scatter plots with SZA all data ----------------------------------------
# data_list <- c(  "ALL_2_daily_DESEAS",
#                "CLEAR_2_daily_DESEAS",
#                "CLEAR_2_daily_DESEAS")
# ## x variables
# by_var    <- c("doy","SZA")
# for (i in data_list) {
#     ## get data and y vars to plot
#     Dplot  <- get(i)
#     wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
#     ## loop existing x vars
#     for (xvar in names(Dplot)[names(Dplot) %in% by_var]) {
#         for (yvar in wecare) {
#             col <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
#                               collapse = "_"))
#             vect <- Dplot[[yvar]]
#             plot(Dplot[[xvar]], vect,
#                  pch  = 19,
#                  cex  = .3,
#                  col  = col,
#                  main = paste(i, yvar),
#                  xlab = xvar, ylab = yvar)
#         }
#     }
# }
#
# ## ____ Histograms Plots all data ----------------------------------------------
# for (i in data_list) {
#     ## get data and y vars to plot
#     Dplot  <- get(i)
#     wecare <- grep("HOR|GLB|DIR", names(Dplot), value = TRUE)
#     for (yvar in wecare) {
#         if (!yvar %in% names(Dplot)) next()
#         col <- get(paste0(c("col", unlist(strsplit(yvar,split = "_" ))[1:2]),
#                           collapse = "_"))
#         hist(Dplot[[yvar]],
#              main   = paste(i, yvar),
#              xlab   = yvar,
#              breaks = 100, col = col)
#     }
# }
# #+ echo=F, include=F
# rm(data_list)






##  Daily SZA trends for all year  ---------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Daily SZA trends
#'
#+ echo=F, include=F

test <- ALL_2_daily_DESEAS[preNoon == TRUE & SZA > 70 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)



for (asz in unique(test$SZA)) {
    if (all(is.na(test[SZA == asz, GLB_att_des]))) next()

    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    hist(test[SZA == asz, GLB_att_des], breaks = 100,
         main = asz)
    abline(v = 170, col = "red")
}
test[GLB_att_des > 180, .N , by = Date]

for (asz in unique(test$SZA)) {
    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    if (all(is.na(test[SZA == asz, GLB_att]))) next()
    hist(test[SZA == asz, GLB_att], breaks = 100,
         main = asz)
    abline(v = 300, col = "red")
}
test[GLB_att_des > 180, .N , by = Date]






test <- CLOUD_2_daily_DESEAS[preNoon == TRUE & SZA > 70 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)

for (asz in unique(test$SZA)) {
    if (all(is.na(test[SZA == asz, GLB_att]))) next()

    # plot(test[SZA == asz, GLB_att, Date],
    #      main = asz)
    # abline(lm(test[SZA == asz, Date, GLB_att]), col = "red")

    plot(test[SZA == asz, GLB_att_des, Date],
         main = asz)
    abline(lm(test[SZA == asz, Date, GLB_att_des]), col = "red")


    # hist(test[SZA == asz, GLB_att_des], breaks = 100,
    #      main = asz)
    # abline(v = 170, col = "red")
}
test[GLB_att_des > 170, .N , by = Date]

for (asz in unique(test$SZA)) {
    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    if (all(is.na(test[SZA == asz, GLB_att]))) next()
    hist(test[SZA == asz, GLB_att], breaks = 100,
         main = asz)
    abline(v = 300, col = "red")
}
test[GLB_att_des > 170, .N , by = Date]



test[SZA == 78 & GLB_att_des > 200, .N , by = Date ]



test <- ALL_2_daily_DESEAS[ SZA > 75 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)


count <- test[, sum(GLB_att_N) , by = .(SZA, preNoon, Date )]

tc <- count[, (diff(V1)), by = .(SZA, Date) ]

hist(tc$SZA)
plot(tc[SZA==76 , V1, Date])
plot(tc[SZA==77 , V1, Date])
# plot(tc[SZA==78 , V1, Date])
# plot(tc[SZA==79 , V1, Date])

tc[SZA==78 & V1 < -2]



##  EXCLUDE PROBLEMATIC DATA  --------------------------------------------------

dbs  <- c("ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS",
          ## monthly
          "ALL_2_monthly_DESEAS",
          "CLEAR_2_monthly_DESEAS",
          "CLOUD_2_monthly_DESEAS",
          ## seasonal daily
          "ALL_2_bySeason_daily_DESEAS",
          "CLEAR_2_bySeason_daily_DESEAS",
          "CLOUD_2_bySeason_daily_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)
    # DB[ !(SZA > 77 & preNoon == TRUE) ]
    assign(DBn, DB[ !(SZA > 76 & preNoon == TRUE) ])
}

dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                   "CLEAR_2_bySeason_daily_DESEAS",
                   "CLOUD_2_bySeason_daily_DESEAS")



## __ Calculate trend SZA ~ Day ------------------------------------------------

vars <- c("GLB_att_des", "GLB_att")

dbs  <- c(  "ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS")

gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {

                dataset <- DB[ SZA == asza & preNoon == anoon ]

                if (sum(!is.na(dataset[[avar]])) <= 1) next()

                lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                gather <- rbind(gather,
                                data.frame(
                                    linear_fit_stats(lm1),
                                    preNoon = anoon,
                                    SZA     = asza,
                                    DATA    = DBn,
                                    var     = avar,
                                    N       = sum(!is.na(dataset[[avar]]))
                                ))
            }
        }
    }
}
#+ echo=F, include=F
gather    <- data.table(gather)
szatrends <- data.table(gather)
setorder(szatrends, SZA)




# ## Use the actual SDR for trends !!
# vars <- c("GLB_att")
# dbs  <- c(  "ALL_2_daily_mean",
#           "CLEAR_2_daily_mean",
#           "CLOUD_2_daily_mean")
# gather <- data.frame()
# for (DBn in dbs) {
#     DB <- get(DBn)
#     for (avar in vars) {
#         for (anoon in unique( DB$preNoon)) {
#             for (asza in unique( DB$SZA )) {
#                 dataset <- DB[ SZA == asza & preNoon == anoon ]
#                 if (sum(!is.na(dataset[[avar]])) <= 1) next()
#                 lm1 <- lm( dataset[[avar]] ~ as.numeric(dataset$Date) )
#                 # lm1 <- lm( as.numeric(dataset$Date) ~ dataset[[avar]] )
#                 plot(dataset$Date, dataset[[avar]])
#                 abline(lm1, col = 2)
#                 gather <- rbind(gather,
#                                 data.frame(
#                                     linear_fit_stats(lm1),
#                                     preNoon   = anoon,
#                                     SZA       = asza,
#                                     DATA      = DBn,
#                                     var       = avar,
#                                     N         = sum(!is.na(dataset[[avar]]))
#                                 )
#                 )
#             }
#         }
#     }
# }
# gather    <- data.table(gather)
# szatrends <- data.table(gather)
# setorder(szatrends, SZA)
# for (adata in unique(gather$DATA)) {
#     subd <- gather[DATA == adata]
#     plot(subd$SZA, subd$slope)
# }
# for (adata in unique(gather$DATA)) {
#     subd <- gather[DATA == adata]
#     plot(subd$SZA, 100 * subd$slope / Days_of_year)
# }
# gather2 <- data.table()
# for (asza in unique(ALL_2_daily_mean$SZA)) {
#     for (pday in unique(ALL_2_daily_mean$preNoon)) {
#         subdata <- ALL_2_daily_mean[SZA == asza & preNoon == pday]
#         if (sum(!is.na(subdata$GLB_att)) <= 1) next()
#         llm1    <- lm(GLB_att ~ Date, data = subdata)
#         # llm1    <- lm(GLB_att ~ Date, data = subdata)
#         ll1     <- coefficients(llm1)
#         plot(subdata$Date, subdata$GLB_att)
#         abline(llm1, col = 2)
#         title(paste("Daily mean", asza, pday))
#         gather2 <- rbind(gather2,
#                          data.frame(t(ll1),
#                                     SZA = asza,
#         )
#     }
# }
# plot(gather2$SZA, gather2$Date)
# gather3 <- data.table()
# for (asza in unique(ALL_2_monthly_mean$SZA)) {
#     for (pday in unique(ALL_2_monthly_mean$preNoon)) {
#         subdata <- ALL_2_monthly_mean[SZA == asza & preNoon == pday]
#         llm1    <- lm(GLB_att ~ Date, data = subdata)
#         ll1     <- coefficients(llm1)
#         plot(subdata$Date, subdata$GLB_att)
#         abline(llm1, col = 2)
#         title(paste("Monthly mean", asza, pday))
#         gather3 <- rbind(gather3,
#                          data.frame(t(ll1),
#                                     SZA = asza,
#                                     preNoon = pday)
#         )
#     }
# }
# plot(gather3$SZA, gather3$Date)
# gather4 <- data.table()
# for (asza in unique(ALL_2_yearly_mean$SZA)) {
#     for (pday in unique(ALL_2_yearly_mean$preNoon)) {
#         subdata <- ALL_2_yearly_mean[SZA == asza & preNoon == pday]
#         llm1    <- lm(GLB_att ~ Year, data = subdata)
#         ll1     <- coefficients(llm1)
#         plot(subdata$Year, subdata$GLB_att)
#         abline(llm1, col = 2)
#         title(paste("Yearly mean", asza, pday))
#         gather4 <- rbind(gather4,
#                          data.frame(t(ll1),
#                                     SZA = asza,
#                                     preNoon = pday)
#         )
#     }
# }
# plot(gather4$SZA, gather4$Year)
# for (aDATA in unique( szatrends$DATA )) {
#         pp <- szatrends[DATA == aDATA ]
#         plot(pp$SZA, pp$slope * Days_of_year)
#         title(paste(aDATA, mean(100 * pp$slope / Days_of_year, na.rm = T), median(100 * pp$slope / Days_of_year, na.rm = T) ))
#
# }



##__ Covert to trend per year --------------------------------------------------
szatrends[, slope    := slope    * Days_of_year ]
szatrends[, slope.sd := slope.sd * Days_of_year ]

## For raw values
# szatrends[, slope    := 100 * slope    / Days_of_year ]
# szatrends[, slope.sd := 100 * slope.sd / Days_of_year ]


## set some plot option for data
# szatrends[var == "DIR_att",    col := col_DIR_att    ]
# szatrends[var == "GLB_att",    col := col_GLB_att    ]
# szatrends[var == "DIR_transp", col := col_DIR_transp ]
szatrends[preNoon == T, pch := pch_am ]
szatrends[preNoon == F, pch := pch_pm ]

# hist(szatrends[DATA == dbs[1], N], breaks = 100)
# hist(szatrends[DATA == dbs[2], N], breaks = 100)

# hist(szatrends[var == vars[1], N], breaks = 100)
# hist(szatrends[var == vars[2], N], breaks = 100)

# szatrends <- szatrends[ N > 50]

# plot(szatrends$SZA,szatrends$N)

# test1 <- szatrends[DATA == "CLEAR_2_daily_DESEAS" & var == "DIR_att"]
# test2 <- szatrends[DATA == "CLEAR_2_daily_DESEAS" & var == "GLB_att"]
# plot(test1$SZA, test1$N, pch = 19)
# abline(h=50)
# plot(test2$SZA, test2$N, pch = 19)
# abline(h=300)




szatrends[DATA == "ALL_2_daily_DESEAS" & preNoon == TRUE & slope > 0.6]




## __ Plot trend ~ SZA stats from daily ----------------------------------------

## stats vars to plot
wecare <- grep("^slope|^N",  names(szatrends), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)


warning("this should be _des  !!")
vars <- c("GLB_att_des")
# vars <- c("GLB_att")


#+ SzaTrends, echo=F, include=T, results = "asis"
for (avar in vars) {
    ## ALL - CS
    lec <- 0
    for (type in unique(szatrends$DATA)) {
        lec <- lec + 1

        cat("\n\\newpage\n\n")
        cat("\n#### ", translate(type), translate(avar) , "from daily\n\n")

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(ceiling(length(wecare)/2), 2))
        }

        par("mar" = c(4, 5, 2, 2))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## Replace variable name
            if (awename == "Slope") { awename <- "Trend [%/y]" }

            ## limit plot p-values
            p_lim     <- 0.05

            szatrends <- data.table(szatrends)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends[DATA == type &
                                 var  == avar, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]

            ## plot only under accepted p-value limit
            # subdata <- subdata[ slope.p < p_lim, ]

            xlim <- range(subdata$SZA,    na.rm = T)
            ylim <- range(subdata[[awe]], na.rm = T)

            pam  <- subdata[preNoon == TRUE ]
            ppm  <- subdata[preNoon == FALSE]

            ccex <- ccex_sbs
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            if (DRAFT == TRUE) {
                par("mar" = c(4,   5,   2,   2))
            } else {
                par("mar" = c(4, 4.5, 0.5, 0.5))
            }

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = "",
                 xlim = xlim,
                 ylim = ylim,
                 yaxt = "n")

            ## y axis
            axis(2, pretty(ylim), las = 2)

            ## x axis
            axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                      tcl = -0.25)
            title(xlab = bquote("Solar zenith angle (SZA)"),
                  line = 2.5)

            title(ylab = awename,
                  line = 3.3)

            ## zero line
            abline(h = 0, lty = 3)

            if (DRAFT == TRUE) {
                # title(paste(translate(avar), awename, "for", translate(type)), cex.main =  .8 * ccex)
                title(paste(translate(avar), "trend", "for", translate(type)), cex.main =  .8 * ccex)
            } else {
                # translate(type)
                legend("bottomright", 0, paste0("(", letters[lec], ")"),
                       bty   = "n",
                       # cex   = .8 * ccex,
                       cex   = 1.7,
                       xjust = 0.5,      # 0.5 means center adjusted
                       yjust = 0.5,      # 0.5 means center adjusted
                       x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                       y.intersp =  0.2, # adjust character interspacing to effect box height
                       adj = c(0, 0.5))  # adjust string position (default values used here)
                # text.font = 2)  # bold the text if you like (not used here)
                par("mar" = c(4,   5,   2,   2))
            }

            legend("topleft",
                   legend = c("Morning", "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)

            ccex <- 1
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            cat("\n\n")
        }
        par(mfrow = c(1, 1)) ## just reset layout
    }
}



# __ Plot daily climatology --------------------------------------------------
#+ SzaClimaDaily, echo=F, include=T, results = "asis", out.heigth="30%"
vars <- c("GLB_att_seas")

dbs  <- c(  "ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)

    cat("\n\\newpage\n\n")
    cat("\n#### Daily SZA Climatology", translate(type), translate(avar), "\n\n")

    for (avar in vars) {
        for (asza in sort(unique( DB$SZA ))) {

            par("mar" = c(4, 4, 2, 2))

            dataset <- DB[ SZA == asza ]
            ## get only one season
            dataset <- dataset[!duplicated(dataset[ , c("SZA", "doy", "preNoon")]), ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ylim <- range(0, DB[[avar]], na.rm = T)
            xlim <- range(DB$doy)

            pam  <- dataset[preNoon == TRUE ]
            ppm  <- dataset[preNoon == FALSE]

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = avar,
                 xlim = xlim,
                 ylim = ylim)

            ## morning lines
            points(pam$doy, pam[[avar]],
                   col  = 2,
                   pch  = pam$pch,
                   cex  = 1)

            points(ppm$doy, ppm[[avar]],
                   col  = 3,
                   pch  = ppm$pch,
                   cex  = 1)

            title(main = paste("Seasonal ", DBn, asza, sub("_des", "_seas", avar)))

            legend("top",
                   legend = c("Morning", "Evening"),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

        }
    }
}




##  Monthly SZA trends for all year  -------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Monthly SZA trends
#'
#+ echo=F, include=F


## __ Calculate trend SZA ~ Month ----------------------------------------------

vars <- c("GLB_att_des")

dbs  <- c(  "ALL_2_monthly_DESEAS",
          "CLEAR_2_monthly_DESEAS",
          "CLOUD_2_monthly_DESEAS")

gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {

                dataset <- DB[ SZA == asza & preNoon == anoon ]

                if (sum(!is.na(dataset[[avar]])) <= 1) next()

                lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                gather <- rbind(gather,
                                data.frame(
                                    linear_fit_stats(lm1),
                                    preNoon = anoon,
                                    SZA     = asza,
                                    DATA    = DBn,
                                    var     = avar,
                                    N       = sum(!is.na(dataset[[avar]]))
                                ))
            }
        }
    }
}
#+ echo=F, include=F
szatrends_M <- data.table(gather)
setorder(szatrends_M, SZA)

##__ covert to trend per year --------------------------------------------------
szatrends_M[, slope    := slope    * Days_of_year ]
szatrends_M[, slope.sd := slope.sd * Days_of_year ]

## set some plot option for data
szatrends_M[preNoon == T, pch := pch_am ]
szatrends_M[preNoon == F, pch := pch_pm ]






## __ Plot trend ~ SZA stats from monthly --------------------------------------

## stats vars to plot
wecare <- grep("^slope|^N",  names(szatrends_M), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

vars <- c("GLB_att_des")

## TODO separate plots by direct global

#+ SzaTrendsMonthly, echo=F, include=T, results = "asis"
for (avar in vars) {
    ## ALL - CS
    for (type in unique(szatrends_M$DATA)) {

        cat("\n\\newpage\n\n")
        cat("\n#### ", translate(type), translate(avar), "from monthly\n\n")

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(ceiling(length(wecare)/2), 2))
        }

        par("mar" = c(4, 5, 2, 2))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## Replace variable name
            if (awename == "Slope") { awename <- "Trend [%/y]" }

            ## limit plot p-values
            p_lim       <- 0.05

            szatrends_M <- data.table(szatrends_M)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends_M[DATA == type &
                                   var  == avar, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


            ## plot only under accepted p-value limit
            # subdata <- subdata[ slope.p < p_lim, ]

            xlim <- range(subdata$SZA,    na.rm = T)
            ylim <- range(subdata[[awe]], na.rm = T)

            pam  <- subdata[preNoon == TRUE ]
            ppm  <- subdata[preNoon == FALSE]

            ccex <- ccex_sbs
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            if (DRAFT == TRUE) {
                par("mar" = c(4,   5,   2,   2))
            } else {
                par("mar" = c(4, 4.5, 0.5, 0.5))
            }

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = awename,
                 xlim = xlim,
                 ylim = ylim,
                 yaxt = "n")

            ## y axis
            axis(2, pretty(ylim), las = 2)

            ## x axis
            axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                 tcl = -0.25)
            title(xlab = bquote("Solar zenith angle (SZA)"),
                  line = 2.5)

            ## zero line
            abline(h = 0, lty = 3)

            if (DRAFT == TRUE) {
                # title(paste(translate(avar), awename, "for", translate(type)), cex.main =  .8 * ccex)
                title(paste(translate(avar), "trend", "for", translate(type)), cex.main =  .8 * ccex)
            } else {
                legend("bottomleft", 0, translate(type),
                       bty   = "n",
                       cex   = .8 * ccex,
                       xjust = 0.5,      # 0.5 means center adjusted
                       yjust = 0.5,      # 0.5 means center adjusted
                       x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                       y.intersp =  0.2, # adjust character interspacing to effect box height
                       adj = c(0, 0.5))  # adjust string position (default values used here)
                # text.font = 2)  # bold the text if you like (not used here)
                par("mar" = c(4,   5,   2,   2))
            }

            legend("topleft",
                   legend = c("Morning", "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)

            ccex <- 1
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            cat("\n\n")
        }
        par(mfrow = c(1, 1)) ## just reset layout
    }
}


# __ Plot Monthly climatology --------------------------------------------------
#+ SzaClimaMonthly, echo=F, include=T, results = "asis", out.heigth="30%"
vars <- c("GLB_att_seas")

dbs  <- c(  "ALL_2_monthly_DESEAS",
            "CLEAR_2_monthly_DESEAS",
            "CLOUD_2_monthly_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)

    cat("\n\\newpage\n\n")
    cat("\n#### Monthly SZA Climatology", translate(type), translate(avar), "\n\n")

    for (avar in vars) {
        for (asza in sort(unique( DB$SZA ))) {

            par("mar" = c(4, 4, 2, 2))

            dataset <- DB[ SZA == asza ]
            ## get only one season
            dataset <- dataset[!duplicated(dataset[ , c("SZA", "Month", "preNoon")]), ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ylim <- range(0, DB[[avar]], na.rm = T)
            xlim <- range(1:12)

            pam  <- dataset[preNoon == TRUE ]
            ppm  <- dataset[preNoon == FALSE]

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = avar,
                 xlim = xlim,
                 ylim = ylim)

            ## morning lines
            points(pam$Month, pam[[avar]],
                   col  = 2,
                   pch  = pam$pch,
                   cex  = 1)

            points(ppm$Month, ppm[[avar]],
                   col  = 3,
                   pch  = ppm$pch,
                   cex  = 1)

            title(main = paste("Seasonal ", DBn, asza, sub("_des", "_seas", avar)))

            legend("bottom",
                   legend = c("Morning", "Evening"),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

        }
    }
}


## ........................................................................ ----




##  SZA trends for season of year from daily -----------------------------------

#'
#' ## Plot of SZA trends for each season of year from daily
#'
#+ echo=F, include=F


## __ Calculate trends SZA ~ Season from daily  --------------------------------

# vars        <- c("DIR_att_des", "GLB_att_des", "DIR_transp_des")
vars        <- c("GLB_att_des")

dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                 "CLEAR_2_bySeason_daily_DESEAS",
                 "CLOUD_2_bySeason_daily_DESEAS")
seasons     <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm(dataset[[avar]] ~ dataset$Date)

                    gather_seas <- rbind(gather_seas,
                                    data.frame(
                                        linear_fit_stats(lm1),
                                        preNoon   = anoon,
                                        SZA       = asza,
                                        DATA      = DBn,
                                        var       = avar,
                                        Season    = ase,
                                        N         = sum(!is.na(dataset[[avar]]))
                                    ))
                }
            }
        }
    }
}
#+ echo=F, include=F
szatrends_seas <- data.table(gather_seas)
setorder(szatrends_seas, SZA)

hist(gather_seas$N[gather_seas$N > 50], breaks = 100)






##__ Covert to trend per year --------------------------------------------------
szatrends_seas[, slope    := slope    * Days_of_year]
szatrends_seas[, slope.sd := slope.sd * Days_of_year]

# szatrends_seas[, slope    := 100 * slope    / Days_of_year]
# szatrends_seas[, slope.sd := 100 * slope.sd / Days_of_year]


## define plot colors
szatrends_seas[ var == "DIR_att_des", col := col_DIR_att   ]
szatrends_seas[ var == "GLB_att_des", col := col_GLB_att   ]
szatrends_seas[ var == "DIR_transp",  col := col_DIR_transp]
szatrends_seas[ preNoon == T, pch := pch_am ]
szatrends_seas[ preNoon == F, pch := pch_pm ]


# hist(szatrends_seas[DATA == dbs[1],N],  breaks = 100)
# hist(szatrends_seas[DATA == dbs[2],N],  breaks = 100)
# hist(szatrends_seas[DATA == dbs[3],N],  breaks = 100)
# hist(szatrends_seas[var  == vars[1],N], breaks = 100)
# hist(szatrends_seas[var  == vars[2],N], breaks = 100)

# szatrends_seas <- szatrends_seas[ N > 50]

# plot(szatrends_seas$SZA, szatrends_seas$N)

# test <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "DIR_att_des" ]
# plot(test$SZA, test$N, pch = 19)



# test1 <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "DIR_att_des" ]
# test2 <- szatrends_seas[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "GLB_att_des" ]
# plot(test1$SZA, test1$N, pch = 19)
# plot(test2$SZA, test2$N, pch = 19)




## __ Plot SZA ~ Season stats  -------------------------------------------------

## stats vars to plot
wecare <- grep( "^slope|^N", names(szatrends_seas), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

wecare <- "slope"

# FIGURESGRID <- TRUE

#+ SzaTrendsSeas, echo=F, include=T, results = "asis", out.heigth="30%"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, translate(type), translate(avar),"\n\n"))

            ## plot in a grid
            if (FIGURESGRID) {
                par(mfrow = c(ceiling(length(wecare)/2), 2))
            }

            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## limit plot p-values
                p_lim     <- 0.05

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas[ DATA   == type &
                                           var    == avar &
                                           Season == ase    , ]

                ## set symbols for plotting
                subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
                subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
                subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
                subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range(szatrends_seas$SZA, na.rm = T)
                ylim <- range(subdata[[awe]],     na.rm = T)

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T)


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                ccex <- ccex_sbs
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                if (DRAFT == TRUE) {
                    par("mar" = c(4,   5,   2,   2))
                } else {
                    par("mar" = c(4.5, 4.5, 0.5, 0.5))
                }


                plot(1, type = "n",
                     xlab = "",
                     ylab = awename,
                     xlim = xlim,
                     ylim = ylim,
                     yaxt = "n")

                ## y axis
                axis(2, pretty(ylim), las = 2)

                ## x axis
                axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                     tcl = -0.25)
                title(xlab = bquote("Solar zenith angle (SZA)"),
                      line = 2.5)

                ## zero line
                abline(h = 0, lty = 3)

                ## test for some plots
                if (grepl("CLEAR", type, ignore.case = T)) typeP <- "Clear Sky"
                if (grepl("ALL",   type, ignore.case = T)) typeP <- "All Sky"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 0.8,)

                ## morning lines
                lines(pam$SZA, pam[[awe]],
                      col  = 2,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## morning points
                points(pam$SZA, pam[[awe]],
                       pch = pam$pch,
                       col = 2,
                       cex = 0.8)

                ## evening lines
                lines(ppm$SZA, ppm[[awe]],
                      col  = 3,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## evening points
                points(ppm$SZA, ppm[[awe]],
                       pch = ppm$pch,
                       col = 3,
                       cex = 0.8)

                # legend("top",
                #        legend = c("Morning", "Evening"),
                #        # col    = c(unique(pam$col), unique(ppm$col)),
                #        col    = c( 2,  3),
                #        pch    = c(16, 17), ncol = 2, bty = "n",
                #        cex    = ccex)

                ## reset fonts
                ccex <- 1
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(16, 17), ncol = 2, bty = "n")
            }
            par(mfrow = c(1, 1)) ## just reset layout
        }
    }
}



## __ by season daily in tight grid --------------------------------------------


#'
#' ### Grid of SZA trends for each season of year from daily
#'
#+ echo=F, include=F

#+ SzaTrendsSeasTogether, echo=F, include=T
{
    vars        <- c("GLB_att_des") ## original
    # vars        <- c("GLB_att")
    avar        <- vars[1]
    dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                     "CLEAR_2_bySeason_daily_DESEAS",
                     "CLOUD_2_bySeason_daily_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
    lec         <- 0

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1,  1,1,1,1, 0.5 )
    )
    layout.show(nf)

    # 1
    par("mar" = c(0, 0, 0, 0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            lec <- lec + 1
            par("mar" = c(0,0,0,0))

            kk       <- expanded[ 1, ]
            expanded <- expanded[-1, ]

            ## limit plot p-values
            p_lim    <- 0.05
            N_lim    <- 85


            ## select All/CS  DIR/GLB/trans winter/summer
            subdata <- szatrends_seas[ DATA   == kk$Dataset &
                                       var    == avar &
                                       Season == kk$Seasons, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ N       <= N_lim & preNoon == TRUE,  pch :=  1 ]

            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]
            subdata[ N       <= N_lim & preNoon == FALSE, pch :=  2 ]


            # xlim <- range( subdata$SZA,        na.rm = T )
            ## use same axis for all
            xlim <- range(szatrends_seas$SZA, na.rm = T)

            ## test always show zero on plots
            ylim <- range(0, szatrends_seas$slope, na.rm = T)
            # ylim <- c(-2.5, 4.5)

            # ylim <- range(szatrends_seas[slope.p < p_lim & SZA < 75 , slope] , na.rm = T)
            ylim <- range(szatrends_seas[slope.p < p_lim & N > N_lim , slope], na.rm = T)


            pam  <- subdata[ preNoon == T ]
            ppm  <- subdata[ preNoon == F ]

            ## empty plot
            par("mar" = c(0, 0, 0.5, 0.5))
            plot(1, type = "n",
                 cex      = .6,
                 cex.axis = 0.8,
                 cex.lab  = 0.8,
                 cex.main = 0.9,
                 mgp      = c(2, 0.5, 0),
                 xaxt     = "n",
                 xlab     = "",
                 xlim     = xlim,
                 yaxt     = "n",
                 ylab     = "",
                 ylim     = ylim,
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2, cex.axis = 0.8,              tck = -0.06   )
                axis(2, seq(-3, 3, by = 0.1),  cex.axis = 0.8, labels = NA, tck = -0.03   )
            } else {
                # axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim),          cex.axis = 0.8, labels = NA, tck = -0.03   )
                axis(2, seq(-3, 3, by = 0.1),  cex.axis = 0.8, labels = NA, tck = -0.03/2 )

            }

            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA, tcl = -0.25)
            } else {
                ## major ticks
                # axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck = -0.04)
                ## minor ticks
                axis(1, seq(5, 90, 1), cex.axis = 0.8, labels = NA, tcl = -0.3/2)
                # axis(1, at = seq(5, 90, 1), labels = NA,
                #      tcl = +0.25/2)
            }

            ## zero line
            abline(h = 0, lty = 3)

            ccex <- .8

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = 1,
                  cex  = ccex)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch    = pam$pch,
                   lwd    = .5,
                   col    = 2,
                   cex    = ccex)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = 1,
                  cex  = ccex)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch    = ppm$pch,
                   lwd    = .5,
                   col    = 3,
                   cex    = ccex)

            ## tag plots with letters
            legend("bottomright", 0, paste0("(",letters[lec],")"),
                   cex   = 1.1,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)


            ## legend on any of the plots
            if (i %in% c(7)) {
                # legend("top",
                #        legend = c("Morning low stat. sig.", "Evening low stat. sig.",
                #                   "Morning",                "Evening"),
                #        col    = c(2, 3),
                #        pt.cex = 1,
                #        cex    = 0.8,
                #        pch    = c(1, 2, 16, 17),
                #        ncol   = 2,
                #        bty = "n")
                legend("topleft",
                       legend = c("Morning",                "Evening",
                                  "Morning low stat. sig.", "Evening low stat. sig."),
                       col    = c(2, 3),
                       pt.cex = 1,
                       cex    = 0.8,
                       pch    = c(16, 17, 1, 2),
                       ncol   = 1,
                       bty = "n")
            }

            ## x labels
            if (i %in% c(22, 23, 24)) {
                mtext(text = bquote("Solar zenith angle (SZA)"),
                      cex  = 0.6,
                      side = 1,
                      line = 1.3)
                }


            ## y labels
            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Trend [%/y]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }
            ## set all to zero margin
            par("mar" = c(0,0,0,0))
        }
    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1, 1))
}
#+ echo=F, include=F





##  SZA trends for season of year from monthly -----------------------------------

#'
#' ## Plot of SZA trends for each season of year from monthly
#'
#+ echo=F, include=F


## __ Calculate SZA ~ Season stats  --------------------------------------------

vars          <- c("GLB_att_des")

dbs           <- c(  "ALL_2_bySeason_monthly_DESEAS",
                 "CLEAR_2_bySeason_monthly_DESEAS",
                 "CLOUD_2_bySeason_monthly_DESEAS")
seasons       <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas_M <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique(DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1)       next()
                    if (sum(dataset[[avar]], na.rm = TRUE) == 0) next()

                    lm1 <- lm(dataset[[avar]] ~ dataset$Date)

                    gather_seas_M <- rbind(gather_seas_M,
                                         data.frame(
                                             linear_fit_stats(lm1),
                                             preNoon   = anoon,
                                             SZA       = asza,
                                             DATA      = DBn,
                                             var       = avar,
                                             Season    = ase,
                                             N         = sum(!is.na(dataset[[avar]]))
                                         ))
                }
            }
        }
    }
}
#+ echo=F, include=F

szatrends_seas_M <- data.table(gather_seas_M)
setorder(szatrends_seas_M, SZA)


## covert to trend per year
szatrends_seas_M[, slope    := slope    * Days_of_year]
szatrends_seas_M[, slope.sd := slope.sd * Days_of_year]

# szatrends_seas[, slope    := 100 * slope    / Days_of_year]
# szatrends_seas[, slope.sd := 100 * slope.sd / Days_of_year]


## define plot colors
szatrends_seas_M[ var == "DIR_att_des", col := col_DIR_att   ]
szatrends_seas_M[ var == "GLB_att_des", col := col_GLB_att   ]
szatrends_seas_M[ var == "DIR_transp",  col := col_DIR_transp]
szatrends_seas_M[ preNoon == T, pch := pch_am ]
szatrends_seas_M[ preNoon == F, pch := pch_pm ]


# hist(szatrends_seas_M[DATA == dbs[1],N],  breaks = 100)
# hist(szatrends_seas_M[DATA == dbs[2],N],  breaks = 100)
# hist(szatrends_seas_M[DATA == dbs[3],N],  breaks = 100)
# hist(szatrends_seas_M[var  == vars[1],N], breaks = 100)
# hist(szatrends_seas_M[var  == vars[2],N], breaks = 100)

# szatrends_seas_M <- szatrends_seas[ N > 50]

# plot(szatrends_sea_Ms$SZA, szatrends_seas$N)

# test <- szatrends_seas_M[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == "DIR_att_des"
# plot(test$SZA, test$N, pch = 19)



# test1 <- szatrends_seas_M[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == R_att_des" ]
# test2 <- szatrends_seas_M[ DATA == "CLEAR_2_bySeason_daily_DESEAS" & var == B_att_des" ]
# plot(test1$SZA, test1$N, pch = 19)
# plot(test2$SZA, test2$N, pch = 19)




## __ Plot SZA ~ Season stats  -------------------------------------------------

## stats vars to plot
wecare <- grep( "^slope|^N", names(szatrends_seas_M), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

wecare <- "slope"

# FIGURESGRID <- TRUE

#+ SzaTrendsSeasMonthly, echo=F, include=T, results = "asis"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas_M$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas_M$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, translate(type), translate(avar),"\n\n"))

            ## plot in a grid
            if (FIGURESGRID) {
                par(mfrow = c(ceiling(length(wecare)/2), 2))
            }


            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## limit plot p-values
                p_lim     <- 0.05

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas_M[ DATA   == type &
                                                 var    == avar &
                                                 Season == ase    , ]

                ## set symbols for plotting
                subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
                subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
                subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
                subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range(szatrends_seas$SZA, na.rm = T)
                ylim <- range(subdata[[awe]],     na.rm = T)

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T)


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                ccex <- ccex_sbs
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                if (DRAFT == TRUE) {
                    par("mar" = c(4,   5,   2,   2))
                } else {
                    par("mar" = c(4.5, 4.5, 0.5, 0.5))
                }


                plot(1, type = "n",
                     xlab = "",
                     ylab = awename,
                     xlim = xlim,
                     ylim = ylim,
                     yaxt = "n")

                ## y axis
                axis(2, pretty(ylim), las = 2)

                ## x axis
                axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                     tcl = -0.25)
                title(xlab = bquote("Solar zenith angle (SZA)"),
                      line = 2.5)

                ## zero line
                abline(h = 0, lty = 3)

                ## test for some plots
                if (grepl("CLEAR", type, ignore.case = T)) typeP <- "Clear Sky"
                if (grepl("ALL",   type, ignore.case = T)) typeP <- "All Sky"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 0.8,)

                ## morning lines
                lines(pam$SZA, pam[[awe]],
                      col  = 2,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## morning points
                points(pam$SZA, pam[[awe]],
                       pch = pam$pch,
                       col = 2,
                       cex = 0.8)

                ## evening lines
                lines(ppm$SZA, ppm[[awe]],
                      col  = 3,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## evening points
                points(ppm$SZA, ppm[[awe]],
                       pch = ppm$pch,
                       col = 3,
                       cex = 0.8)

                # legend("top",
                #        legend = c("Morning", "Evening"),
                #        # col    = c(unique(pam$col), unique(ppm$col)),
                #        col    = c( 2,  3),
                #        pch    = c(16, 17), ncol = 2, bty = "n",
                #        cex    = ccex)

                ## reset fonts
                ccex <- 1
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(16, 17), ncol = 2, bty = "n")
            }
            par(mfrow = c(1, 1)) ## just reset layout
        }
    }
}



## __ by season in a tight grid ----------------------------------------------
#+ SzaTrendsSeasTogetherMonthly, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]
    dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                       "CLEAR_2_bySeason_daily_DESEAS",
                       "CLOUD_2_bySeason_daily_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.3,   1,1,1, 0.1),
        heights = c(0.2, 1,1,1,1, 0.5)
    )
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            par("mar"=c(0,0,0,0))

            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            ## limit plot p-values
            p_lim     <- 0.05

            ## select All/CS  DIR/GLB/trans winter/summer
            subdata <- szatrends_seas[ DATA   == kk$Dataset &
                                           var    == avar &
                                           Season == kk$Seasons, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


            # xlim <- range( subdata$SZA,        na.rm = T )
            ## use same axis for all
            xlim <- range(szatrends_seas$SZA, na.rm = T)

            ## test always show zero on plots
            ylim <- range(0, szatrends_seas$slope, na.rm = T)
            # ylim <- c(-2.5, 4.5)

            ylim <- range(szatrends_seas[slope.p < p_lim & SZA < 75, slope] , na.rm = T)


            pam  <- subdata[ preNoon == T ]
            ppm  <- subdata[ preNoon == F ]

            ## plot
            par("mar" = c(0, 0, 0.3, 0.3))


            plot(1, type = "n",
                 cex      = .6,
                 cex.axis = 0.8,
                 cex.lab  = 0.8,
                 cex.main = 0.9,
                 mgp      = c(2, 0.5, 0),
                 xaxt     = "n",
                 xlab     = "",
                 xlim     = xlim,
                 yaxt     = "n",
                 ylab     = "",
                 ylim     = ylim,
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2, cex.axis = 0.8)
            } else {
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck = -0.03)
            }


            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = -0.25)
            } else {
                ## major ticks
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck = -0.03)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = -0.25/2)
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = +0.25/2)
            }

            ## zero line
            abline(h = 0, lty = 3)


            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)


            if (i %in% c(7)) {
                # legend("top",
                #        legend = c("Morning low stat. sig.", "Evening low stat. sig.",
                #                   "Morning",                "Evening"),
                #        col    = c(2, 3),
                #        pt.cex = 1,
                #        cex    = 0.8,
                #        pch    = c(1, 2, 16, 17),
                #        ncol   = 2,
                #        bty = "n")
                legend("topleft",
                       legend = c("Morning",                "Evening",
                                  "Morning low stat. sig.", "Evening low stat. sig."),
                       col    = c(2, 3),
                       pt.cex = 1,
                       cex    = 0.8,
                       pch    = c(16, 17, 1, 2),
                       ncol   = 1,
                       bty = "n")

            }


            if (i %in% c(22, 23, 24)) {
                mtext(text = bquote("Solar zenith angle (SZA)"),
                      cex  = 0.6,
                      side = 1,
                      line = 1.3)
            }


            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Trend [%/y]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }


            par("mar" = c(0,0,0,0))
        }

    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
