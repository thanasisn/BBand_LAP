#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Project performance monitoring"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.1in,right=0.1in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#'
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "pdf"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!h"    )

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/99_Self_evaluation.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(gdata,      warn.conflicts = FALSE, quietly = TRUE)


##  Evaluate data sizes  -------------------------------------------------------

## _ Gather data size ----------------------------------------------------------
#'
#' # Data size overview
#'
#' \footnotesize
#'
#+ echo=F, include=T

overview_data <- "~/BBand_LAP/SIDE_DATA/Data_size.Rds"

## Broad band parquet data base
BB <- opendata()
gather <- data.frame(
    Name = "BBDB",
    Rows = BB |> nrow(),
    Vars = BB |> ncol(),
    Valu = BB |> summarise(across(everything(), ~ sum(!is.na(.)))) |>
        collect() |> rowwise() |> sum(),
    Size = strsplit(
        system(
            paste("du -s", DB_DIR),
            intern = TRUE),
        "\t")[[1]][1]
)
rm(BB)

## Broad band parquet data base meta data
BB <- open_dataset(DB_META_fl)
gather <- rbind(gather,
                data.frame(
                    Name = "BBDB meta",
                    Rows = BB |> nrow(),
                    Vars = BB |> ncol(),
                    Valu = BB |> summarise(across(everything(), ~ sum(!is.na(.)))) |>
                        collect() |> rowwise() |> sum(),
                    Size = strsplit(
                        system(
                            paste("du -s", DB_META_fl),
                            intern = TRUE),
                        "\t")[[1]][1]
                )
)
rm(BB)



## Tracker parquet data base
BB <- open_dataset(DB_Steps_DIR)
gather <- rbind(gather,
                data.frame(
                    Name = "TrackerDB",
                    Rows = BB |> nrow(),
                    Vars = BB |> ncol(),
                    Valu = BB |> summarise(across(everything(), ~ sum(!is.na(.)))) |>
                        collect() |> rowwise() |> sum(),
                    Size = strsplit(
                        system(
                            paste("du -s", DB_Steps_DIR),
                            intern = TRUE),
                        "\t")[[1]][1]
                )
)
rm(BB)

## Tracker parquet data base meta data
BB <- open_dataset(DB_Steps_META_fl)
gather <- rbind(gather,
                data.frame(
                    Name = "TrackerDB meta",
                    Rows = BB |> nrow(),
                    Vars = BB |> ncol(),
                    Valu = BB |> summarise(across(everything(), ~ sum(!is.na(.)))) |>
                        collect() |> rowwise() |> sum(),
                    Size = strsplit(
                        system(
                            paste("du -s", DB_Steps_META_fl),
                            intern = TRUE),
                        "\t")[[1]][1]
                )
)
rm(BB)

## Broad Band data hash file storage
BB <- open_dataset(DB_HASH_fl)
gather <- rbind(gather,
                data.frame(
                    Name = "Raw files hashes",
                    Rows = BB |> nrow(),
                    Vars = BB |> ncol(),
                    Valu = BB |> summarise(across(everything(), ~ sum(!is.na(.)))) |>
                        collect() |> rowwise() |> sum(),
                    Size = strsplit(
                        system(
                            paste("du -s", DB_HASH_fl),
                            intern = TRUE),
                        "\t")[[1]][1]
                )
)
rm(BB)

gather <- data.table(gather)
gather[, Fill := round(100 * as.double(Valu) / (as.double(Rows) * as.double(Vars)),2 ) ]


gather$Size <- as.numeric(gather$Size) * 1024
gather <- rbind(gather,
                data.frame(
                    Name = "**Total**",
                           Rows = sum(gather$Rows),
                           Vars = sum(gather$Vars),
                           Valu = sum(gather$Valu),
                           Size = sum(gather$Size),
                           Fill = NA
                    )
)
gather <- data.table(gather)
gather[, "Bytes/Value" := round(Size / as.double(Valu), 2)]



## Gather results
gather[, Date := Sys.time()]
if (!file.exists(overview_data)) {
    saveRDS(gather, overview_data)
} else {
    DATA <- readRDS(overview_data)
    DATA <- unique(rbind(DATA, gather, fill = T))
    saveRDS(DATA, overview_data)
}


pp      <- gather
pp      <- rename(pp, Values = "Valu")
pp$Size <- humanReadable(pp$Size)
pp$Date <- NULL
pp$Fill <- paste0(pp$Fill, "%" )


##  Export table for Readme.md
panderOptions('knitr.auto.asis', FALSE)
temp <- pander_return(
    pp,
    justify = "lrrrrrr",
    style   = "rmarkdown",
    caption = paste("Datasets sizes on", Sys.Date())
)
capture.output(cat(temp, sep = "\n"), file = "~/BBand_LAP/.databasestats.md")


##  Table for rendering document
#+ echo=F, include=T, results="asis"
pander(pp, justify = "lrrrrrr")
cat(" \n \n")




## _ Plot data size ------------------------------------------------------------

#'
#' ## Data size plots
#'
#' \footnotesize
#'
#+ echo=F, include=T, results = "asis"
vars <- grep("Name|Date", names(DATA), value = TRUE, invert = TRUE)

for (av in vars) {

    types <- unique(DATA$Name)
    ylim  <- range(DATA[, .(get(av))], na.rm = T)
    xlim  <- range(DATA[, Date], na.rm = T)

    par("mar" = c(2, 5, 4, 0.1))

    plot(1,
         xlab = "",
         ylab = av,
         ylim = ylim,
         xlim = xlim,
         las  = 1,
         xaxt = "n",
         yaxt = "n")
    axis.POSIXct(1, pretty(DATA[, Date]))

    if (av == "Size") {
        axis(2,
             at     = pretty(DATA[[av]]),
             labels = humanReadable(pretty(DATA[[av]])),
             las    = 2)
    } else if (av == "Rows") {
        axis(2,
             at     = pretty(DATA[[av]]),
             labels = paste(pretty(DATA[[av]])/1000000, "M"),
             las    = 2)
    } else {
        axis(2, pretty(DATA[[av]]), las = 2)
    }

    cc <- 1
    for (at in types) {
        pp <- DATA[Name == at , .(get(av), Date)]
        cc <- cc + 1
        lines(pp$Date, pp$V1, col = cc)
    }

    par(xpd = TRUE)
    legend("topleft",
           inset  = c(-.2, -.17),
           legend = types,
           bty    = "n",
           lty    = 1,
           col    = 2:(length(types) + 2),
           ncol   = 3)
    par(xpd = FALSE)
    cat(" \n \n")
}



##  Evaluate execution times  --------------------------------------------------

## _ Parse data  ---------------------------------------------------------------
DATA <- fread("~/BBand_LAP/REPORTS/LOGs/Run.log",
              fill = TRUE,
              blank.lines.skip = TRUE)
## Create datetime
DATA$Date <- as.POSIXct(strptime(DATA[, paste(V1, V2)], "%F %H:%M:%OS"))
DATA[, V1 := NULL]
DATA[, V2 := NULL]
## Check units
stopifnot(DATA[, all(V6 == "mins")])
DATA[, V6 := NULL]
## Check execution time
if (is.numeric(DATA$V5)) {
  names(DATA)[names(DATA) == "V5"] <- "Minutes"
}
## Parse script name

## get base dir
DATA[, Script   := basename(V4)]
DATA[, Category := basename(dirname(V4))]
DATA[, Script_2 := paste0(Category, "::", Script)]
DATA[, V4       := NULL]

## Parse host
DATA[, c("User", "Host") := tstrsplit(V3, "@")]
DATA[, V3 := NULL]

## Show only stats for the main machine
DATA <- DATA[Host == "sagan"]

## Limit date range
DATA <- DATA[Date > Sys.time() - (370 * 24 * 3600)]



xlim <- range(DATA$Date)

## _ Last executions time  -----------------------------------------------------
#' \newpage
#' # Execution times overview
#'
#' ## Last run
#'
#' \footnotesize
#'
#+ echo=F, include=T, results="asis"
# last <- DATA[DATA[, .I[which.max(Date)], by = .(Script, Category)]$V1]
last <- DATA[DATA[, .I[which.max(Date)], by = .(Script_2)]$V1]
last <- last[, .(Script_2, Category, Date, Minutes)]
last$Minutes <- round(last$Minutes, 2)
setorder(last, Date)
cat(pander(
  last[, .(Script_2, Date = lubridate::round_date(Date, unit = "min"), Minutes)],
  justify = "llr"),"\n")
cat(" \n \n")




## _ Executions statistics  ----------------------------------------------------
#' \newpage
#' ## Executions times statistics
#'
#' \footnotesize
#'
#+ echo=F, include=T, results="asis"
stats <-
    DATA[, .(
        Median = round(median(Minutes), 2),
        Min    = round(min(Minutes)   , 2),
        Max    = round(max(Minutes)   , 2),
        # Mean   = round(mean(Minutes)  , 2),
        .N
    ),
    by = Script_2]
stats <- stats[order(match(Script_2, last$Script_2))]
pander(stats, justify = "lrrrr")
cat(" \n \n")



## _ Total Executions times  ---------------------------------------------------
#' \newpage
#' ## Total Executions
#'
#' \footnotesize
#'
#+ echo=F, include=T
DATA[, G   := 0]
DATA[, GID := 0]
DATA[Script == "Build_DB_01_pysolar.R", G := 1]

c <- 0
for (i in 1:nrow(DATA)) {
    c <- c + DATA$G[i]
    DATA$GID[i] <- c
}

total <- DATA[, .(Minutes = sum(Minutes),
                  Date    = min(Date),
                  .N),
              by = GID]

summary(total$Minutes)
# hist(total$Minutes)
## Ignore unrealistic time spans
total <- total[Minutes < 1440, ]

plot(1,
     xlim = xlim,
     ylim = range(total$Minutes, na.rm = T),
     ylab = "Minutes",
     xlab = "",
     xaxt = "n",
     main = "Total execution")
points(total[N == median(N), Minutes, Date],
       col = "green")
points(total[N > median(N),  Minutes, Date],
       col = "blue")
points(total[N < median(N),  Minutes, Date],
       col = "red")
axis.POSIXct(1, total$Date)


partial <- DATA[, .(Minutes = sum(Minutes),
                    Date    = min(Date),
                    .N),
                by = .(GID, Category)]

# hist(partial$Minutes)
partial[Minutes > 1440]
partial <- partial[Minutes < 1440]

for (as in unique(partial$Category)) {
    pp <- partial[Category == as]
    plot(1,
         xlim = xlim,
         ylim = range(pp$Minutes, na.rm = TRUE),
         ylab = "Minutes",
         xlab = "",
         xaxt = "n",
         main = paste("Total execution: ", as))
    points(pp[N == median(N), Minutes, Date],
           col = "green")
    points(pp[N > median(N),  Minutes, Date],
           col = "blue")
    points(pp[N < median(N),  Minutes, Date],
           col = "red")
    axis.POSIXct(1, pp$Date)
}



## _ Script time statistics  ---------------------------------------------------
#' \newpage
#' ## Script statistics
#'
#' \footnotesize
#'
#+ echo=F, include=T, results = "asis", out.height = "30%"
for (as in last$Script_2) {
    pp <- DATA[Script_2 == as]
    plot(pp[, Minutes, Date],
         xlim = xlim,
         main = as)
}


## _ Update Readme.md  ---------------------------------------------------------
system("~/BBand_LAP/.update_readme.sh")


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
