# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Check data source files integrity"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Inspect raw data for potential problems."
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
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
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' - Lists Sirena files
#' - Lists Radmon files
#' - Incremental Stores metadata for source files
#'    - mtime
#'    - md5sum
#'    - basename
#'    - parsed date
#' - Checks stored data for inconsistencies
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/00_Check_input_files.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")

##  CHP-1 raw data check  ------------------------------------------------------

#'
#' ## CHP-1 files
#'
#+ echo=F, include=T, results="asis"

## __ Get Sirena files  --------------------------------------------------------
sirena_files <- list.files(path        = SIRENA_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
## just in case, there are nested folders with more lap files in Sirena
sirena_files <- grep("OLD", sirena_files,
                     ignore.case = TRUE, invert = TRUE, value = TRUE )

## __ Get Radmon files  --------------------------------------------------------
radmon_files <- list.files(path        = RADMON_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )

## __  Compare files between Radmon and Sirena  --------------------------------
sir_names <- basename(sirena_files)
rad_names <- basename(radmon_files)

cat("\n**CHP-1:", paste(length(sirena_files), "files from Sirena**\n"))
cat("\n**CHP-1:", paste(length(radmon_files), "files from Radmon**\n"))

missing_from_sir <- rad_names[!rad_names %in% sir_names ]
if (length(missing_from_sir) > 0) {
    # warning("There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena\n")
    cat("\n**There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena**\n\n")
    cat(missing_from_sir, sep = " ")
    cat("\n\n")
} else {
    cat("\nThere aren't any CHP-1 files in Radmon missing from Sirena\n\n")
}
rm(rad_names, radmon_files, sirena_files)

##  CM-21 raw data check  ------------------------------------------------------

#'
#' ## CM-21 files
#'
#+ echo=F, include=T, results="asis"

## __ Get Sirena files  --------------------------------------------------------
sirena_files <- list.files(path        = SIRENA_GLB,
                           recursive   = TRUE,
                           pattern     = "[0-9]*06.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )

## just in case, there are nested folders with more lap files in Sirens
sirena_files <- grep("OLD", sirena_files,
                     ignore.case = TRUE, invert = TRUE, value = TRUE )

## __ Get Radmon files  --------------------------------------------------------
radmon_files <- list.files(path        = RADMON_GLB,
                           recursive   = TRUE,
                           pattern     = "[0-9]*06.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )

## __  Compare files between Radmon and Sirena  --------------------------------
sir_names <- basename(sirena_files)
rad_names <- basename(radmon_files)

cat("\n**CM-21:", paste(length(sirena_files), "files from Sirena**\n"))
cat("\n**CM-21:", paste(length(radmon_files), "files from Radmon**\n"))

missing_from_sir <- rad_names[ ! rad_names %in% sir_names ]
if (length(missing_from_sir) > 0) {
    # warning("There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena\n")
    cat("\n**There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena**\n\n")
    cat(missing_from_sir, sep = " ")
    cat("\n\n")
} else {
    cat("\nThere aren't any CM-21 files in Radmon missing from Sirena\n\n")
}
rm(rad_names, radmon_files, sirena_files)

##  ECO-UVA raw data check  ----------------------------------------------------

#'
#' ## EKO files
#'
#+ echo=F, include=T, results="asis"

## __ Get Sirena files  --------------------------------------------------------
sirena_files <- list.files(path        = SIRENA_EKO,
                           recursive   = TRUE,
                           pattern     = "[0-9]*05.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )

## just in case, there are nested folders with more lap files in Sirens
sirena_files <- grep("OLD", sirena_files,
                     ignore.case = TRUE, invert = TRUE, value = TRUE )

## __ Get Radmon files  --------------------------------------------------------
radmon_files <- list.files(path        = RADMON_GLB,
                           recursive   = TRUE,
                           pattern     = "[0-9]*05.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )

## __  Compare files between Radmon and Sirena  --------------------------------
sir_names <- basename(sirena_files)
rad_names <- basename(radmon_files)

cat("\n**EKO:", paste(length(sirena_files), "files from Sirena**\n"))
cat("\n**EKO:", paste(length(radmon_files), "files from Radmon**\n"))

missing_from_sir <- rad_names[ ! rad_names %in% sir_names ]
if (length(missing_from_sir) > 0) {
    # warning("There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena\n")
    cat("\n**There are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena**\n\n")
    cat(missing_from_sir, sep = " ")
    cat("\n\n")
} else {
    cat("\nThere aren't any EKO files in Radmon missing from Sirena\n\n")
}
rm(rad_names, radmon_files, sirena_files)

##  Checksum test  -------------------------------------------------------------

#'
#' ## Check the `md5` check sum of raw files.
#'
#+ echo=F, include=T, results="asis"

## get a fresh hash table from meta data
parthash <- read_parquet(DB_META_fl) |>
    select(ends_with("_mtime",    ignore.case = TRUE),
           ends_with("_md5sum",   ignore.case = TRUE),
           ends_with("_basename", ignore.case = TRUE),
           ends_with("_parsed",   ignore.case = TRUE),
    )
## remove constructed files
parthash$pysolar_mtime    <- NULL
parthash$pysolar_basename <- NULL
## unify variables
parthash <- melt(data = parthash,
                 measure = patterns("_mtime$",
                                    "_md5sum$",
                                    "_basename$",
                                    "_parsed"),
                 value.name = c("mtime",
                                "md5sum",
                                "basename",
                                "parsed"),
                 na.rm = TRUE)
parthash$variable <- NULL

## __ Update hash table  -------------------------------------------------------
if (!file.exists(DB_HASH_fl)) {
    ## Nothing to compare to, just store the new table
    writePARQUET(x = parthash, sink = DB_HASH_fl)
} else {
    ## Add new hashes to permanent storage

    ## read stored
    mainhash <- read_parquet(DB_HASH_fl)

    ## merge stored with current in DB
    parthash <- unique(rbind(parthash, mainhash))
    ## order to keep most resent afte deduplication
    setorder(parthash, md5sum, -parsed )
    ## keep unique combination of md5sum and basenames
    mainhash <- mainhash[!duplicated(mainhash[ , md5sum, basename]), ]

    writePARQUET(x = parthash, sink = DB_HASH_fl)
}

## __ Check duplicate hashes  --------------------------------------------------
dups <- mainhash[duplicated(mainhash$md5sum)]
if (nrow(dups) > 0) {
    cat("\n**There are ", nrow(dups), " files with the same checksum**\n\n")
    setorder(dups, md5sum, basename)
    # \scriptsize
    # \footnotesize
    # \small
    cat("\n \\footnotesize \n\n")
    panderOptions('table.split.table', Inf)
    cat(pander(dups,
           caption = "Files with the same md5sum"))
    cat("\n \n \\normalsize \n \n")
    # cat("\n\n \\ \n\n")
} else {
    cat("\n**All checksum are unique**\n")
}

## __ Check files with different hashes  ---------------------------------------
tabs <- mainhash[, .N, by = basename]
tabs <- tabs[N > 1, ]
if (nrow(tabs) > 0) {
    cat("\n**There are ", nrow(tabs), " files with the same filename and different hash**\n\n")

    cat("\n \\footnotesize \n\n")
    panderOptions('table.split.table', Inf)
    cat(pander(tabs,
           caption = "Files with the same filename and different hash!!"))
    cat("\n \n \\normalsize \n \n")
} else {
    cat("**There are no files with the same filename and different md5 hash**\n\n")
}


## TODO
## - list snc
## - list therm
## - list step
## - more instruments



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

