#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Test the compresion of the BB data base

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tools/Test_compression.R"
renv::load("~/BBand_LAP")

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)


for (algo in c("gzip", "brotli", "zstd", "lz4", "lzo", "bz2")) {
    if (codec_is_available(algo)) {
        cat("AVAILABLE:", algo, "\n")
    } else {
        cat("NOT available:", algo, "\n")
    }
}


results <- "~/BBand_LAP/SIDE_DATA/DB_compression_test.Rds"
nodes   <- c("sagan")

if (Sys.info()["nodename"] %in% nodes) {

    ##  Create a test database  ----------------------------------------------------
    {
        source("~/BBand_LAP/DEFINITIONS.R")
        cat("\n * * * Using a temp DB * * * \n\n")
        ## copy data to temp
        tyear <- sample(1993:2023, 20)
        # tyear <- c(2015, 2007, 1999, 2021, 1993)
        # tyear <- c(2021)
        dir.create(test_DB_DIR, showWarnings = FALSE, recursive = TRUE)
        system(paste( "cp -rv --update ", DB_HASH_fl, test_DB_HASH_fl))
        system(paste( "cp -rv --update ", DB_META_fl, test_DB_META_fl))
        for (ay in tyear) {
            system(paste0("rsync -avr ", DB_DIR, "/", tyear, "/ ", test_DB_DIR, "/", ay))
        }
        ## replace paths with test paths
        DB_DIR     <- test_DB_DIR
        DB_lock    <- test_DB_lock
        DB_META_fl <- test_DB_META_fl
        DB_HASH_fl <- test_DB_HASH_fl
    }

    ##  Test Broad Band compression  -----------------------------------------------
    BB          <- opendata()
    currentsize <- as.numeric(strsplit(system(paste("du -s", DB_DIR), intern = TRUE), "\t")[[1]][1])
    gatherDB    <- data.frame()

    for (algo in c("gzip", "brotli", "zstd", "lz4", "lzo")) {
        if (codec_is_available(algo)) {
            targetdb <- paste0(DB_DIR, "_temp")
            # for (comLev in c(2, 3, 5, 7, 9, 10, 11, 20)) {
            for (comLev in c(2, 3, 5, 7, 9)) {
                cat("Algo: ", algo, " Level:", comLev, "\n")
                ## remove target dir
                system(paste("rm -rf ", targetdb))
                ## try compression
                aa <- system.time(
                    write_dataset(BB, path          = targetdb,
                                  compression       = algo,
                                  compression_level = comLev,
                                  format            = "parquet",
                                  partitioning      = c("year", "month"),
                                  hive_style        = FALSE)
                )
                ## gather stats
                temp <- data.frame(
                    Date = Sys.time(),
                    Host = Sys.info()["nodename"],
                    User = aa[1],
                    Syst = aa[2],
                    Elap = aa[3],
                    Algo = algo,
                    Level = comLev,
                    Size = as.numeric(strsplit(system(paste("du -s", targetdb), intern = TRUE), "\t")[[1]][1])
                )

                cat(temp$Algo,
                    "level:", temp$Level,
                    "Elap:",  temp$Elap,
                    "Size:",  temp$Size,
                    "Ratio:", temp$Size/currentsize,
                    "\n")
                temp$Ratio   <- temp$Size / currentsize
                temp$Current <- currentsize
                gatherDB     <- data.table(rbind(gatherDB, temp))
            }
        }
    }

    ## Gather results
    if (!file.exists(results)) {
        saveRDS(gatherDB, results)
    } else {
        DATA <- readRDS(results)
        DATA <- unique(rbind(DATA, gatherDB, fill = T))
        saveRDS(DATA, results)
    }
}

setorder(DATA, -Ratio, -Elap)
print(DATA)


DATA <- readRDS(results)
setorder(DATA, -Ratio, -Elap)

DATA <- DATA[Current > 443360, ]


plot(DATA$Ratio, DATA$Level)




tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
