#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */




## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/Plot_test.R"


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


##  Create a test database  ----------------------------------------------------
TEST_DB <- TRUE
if (TEST_DB) {
    source("~/BBand_LAP/DEFINITIONS.R")
    cat("\n * * * Using a temp DB * * * \n\n")
    ## copy data to temp
    tyear <- sample(1993:2023, 5)
    tyear <- c(2015, 2007, 1999, 2021, 1993)
    tyear <- c(2021)
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




BB <- opendata()

dir_size <- function(path, recursive = TRUE) {
    stopifnot(is.character(path))
    files <- list.files(path, full.names = T, recursive = T)
    vect_size <- sapply(files, function(x) file.size(x))
    size_files <- sum(vect_size)
    size_files
}



for (algo in c("gzip", "brotli", "zstd", "lz4", "lzo", "bz2")) {
    if (codec_is_available(algo)) {
        cat("AVAILABLE:", algo, "\n")
    }
}

# for (algo in c("snappy", "gzip", "brotli", "zstd", "lz4", "lzo", "bz2")) {

gatherDB <- data.frame()
for (algo in c("gzip", "brotli", "zstd", "lz4", "lzo", "bz2")) {
    if (codec_is_available(algo)) {
        cat("Algo ", algo, "\n")
        targetdb <- paste0(DB_DIR, "_temp")
        system(paste("rm -rf ", targetdb))

        for (comLev in c(25, 50, 75, 100)) {


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
                Size = strsplit(system(paste("du -s", targetdb), intern = TRUE), "\t")[[1]][1]
            )

            cat(temp$Algo, "level:", temp$Level, "Elap:", temp$Elap, "Size:", temp$Size, "\n")

            gatherDB <- rbind(gatherDB, temp)

        }
    }
}








tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

