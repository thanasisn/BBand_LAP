# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */


#+ echo=F, include=T
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!h"    )

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/test.R"
Script.ID    <- "Q1"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",  basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)


# Daily plots
DO_PLOTS       <- TRUE
# Ignore previous flagged points in plots (not fully implemented yet)
IGNORE_FLAGGED <- TRUE   ## TRUE is the default of the original
IGNORE_FLAGGED <- FALSE

## __ Select a part of data to plot  -------------------------------------------
PARTIAL    <- FALSE
PARTIAL    <- TRUE
PLOT_FIRST <- as_date("1993-01-01")
PLOT_LAST  <- as_date("2024-01-01")


flagname_DIR <- "QCv10_02_dir_flag"
flagname_GLB <- "QCv10_02_glb_flag"

con <- dbConnect(duckdb(dbdir = DB_BROAD))

grep("_SZA_", dbListTables(con), value = TRUE)
dbRemoveTable(con)
stop()
dbDisconnect(con, shutdown = TRUE)
stop()

DD <- tbl(con, "LAP")

test  <- DD |>
  filter(!is.na(GLB_strict))  |>
  filter(!is.na(DIFF_strict)) |> head() |> collect() |> data.table()


## create a test db
file.remove("~/ZHOST/test.duckdb")
con <- dbConnect(duckdb(dbdir = "~/ZHOST/test.duckdb"))
dbWriteTable(con, "test", test)


tt <- tbl(con, "test")

categories <- c("empty",
                "pass",
                "Extremely rare limits min (3)",
                "Extremely rare limits max (4)")


make_categorical_column(flagname_DIR, categories, con, "test")



tt <- tbl(con, "test")

tt |> glimpse()

add <- tt |> head(1) |>
  mutate(QCv10_02_dir_flag = "Extremely rare limits min (3)")

update_table(con, add, "test", "Date")





#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
