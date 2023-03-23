#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

dd <- ""

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/Mark_excluded_data_ranges.R"


source("~/BBand_LAP/DEFINITIONS.R")
source("~/CHP_1_DIR/Functions_CHP1.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)
on.exit(myunlock(DB_lock))

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
#library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(shiny)


opendata <- function() {
    open_dataset(sources       = DB_DIR,
                 unify_schemas = TRUE,
                 hive_style    = FALSE,
                 partitioning  = c("year", "month"))
}

writedata <- function(.) {
    write_dataset(., path      = DB_DIR,
                  format       = "parquet",
                  partitioning = c("year", "month"),
                  hive_style   = FALSE)
}


BB <- opendata()



grep("Date|Azimuth|doy|year|month", names(BB), invert = TRUE, value = TRUE)



stop()


## Only run examples in interactive R sessions
if (interactive()) {

    ui <- fluidPage(
        checkboxInput("somevalue", "Some value", FALSE),
        verbatimTextOutput("value")
    )
    server <- function(input, output) {
        output$value <- renderText({ input$somevalue })
    }
    shinyApp(ui, server)
}











tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
