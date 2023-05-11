#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */




## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/Plot_test.R"


source("~/BBand_LAP/DEFINITIONS.R")
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
#library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(shiny)
library(plotly)


##  Create a test database  ----------------------------------------------------

TEST_DB <- FALSE

# TEST_DB <- TRUE

if (TEST_DB) {
    source("~/BBand_LAP/DEFINITIONS.R")
    cat("\n * * * Using a temp DB * * * \n\n")
    ## copy data to temp
    tyear <- 2017
    dir.create(test_DB_DIR, showWarnings = FALSE, recursive = TRUE)
    system(paste( "cp -rv --update ", DB_HASH_fl, test_DB_HASH_fl))
    system(paste( "cp -rv --update ", DB_META_fl, test_DB_META_fl))
    system(paste0("rsync -avr ", DB_DIR, "/", tyear, "/ ", test_DB_DIR, "/", tyear))
    ## replace paths with test paths
    DB_DIR     <- test_DB_DIR
    DB_lock    <- test_DB_lock
    DB_META_fl <- test_DB_META_fl
    DB_HASH_fl <- test_DB_HASH_fl
}



##  Create a new variable to the whole database  -------------------------------

# InitVariableBBDB("somevar", as.numeric(NA))


## Find duration of day ligth

## Tracker start at -5
min_elevation <- -5

BB <- opendata()

Duration <- BB |> filter(Elevat >= min_elevation) |> select(Date, Elevat) |> group_by(as_date(Date)) |> count() |> collect()

Duration <- data.table(Duration)

Duration[ n == max(n) ]

Duration[, max(n)] / 60

## max day duration is 960 minutes or 16 hours

stop()

##  Interactive tests  ---------------------------------------------------------


vars <- grep("Date|Azimuth|doy|year|month", names(BB), invert = TRUE, value = TRUE)


ttd <- BB |> filter(is.na(CHP1_sig_wo_dark)) |> collect()
ttg <- BB |> filter(is.na(CM21_sig_wo_dark)) |> collect()


stop()



if (interactive()) {

    ui <- fluidPage(
        sidebarPanel(

            dateInput("date1", "Date:"),


            checkboxGroupInput("variables", "Variables:",
                               choiceNames  = vars,
                               choiceValues = vars),
            textOutput("txt"),
            width = 2
        )
    )

    server <- function(input, output, session) {
        output$txt <- renderText({
            variables <- paste(input$variables, collapse = ", ")
            sdate <- paste(input$date1, collapse = ", ")
            paste("You chose", variables, sdate)
        })
    }
    shinyApp(ui, server)
}




ui <- fluidPage(
    headerPanel('Example'),
    sidebarPanel(
        selectInput('xcol','X Variable', names(mtcars)),
        selectInput('ycol','Y Variable', names(mtcars)),
        selected = names(mtcars)[[2]]),
    mainPanel(
        plotlyOutput('plot')
    )
)

server <- function(input, output) {

    x <- reactive({
        mtcars[,input$xcol]
    })

    y <- reactive({
        mtcars[,input$ycol]
    })


    output$plot <- renderPlotly(
        plot1 <- plot_ly(
            x = x(),
            y = y(),
            type = 'scatter',
            mode = 'markers')
    )

}

shinyApp(ui,server)



myunlock(DB_lock)



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

