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
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
#library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(shiny)
library(plotly)


##  Create a test database  ----------------------------------------------------

TEST_DB <- TRUE

if (TEST_DB) {
    source("~/BBand_LAP/DEFINITIONS.R")
    cat("\n * * * Using a temp DB * * * \n\n")
    ## copy data to temp
    tyear <- 2017
    dir.create(test_DB_DIR)
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

BB <- opendata()
for (avar in c("chp1_t_cor_factor")) {
    if (!any(names(BB) == avar)) {
        cat("Create column: ", avar, "\n")
        BB <- BB |> mutate( !!avar := as.numeric(NA)) |> compute()
        writedata(BB)
    }
}


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
