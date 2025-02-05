#!/usr/bin/env Rscript
# /* Copyright (C) 2019 Athanasios Natsis <natsisphysicist@gmail.com> */

rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()

library(rmarkdown)
library(knitr)

setwd("~/CM_21_GLB/")

cat("Run manually to create the graphs if everything seems good:\n")
cat("./CM21_P98_Plot_all_years_LAP.R\n")
cat("./CM21_P99_Plot_all_daily_LAP.R\n")

# ## some more nice plots for sirena display
# source("./CM21_P98_Plot_all_years_LAP.R")
# source("./CM21_P99_Plot_all_daily_LAP.R")
