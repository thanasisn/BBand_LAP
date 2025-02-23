#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tools/List_dependencies.R"

source("~/CODE/FUNCTIONS/R/listDependencies.R")

## __ Describe environment -----------------------------------------------------
folders <- c(
  "~/BBand_LAP/",
  # "~/BBand_LAP/build_db/",
  # "~/BBand_LAP/build_duckdb/",
  # "~/BBand_LAP/inspect_db/",
  # "~/BBand_LAP/inspect_duckdb/",
  # "~/BBand_LAP/process/",
  # "~/BBand_LAP/tools/",
  NULL
)

for (af in folders) {
  env_info <- paste0(af, "/Dependencies.md")
  listDependencies(af,
                   output = env_info,
                   output_overwrite = TRUE)

}

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
