#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tools/List_dependencies.R"


## __ Describe environment -----------------------------------------------------
env_info <- "~/BBand_LAP/Dependencies.md"

cat("\n## BBand_LAP Current Running Environment\n", file = env_info)
cat("\n", R.version.string, "\n", file = env_info, append = TRUE)

cat("\n### `build_db`\n\n", file = env_info, append = TRUE)
pkgs <- renv::dependencies("~/BBand_LAP/build_db/")
for (ap in unique(pkgs$Package)) {
    cat(sprintf("%16s:  %8s\n", ap, packageVersion(ap)), file = env_info, append = TRUE)
}
cat("\n### `inspect_db`\n\n", file = env_info, append = TRUE)
pkgs <- renv::dependencies("~/BBand_LAP/inspect_db/")
for (ap in unique(pkgs$Package)) {
    cat(sprintf("%16s:  %8s\n", ap, packageVersion(ap)), file = env_info, append = TRUE)
}
cat("\n### `process`\n\n", file = env_info, append = TRUE)
pkgs <- renv::dependencies("~/BBand_LAP/process/")
for (ap in unique(pkgs$Package)) {
    cat(sprintf("%16s:  %8s\n", ap, packageVersion(ap)), file = env_info, append = TRUE)
}
cat("\n### `tools`\n\n", file = env_info, append = TRUE)
pkgs <- renv::dependencies("~/BBand_LAP/tools/")
for (ap in unique(pkgs$Package)) {
    cat(sprintf("%16s:  %8s\n", ap, packageVersion(ap)), file = env_info, append = TRUE)
}


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
