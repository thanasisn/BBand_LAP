#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2024 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Theory/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

try({
  rmarkdown::render(input      = "~/BBand_LAP/parameters/theory/Air_mass_models.R",
                    output_dir = output_dir)
})
try({
  rmarkdown::render(input      = "~/BBand_LAP/parameters/theory/Extraterrestrial_radiation_models.R",
                    output_dir = output_dir)
})
try({
  rmarkdown::render(input      = "~/BBand_LAP/parameters/theory/Clear_sky_irradiance_models.R",
                    output_dir = output_dir)
})
try({
  rmarkdown::render(input      = "~/BBand_LAP/parameters/theory/Linke_turbidity_models.R",
                    output_dir = output_dir)
})
try({
  rmarkdown::render(input      = "~/BBand_LAP/parameters/theory/Diffuse_fraction_models.R",
                    output_dir = output_dir)
})

