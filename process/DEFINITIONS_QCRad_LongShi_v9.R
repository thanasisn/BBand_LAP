
#'
#' Parameters for Radiation Quality Control by Long and Shi version 9.
#'
#+ echo=F, include=T

## gather configurations for quality control
QS <- list()



#'
#' ## 1. PHYSICALLY POSSIBLE LIMITS PER BSRN
#'

QS$dir_SWdn_min <-  -4  # Minimum direct value to consider valid measurement
QS$dir_SWdn_dif <- 327  # Closeness to to TSI
QS$glo_SWdn_min <-  -4  # Minimum global value to consider valid measurement
QS$glo_SWdn_off <- 160  # Global departure offset above the model
QS$glo_SWdn_amp <- 1.3  # Global departure factor above the model



#'
#' ## 2. EXTREMELY RARE LIMITS PER BSRN
#'

# Upper modeled values
QS$Dir_SWdn_amp     <-    0.91 # Direct departure factor above the model
QS$Dir_SWdn_off     <- -140    # Direct departure offset above the model
QS$Glo_SWdn_amp     <- 1.18    # Global departure factor above the model
QS$Glo_SWdn_off     <- 40      # Global departure offset above the model
# Minimum accepted values
QS$dir_SWdn_min_ext <-   -2    # Extremely Rare Minimum Limits
QS$glo_SWdn_min_ext <-   -2    # Extremely Rare Minimum Limits



#'
#' ## 3. COMPARISON TESTS PER BSRN “non-definitive”
#'

QS$dif_rati_po0  <-  0.03
QS$dif_rati_po2  <-  0.08
QS$dif_sza_break <- 75
QS$dif_rati_pr1  <-  1.03
QS$dif_rati_pr2  <-  1.06
QS$dif_watt_lim  <-  10



#'
#' ## 4. Climatological (configurable) Limits
#'

QS$clim_lim_C3 <- 0.77
QS$clim_lim_D3 <- 0.81
QS$clim_lim_C1 <- 1.14
QS$clim_lim_D1 <- 1.32



#'
#' ## 5. Tracker is off test
#'

## criteria
QS$Tracking_min_elev <-   15
QS$ClrSW_lim         <-    0.85
QS$glo_min           <-   25
## Global Clear SW model
QS$ClrSW_a           <- 1050.5
QS$ClrSW_b           <-    1.095



#'
#' ## 6. Rayleigh Limit Diffuse Comparison
#'

# criteria
QS$Rayleigh_upper_lim <- 500   # Upper departure diffuse limit
QS$Rayleigh_lower_lim <-  -3   # Lower departure diffuse limit
QS$Rayleigh_dif_glo_r <-   0.8 # Low limit diffuse/global < threshold
QS$Rayleigh_glo_min   <-  50   # Low limit minimum global



#'
#' ## 8. Test for inverted values
#'

QS$dir_glo_invert  <- 5  # Diffuse Inversion test: DIRhor - GLBhor > lim[%]
QS$dir_glo_glo_off <- 5  # Diffuse Inversion test: apply for GLBhor > offset



#'
#' ## 9. Clearness index test
#'

QS$CL_idx_max <-  1.13  # Upper Clearness index accepted level
QS$CL_idx_min <- -0.001 # Lower Clearness index accepted level
QS$CL_idx_ele <-  8     # Apply for elevations above this angle

