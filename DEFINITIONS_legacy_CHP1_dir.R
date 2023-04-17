
####  Filtering Variables  ####


####    Filters for CHP 1 temperatures    ######################################
CHP_TEMP_MIN      <- -25    ## Drop temperatures below this value
CHP_TEMP_MAX      <-  60    ## Drop temperatures above this value
CHP_TEMP_STD_LIM  <-   1    ## Drop temperatures with standard deviation above this value
CHP_TEMP_UNC_LIM  <-   0.8  ## Drop temperatures with uncertainty above this value



####    Negative radiation when sun is up   ####################################
SUN_ELEV    <- +0         ## When sun is above that           (R50)
MINglbSUNup <- -0.3       ## Exclude signal values below that (R50) above that will set to zero

####    Positive radiation when sun is down   ##################################
SUN_TOO_LOW <- -5         ## When sun is down  (R50)
ERROR_GLOBA <-  5         ## Positive radiation in the night (R50)

####    Drop night time data    ################################################
NIGHT_DROP  <- -5         ## Drop when sun is below that (R60)




