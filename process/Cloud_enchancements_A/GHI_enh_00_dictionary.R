
#### dictionary  ---------------------------------------------------------------
dictionary <- list(
  # Name                Short                          Long
  ST_daily         = c("Daily Observations",          "Daily statistics on all obvservations" ),
  ST_E_daily       = c("Daily Enhancements",          "Daily Enhancements statistics"         ),
  ST_E_daily_seas  = c("Seasonal Daily Enhancements", "Seasonal Daily Enhancements"           ),
  ST_extreme_daily = c("Daily Extreme Enhancements",  "Daily Extreme Enhancements statistics" ),
  DATA             = c("All Observations",            "All Observations"                      ),
  empty            = c(" --- empty --- ",             " --- empty --- "                       )
)


#' Get nice name for a variable in BB DB
#'
#' @param x    Name of a column
#' @param type What to return one of c("short", "long")
#'
#' @return     A string for a variable
#' @export
#'
tr_var <- function(x, type = "short") {
  require(stringr)
  types <- c("short", "long")

  if (!type %in% types) {
    cat("No such column:", type, "\n")
    return(NA)
  }

  ty  <- which(type == types)
  res <- c()
  for (ax in x) {
    res <- c(res,
             as.vector(
               unlist(dictionary[str_detect(ax, names(dictionary))])[[ty]]
             )
    )
  }
  if (is.null(res)) {
    return(x)
  } else {
    return(res)
  }
}



## colour for each variable type
varcol <- function(avar) {
  # switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
  switch(gsub("^.*_", "", gsub("\\..*", "", "GLB_diff.p_95")),
         # diff = "green",
         diff = "#317529",
         ench = "blue",
         rati = "magenta",
         "black"
  )
}

# varcol("GLB_diff.p_95")
# gsub("^.*_", "", gsub("\\..*", "", "GLB_diff.p_95"))



## Name for each variable
varname <- function(avar) {
  switch(gsub("\\..*$", "", gsub(".*_", "", avar)),
         diff    = "OI",
         ench    = "Relative Enchancement",
         rati    = "Enchancement ratio",
         wattGLB = "GHI",
         avar
  )
}

## Stats name
staname <- function(avar) {
  switch(gsub(".*\\.", "", avar),
         sum    = "energy",
         min    = "minimum",
         max    = "maximun",
         median = "median",
         mean   = "mean",
         sumPOS = "energy",
         p_5    = "5% percentile",
         p_10   = "10% percentile",
         p_90   = "90% percentile",
         p_95   = "95% percentile",
         sumNEG = "energy",
         N_pos  = "number of positive cases",
         N_neg  = "number of negative cases",
         N      = "number of cases",
         TotalN = "number of observations",
         avar
  )
}


staname("GLB_diff.p_95")



## __ Set ggplot global theme  -------------------------------------------------

gg_text_size <- 16

theme_paper <- function(){
  # font <- "Georgia"   #assign font family up front

  theme_bw(
    base_size = gg_text_size  # global font size was 14
    ) %+replace%    #replace elements we want to change
    theme(
      # panel.grid.major = element_blank(),    #strip major gridlines
      # panel.grid.minor = element_blank(),    #strip minor gridlines
      panel.background   = element_rect(fill = 'transparent'), #transparent panel bg


      # axis.text = element_text(face = "bold"), # bold axis labels
      # text      = element_text(size = 15),     ## srarted at 14

      # axis.ticks = element_blank(),          #strip axis ticks

      #text elements
      # plot.title = element_text(             #title
      #     family = font,            #set font family
      #     size = 20,                #set font size
      #     face = 'bold',            #bold typeface
      #     hjust = 0,                #left align
      #     vjust = 2),               #raise slightly
      #
      # plot.subtitle = element_text(          #subtitle
      #     family = font,            #font family
      #     size = 14),               #font size
      #
      # plot.caption = element_text(           #caption
      #     family = font,            #font family
      #     size = 9,                 #font size
      #     hjust = 1),               #right align
      #
      # axis.title = element_text(             #axis titles
      #     family = font,            #font family
      #     size = 10),               #font size
      #
      # axis.text = element_text(              #axis text
      #     family = font,            #axis famuly
      #     size = 9),                #font size
      #
      # axis.text.x = element_text(            #margin for axis text
      #     margin=margin(5, b = 10)),

      plot.background       = element_rect(fill = 'transparent', color = NA), #transparent plot bg
      # panel.grid.major      = element_blank(), #remove major gridlines
      # panel.grid.minor      = element_blank(), #remove minor gridlines
      legend.background     = element_rect(fill = 'transparent',
                                           linewidth = 0.5,
                                           color = "black"), #transparent legend bg
      legend.box.background = element_rect(fill = 'transparent'), #transparent legend panel
      # axis.line             = element_line(linewidth = .5, colour = "black", linetype = 1),

      NULL
    )
}

theme_set(theme_paper())

