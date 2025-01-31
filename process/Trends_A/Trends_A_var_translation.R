
#### dictionary ####
dictionary <-
    list(DIR_att             = 'Dir. Beam Irrad.',
         HOR_att             = 'Dir. Irrad. horizontal plane',
         DIR_transp          = 'Transp. for Dir. Irrad.',
         wattGLB             = "Global Radiation raw",
         GLB_att             = "SDR",
         tsi1au_att          = "TSI at 1au",
         near_tcc_des        = "TCCn deseasonalized",
         near_tcc_att        = "TCC nearest",
         bilin_tcc_att       = "TCC bilinear fit",
         near_tcc_zero_N     = "TCCn zero counts",
         near_tcc_TN         = "TCCn total counts",
         near_tcc_NOzero_att = "TCCn without zeros",
         near_tcc_clear_att  = "TCCn almost completely clear",
         near_tcc_cloud_att  = "TCCn complementary to almost completely clear",
         near_tcc_zero_rel   = "TCCn zeros relative to total",
         ALL                 = "All sky cond.",
         CLEAR               = "Clear sky cond.",
         CLOUD               = "Cloudy sky cond.")

## Function to translate objects names
translate <- function(x) {
    res <- c()
    for (ax in x) {
        ## get match
        amatch <- as.vector(unlist(
            dictionary[stringr::str_detect(ax, names(dictionary))]
        ))
        ## return same if not found
        if (is.null(amatch)) {
            amatch <- ax
        }
        ## gather results
        res <- c(res, amatch)
    }
    return(res)
}


agrep("near_tcc_N", names(dictionary), value = T)
