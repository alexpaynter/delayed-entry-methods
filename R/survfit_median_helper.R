
# Gets the median and 95% CI from a survfit object.
# If there are strata, one row per stratum.  Otherwise, one row with stratum = "all".
survfit_median_helper <- function(survfit_obj) {
    
    tab <- survfit_obj %>%
        summary(.) %>%
        `$`(.,"table")
    
    if (any(class(tab) %in% c("matrix", "array"))) {
        tab <- as_tibble(tab, rownames = "stratum")
    } else {
        tab <- tab %>%
            as_tibble_row(.) %>%
            mutate(stratum = "all") 
        
    }
    
    tab %<>%
        select(
            stratum,
            median_surv_hat = median,
            median_surv_hat_lcb =  `0.95LCL`,
            median_surv_hat_ucb = `0.95UCL`
        )
    return(tab)
}
