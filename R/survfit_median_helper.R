
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
    
    tab %>%
        select(
            strata,
            median_surv = median,
            median_surv_lcb =  `0.95LCL`,
            median_surv_ucb = `0.95UCL`
        )
}
