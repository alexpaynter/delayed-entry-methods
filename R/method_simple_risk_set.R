method_simple_risk_set <- function(dat) {
    s_obj <- with(dat, 
                  Surv(time = x,
                       time2 = y, 
                       event = event))
    # Data for overall survival:
    os_dat <- with(dat, survfit(s_obj ~ 1)) %>% 
        summary %>%
        `$`(.,"table") %>%
        as_tibble_row() %>%
        select(
            median_surv = median,
            median_surv_lcb =  `0.95LCL`,
            median_surv_ucb = `0.95UCL`
        )
    
}
