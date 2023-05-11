method_risk_set_adj_one_arm <- function(dat) {
    s_obj <- with(dat, 
                  Surv(time = x,
                       time2 = y, 
                       event = event))
    
    surv_fit_all <- with(dat, survfit(s_obj ~ 1)) 
    
    med_surv_all <- surv_fit_all %>%
        survfit_median_helper(.)
    
    return(med_surv_all)
}
