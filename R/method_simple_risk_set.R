method_simple_risk_set <- function(dat) {
    s_obj <- with(dat, 
                  Surv(time = x,
                       time2 = y, 
                       event = event))
    
    surv_fit_all <- with(dat, survfit(s_obj ~ 1)) 
        
    med_surv_all <- surv_fit_all %>%
        survfit_median_helper()
    
    med_surv_strata <- with(dat, survfit(s_obj ~ a)) %>%
        survfit_median_helper()
    
    med_surv <- bind_rows(med_surv_all, med_surv_strata)
    
    cox_ph_a <- with(obs_test, coxph(s_obj ~ a)) %>%
        tidy(., conf.int = T, exponentiate = T)
    
    return(tibble(med_surv = list(med_surv),
                  cox_ph_a = list(cox_ph_a),
                  # save for plotting or other metrics:
                  surv_fit_all = list(surv_fit_all)))
    
}
