method_simple_risk_set <- function(dat) {
    s_obj <- with(dat, 
                  Surv(time = x
                       time2 = y, 
                       event = event))
    survfit(s_obj ~ 1)
}
