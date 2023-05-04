#' @title Kehl (2023) #1 data generator
#' 
#' @description Independent genomic testing and survival risk.  Exponential survival risk, uniform(0,12) censoring distribution, random hazard rate and number of patients per cohort.
#' 
#' @details doi: 10.1158/1055-9965.EPI-22-0875.  This code is based on 
gen_one_arm_exp_all <- function(
        lambda_t,
        lambda_x,
        lambda_c,
        n_latent,
        gen_seed = NULL, 
        return_type = 'both'
) {
    if (is.null(gen_seed)) {
        gen_seed <- sample.int(n = 10^7, 1) 
        cli::cli_inform(message = glue("Using randomly generated seed {gen_seed}."))
    }
    
    hazard_rate = runif(1, min=0.005, max=0.03)
    num_patients_per_cohort = as.integer(runif(1, min=50, max=5000))
    biomarker_rate_ratio = runif(1, min=1.05, max=3)
    biomarker_prevalence = runif(1, 0.05, 0.5)
    biomarker_statuses = rbinom(num_patients_per_cohort, 
                                1, 
                                biomarker_prevalence)
    
    final_rates = ifelse(biomarker_statuses == 0, 
                         bl_haz, 
                         bl_haz*biomarker_rate_ratio)
    
    surv_times = rexp(num_patients_per_cohort, rate=final_rates)
    
    testing_times = runif(num_patients_per_cohort, 0, 12)
    # AP: Impossible that U(0,12) < 0, removed.
    # testing_times = pmax(0, testing_times) 
    censoring_times = runif(num_patients_per_cohort, 0, 120)
    
    latent_df <- tibble(
        x = testing_times,
        t = surv_times,
        c = censoring_times,
        a = biomarker_statuses
    ) %>%
        mutate(
            # give each subject a unique ID:
            id = paste0("s-", stringr::str_pad(1:n(), 
                                               width = 5, 
                                               side = "left",
                                               pad = "0")),
            event = if_else(t < c, 1, 0),
            y = pmin(t,c)
        ) %>%
        select(id, everything())
    
    observed_df <- latent_df %>%
        filter(y > x) %>%
        # not sure if I'll ever need this, but can't hurt.
        mutate(id_obs = paste0("obs-", stringr::str_pad(1:n(), 
                                                        width = 5, 
                                                        side = "left",
                                                        pad = "0"))) %>%
        # We do not get to see the latent survival/censor times, but we
        #.  know the cohort entry event time (x) for those that entered the cohort.
        select(id, id_obs, a, x, y, event)
    
    
    
    # The latent list contains parameters which would be not observed
    #.  in a real experiment.
    latent <- list(
        a_true = biomarker_rate_ratio,
        a_freq_true = biomarker_prevalence,
        bl_haz_true = hazard_rate,
        latent_df = list(latent_df)
    )
    
    if (return_type %in% "both") {
        return(
            list(latent = latent, 
                 observed = list(observed_df))
        )
    } else if (return_type %in% "latent") {
        return(latent)
    } else if (return_type %in% "observed") {
        return(observed)
    } else {
        cli::cli_abort(message = "Invalid return type")
    }
}
