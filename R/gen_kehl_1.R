
#' @title Kehl (2023) #1 data generator
#' 
#' @description Independent genomic testing and survival risk.  Exponential survival risk, uniform(0,12) censoring distribution, random hazard rate and number of patients per cohort.
#' 
#' @details doi: 10.1158/1055-9965.EPI-22-0875.  This code is based on the first scenario, except that use fixed (rather than random) parameters for n, the baseline hazard rate (h0), biomarker proportion (a_prop), and the hazard rate for participants with the biomarker (h1).
#' 
#' @param n The number of recruited participants (that is, not including truncated cases).
#' @param h0 The baseline hazard rate (for those with a = 0).
#' @param h1 The hazard rate for those with a = 1.
#' @param a_prop The proportion of the cohort with the biomarker.
#' 
gen_kehl_1 <- function(n,
                       h0,
                       h1,
                       a_prop,
                       gen_seed = NULL,
                       x_max = 1,
                       c_max = 10,
                       return_type = 'both') {
    if (is.null(gen_seed)) {
        gen_seed <- sample.int(n = 10^7, 1) 
        cli::cli_inform(message = glue("Using randomly generated seed {gen_seed}."))
    }
    
    latent_n <- n*2
    enough_latent_n <- F
    
    while (enough_latent_n %in% F) {
        set.seed(gen_seed)
        dat <- tibble(
            a = rbinom(latent_n, size = 1, prob = a_prop),
            x = runif(n = latent_n, 0, x_max), # originally 12 months.
            c = runif(n = latent_n, 0, c_max),
            t_rates = if_else(a %in% 0, h0, h1),
            t = rexp(n = latent_n, rate = t_rates),
        )
        
        n_poss_obs <- dat %>% filter(x <= t) %>% nrow
        if (n_poss_obs >= n) {
            dat %<>% 
                mutate(not_truncated = x <= pmin(t,c),
                       cum_nt = cumsum(not_truncated),
                       last_case = cum_nt == n & lag(cum_nt) != n) %>%
                slice(1:which(last_case %in% 1)) %>%
                select(-c(not_truncated, cum_nt, last_case))
            # we stop recruiting at n cases in reality.
            enough_latent_n <- T
        } else if (latent_n >= 100*n){
            cli::cli_abort("Cohort recruitment termination - is the truncation rate too high?")
        } else {
            latent_n <- latent_n * 2
        }
    }
    
    latent_df <- dat %>%
        add_id(., prefix = "s-", name = "id") %>%
        mutate(
            event = if_else(t < c, 1, 0),
            y = pmin(t,c)
        ) %>%
        select(id, everything())
    
    observed_df <- latent_df %>%
        filter(y >= x) %>%
        # not sure if I'll ever need this, but can't hurt.
        add_id(., prefix = "obs-", name = "id_obs") %>%
        select(id, id_obs, a, x, y, event)

    
    
    # The latent list contains parameters which would be not observed
    #.  in a real experiment.
    
    if (return_type %in% "both") {
        return(
            list(latent = latent_df,
                 observed = observed_df)
        )
    } else if (return_type %in% "latent") {
        return(latent)
    } else if (return_type %in% "observed") {
        return(observed)
    } else {
        cli::cli_abort(message = "Invalid return type")
    }
} 






