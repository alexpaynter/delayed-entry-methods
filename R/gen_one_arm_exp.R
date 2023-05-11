#' @title Generate a truncated one-arm survival sample with exponential distributions for everything.
#' 
#' @description Exponential distribution for time to event (lambda_t), time to cohort entry (lambda_x) and time to censoring (lambda_c).  All distributions are independent.
#' 
#' @details doi: 10.1158/1055-9965.EPI-22-0875.  This code is based on 
gen_one_arm_exp <- function(
        n,
        lambda_t,
        lambda_x,
        lambda_c,
        gen_seed = NULL,
        return_type = 'both'
) {
    if (is.null(gen_seed)) {
        gen_seed <- sample.int(n = 10^7, 1) 
        cli::cli_inform(message = glue("Using randomly generated seed {gen_seed}."))
    }
    
    # In reality you only observe those cases with T > X.  We start with 2x
    #  as many cases, and keep doubling from there, until we have enough people
    #  who meet the condition.
    latent_n <- n*2
    enough_latent_n <- F
    
    while (enough_latent_n %in% F) {
        set.seed(gen_seed)
        dat <- tibble(
            x = rexp(n = latent_n, rate = lambda_x),
            t = rexp(n = latent_n, rate = lambda_t),
            c = rexp(n = latent_n, rate = lambda_c),
        )
        
        n_poss_obs <- dat %>% filter(x <= t) %>% nrow
        if (n_poss_obs >= n) {
            dat %<>% 
                mutate(not_truncated = x <= pmin(t,c),
                       cum_nt = cumsum(not_truncated),
                       last_case = cum_nt == n & lag(cum_nt) != n) #
            dat %<>% 
                slice(1:which(last_case %in% 1)) %>%
                select(-c(not_truncated, cum_nt, last_case))
                # we stop recruiting at n cases in reality.
            enough_latent_n <- T
        } else if (latent_n >= 100*n){
            cli::cli_abort("Cohort recruitment termination - is lambda_t >> lambda_x?")
        } else {
            latent_n <- latent_n * 2
        }
    }
    
    latent_df <- dat %>%
        mutate(
            # give each subject a unique ID:
            id = paste0("s-", stringr::str_pad(1:n(),
                                               width = ceiling(log10(n()))+1,
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
                                                        width = ceiling(log10(n()))+1,
                                                        side = "left",
                                                        pad = "0"))) %>%
        # We do not get to see the latent survival/censor times, but we
        #.  know the cohort entry event time (x) for those that entered the cohort.
        select(id, id_obs, x, y, event)
    
    # 
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



