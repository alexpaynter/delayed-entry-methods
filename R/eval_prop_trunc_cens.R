eval_prop_trunc_cens <- function(dat_list) {
    latent_dat <- dat_list$latent
    latent_dat %>%
        summarize(
            prop_truncated = mean(x > t),
            # This is the number censored AFTER truncation, as in Chiou (2022).
            prop_censored = mean(x < t & c < t)
        )
}
