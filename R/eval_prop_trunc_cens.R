eval_prop_trunc_cens <- function(dat_list) {
    lobstr::tree(dat_list, max_depth = 1)
    latent_dat <- dat_list$latent
    latent_dat %>%
        summarize(
            prop_truncated = mean(x > t),
            prop_censored = mean(x < t & c < t)
        )
}
