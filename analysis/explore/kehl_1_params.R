# The original Kehl 1 parameters were in months, and had the following ranges:
# h0: from 0.005 to 0.03.
# h1/h0: from 1.05 to 3.
# a_prop: from 0.05 to 0.5.
# c_max: Fixed at 120.
# x_max: Fixed at 12.

# Converting to years, this puts in the range of:
# h0: 0.06 to 0.36
# h1/h0:  Same, therefore h1 %in% (0.0004375, 0.0075)
# a_prop: Same.
# c_max:  Fixed at 10
# x_max:  Fixed at 1.

# Some good starting parameters that get us ~20% truncation and ~20% censoring,
#.  while being roughly in the middle of the Kehl simulation ranges, are:
tests <- purrr::map_dfr(
    .x = sample.int(n = 10^7, size = 100, replace = T),
    .f = \(x) {    
        gen_kehl_1(gen_seed = x,
                   n = 100, h0 = 0.25, h1 = 0.5, a_prop = .25,
                   c_max = 15, x_max = 1.5) %>%
            eval_prop_trunc_cens(.)
    }
)

tests %>% colMeans(.)

