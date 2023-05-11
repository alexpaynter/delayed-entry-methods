coverage_helper <- function(truth, lower, upper) {
    below_upper <- truth <= upper
    above_lower <- truth >= lower
    return(below_upper & above_lower)
}
