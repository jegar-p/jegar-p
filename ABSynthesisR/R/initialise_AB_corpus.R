#' Generate a set of synthetic experiment results based on desired conversion probabilities
#'
#' @description initialize_synthetic_AB_corpus creates a set of numbers that represent what may be seen at the end of a set of AB tests.
#' @param experiments The number of experiments that should be generated
#' @param minimum_visitors The number of visitors that should be simulated
#' @param maximum_visitors The number of visitors that should be simulated
#' @param baseline_probability Proportion of visitors that perform the desired action in the base version
#' @param variant_probability Proportion of visitors that perform the desired action in the variant version
#' @return A set of numbers that represent what would be seen at the end of one AB test with the underlying input parameters.
#' @examples initialize_synthetic_AB_corpus(50, 5000,10000, 0.1, 0.2)
#' @export

initialize_synthetic_AB_corpus <- function(experiments, minimum_visitors, maximum_visitors, baseline_probability, variant_probability)
{
return(as.data.frame(do.call(rbind, lapply(rep(minimum_visitors, experiments), initialize_synthetic_AB_data, maximum_visitors = maximum_visitors, baseline_prob = baseline_probability, variant_prob = variant_probability))))
}
