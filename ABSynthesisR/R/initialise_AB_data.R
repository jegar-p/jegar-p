#' Generate a synthetic experiment result based on desired conversion probabilities
#'
#' @description initialize_synthetic_AB_data creates a set of numbers that represent what may be seen at the end of an AB test.
#' @param minimum_visitors The number of visitors that should be simulated
#' @param maximum_visitors The number of visitors that should be simulated
#' @param baseline_probability Proportion of visitors that perform the desired action in the base version
#' @param variant_probability Proportion of visitors that perform the desired action in the variant version
#' @return A set of numbers that represent what would be seen at the end of one AB test with the underlying input parameters.
#' @examples initialize_synthetic_AB_data()
#' @export
initialize_synthetic_AB_data <- function(minimum_visitors = 5000,
                                         maximum_visitors = 10000,
                                         baseline_probability = 0.01,
                                         variant_probability = 0.02
    ){

  AB_x <- c("converted", "did not convert")
  AB_size <- sample(x = minimum_visitors:maximum_visitors, size = 1) # Determine the overall size of the study
  base_size = floor(AB_size/2 + sample(x = (AB_size * -0.025):(AB_size * 0.025), size = 1))
  variant_size = AB_size - base_size

  AB_replace <- TRUE
  base_prob <- c(baseline_probability,1-baseline_probability)
  variant_prob <- c(variant_probability,1-variant_probability)
  base_visitors <- as.factor(sample(x = AB_x,
                                    size = base_size,
                                    replace = AB_replace,
                                    prob = base_prob))
  variant_visitors <- as.factor(sample(x = AB_x,
                                       size = variant_size,
                                       replace = AB_replace,
                                       prob = variant_prob))
  my_experiment <- table("group" = c(rep(as.factor("base"), length(base_visitors)), rep(as.factor("variant"), length(variant_visitors))),
                              "result" = as.factor(c(base_visitors, variant_visitors)))
  my_experiment_summary <- c(
    "successes_base" = my_experiment['base', 'converted'],
    "visitors_base" = my_experiment['base', 'converted']+ my_experiment['base', 'did not convert'],
    "successes_variant" = my_experiment['variant', 'converted'],
    "visitors_variant" = my_experiment['variant', 'converted']+ my_experiment['variant', 'did not convert']
  )
 # Prevent generation of 0 response outcomes
my_experiment_summary
  if(my_experiment_summary["successes_base"] == 0)
  {
      my_experiment_summary["successes_base"] <- 1
      my_experiment_summary["successes_variant"] <- my_experiment_summary["successes_variant"] + 1}

  if(my_experiment_summary["successes_variant"] == 0)
  {
      my_experiment_summary["successes_variant"] <- 1
      my_experiment_summary["successes_base"] <- my_experiment_summary["successes_base"] + 1}

  return(my_experiment_summary)
}
