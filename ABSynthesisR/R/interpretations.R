interpretation <- function(ind_results, sum_results)
{  if(sum_results$summary_standardized_mean_difference <= 0)
             {perf <-  c("worse", "fewer")}else
            {perf <- c("better", "more")}

return(
"difference_pct" = list(sprintf("Your new version performed %s%% %s than the old version ", sum_results$summary_standardized_mean_difference, perf[1]),

"difference_real" = sprintf("Based on these results, we expect that your new version should get %s%% %s clicks than the old version", round((sd(ind_results$successes_base)*sum_results$summary_standardized_mean_difference)/mean(ind_results$successes_base)*100), perf[2]),

"conf_interval" = sprintf("We are 95%% sure that you will see at least %s %% more clicks with the new version than with the old version, but you could see up to %s %% more clicks if you're lucky", (sd(ind_results$successes_base)*sum_results$lower_limit_summary_standardized_mean_difference)/mean(ind_results$successes_base)*100, (sd(ind_results$successes_base)*sum_results$upper_limit_summary_standardized_mean_difference)/mean(ind_results$successes_base)*100),

"conf_interval_neg" = sprintf("We are 95%% sure that you will see at least %s %% fewer clicks with the new version than with the old version, but you could see up to %s %% fewer clicks if you're unlucky", round((sd(ind_results$successes_base)*sum_results$upper_limit_summary_standardized_mean_difference)/mean(ind_results$successes_base)*100, 2), round((sd(ind_results$successes_base)*sum_results$lower_limit_summary_standardized_mean_difference)/mean(ind_results$successes_base)*100, 2)),

"not_significant" = "We can't be sure that the new version made any difference. You can wait until you have a larger sample, add a platform, or try a more extreme change."
))
    }
