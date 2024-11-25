#' Get a Forest plot of combined experiments
#'
#' @description forest_plot returns summary statistics combining the results of all online controlled experiments
#' @param experiment_ids A list of unique experiment ids if available
#' @param successes_base Count of responses in the base condition
#' @param visitors_base Count of visitors exposed to the base condition
#' @param successes_variant Count of responses in the base condition
#' @param visitors_variant Count of visitors exposed to the base condition
#' @return A forest plot stacking the results of multiple tests of the same element, page, site, app, etc.
#' @examples forest_plot(c(1,2,3), c(10, 15,20), c(50, 50, 50), c(12, 17, 22), c(50, 50, 50))
#' @export
forest_plot <- function(experiment_ids = NA, successes_base, visitors_base, successes_variant, visitors_variant)
{     rapidapi_key <- test_key()
    if(is.na(experiment_ids[1])){
         experiment_ids <- seq(1:length(successes_base))
     }

    ma_adv <- ABSynthesis_advanced(successes_base, visitors_base, successes_variant, visitors_variant)

    standardized_mean_difference <- round(as.numeric(ma_adv$standardized_mean_difference),2)

    max_pos <- max(as.numeric(ma_adv$upper_limit_standardized_mean_difference)) +.05
    min_pos <- min(as.numeric(ma_adv$lower_limit_standardized_mean_difference)) -.05

    if(max_pos < 0){max_pos = abs(min_pos)}
    if(min_pos > 0){min_pos = 0 - max_pos }
    scen <- data.table::data.table("experiment" = experiment_ids,
                       "standardized_mean_difference" = standardized_mean_difference,
                       "upper_limit_standardized_mean_difference" = ma_adv$upper_limit_standardized_mean_difference,
                       "lower_limit_standardized_mean_difference" = ma_adv$lower_limit_standardized_mean_difference,
                       "informed_p_value" = round(ma_adv$informed_p_value,2))

 scen$experiment <- factor(scen$experiment, levels = scen$experiment)
scen$color <- ifelse(scen$lower_limit_standardized_mean_difference <= 0 & scen$upper_limit_standardized_mean_difference >= 0, "red", "darkgreen")
 # crosses_0_ind <- with(scen, scen$lower_limit_standardized_mean_difference <= 0 & scen$upper_limit_standardized_mean_difference >= 0)
 # mycolor_ind <- ifelse(crosses_0_ind == T, "red", "green")

 p_mid <-  ggplot2::ggplot(data = scen, ggplot2::aes(y = scen$experiment)) +
  ggplot2::theme_classic() +
   ggplot2::geom_point(ggplot2::aes(x=scen$standardized_mean_difference), shape=15, size= 3, color = scen$color) +
   ggplot2::geom_linerange(ggplot2::aes(xmin=scen$lower_limit_standardized_mean_difference,
                      xmax=scen$upper_limit_standardized_mean_difference) , color = scen$color) +
   ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
       ggplot2::labs(x="Standardized uplift", y="Experiment") +
   ggplot2::coord_cartesian(ylim=c(1,length(scen$experiment) + 1) , xlim = c(-1, 1))  +
   ggplot2::annotate("text", x = -0.5, y = length(scen$experiment) + 1, label = "Variant worse", hjust = 0.5) +
   ggplot2::annotate("text", x = 0.5, y = length(scen$experiment) + 1, label = "Variant better", hjust = 0.5) +
    # scale_x_discrete(limits = scen$experiment)+
   ggplot2::theme(axis.line.y = ggplot2::element_blank(),
         axis.ticks.y= ggplot2::element_blank(),
         axis.text.y= ggplot2::element_blank(),
         axis.title.y= ggplot2::element_blank())

 scen$sme_ci <- paste0(scen$standardized_mean_difference, " (", round(scen$lower_limit_standardized_mean_difference,2), ", " ,round(scen$upper_limit_standardized_mean_difference, 2), ")")

 headings <- data.table::data.table("experiment" = "Platform", "sme_ci" = "Uplift", "informed_p_value" = "Informed_p")

scenarios_p <- rbind(scen, headings, fill = T)

 p_left <- ggplot2::ggplot(data=scenarios_p, ggplot2::aes(y = experiment)) +
   ggplot2::geom_text(ggplot2::aes(x = 0, label = experiment), hjust = 0, fontface = "bold") +
   ggplot2::geom_text(ggplot2::aes(x = 1, label = sme_ci),
     hjust = 0) +
   ggplot2::theme_void() +
   #scale_x_discrete(limits = scenarios_p$experiment)+
  ggplot2::coord_cartesian(xlim = c(-3, 3))


p_right <- ggplot2::ggplot(data = scenarios_p) +
  ggplot2::geom_text(ggplot2::aes(x = 0, y = experiment, label = informed_p_value), hjust = 0)+
  # fontface = ifelse(res_plot$p.value == "p_value", "bold", "plain")) +
  ggplot2::theme_void()

scensum <- ABSynthesis_summary(successes_base, visitors_base, successes_variant, visitors_variant)
 df <- data.frame(x = c(scensum$lower_limit_summary_standardized_mean_difference , scensum$summary_standardized_mean_difference,
                        scensum$upper_limit_summary_standardized_mean_difference, scensum$summary_standardized_mean_difference),
                  y = c(0, 1, 0, -1))
 crosses_0 <- with(scensum, scensum$lower_limit_summary_standardized_mean_difference <= 0 & scensum$upper_limit_summary_standardized_mean_difference >= 0)
 mycolor <- ifelse(crosses_0 == T, "red", "darkgreen")

 p_bottom <- ggplot2::ggplot(df) +
     ggplot2::geom_polygon(ggplot2::aes(x = x, y = y), fill= mycolor) +
     ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
     ggplot2::geom_vline(xintercept = scensum$weighted_standardized_mean_difference) +
     ggplot2::theme_void() +
     ggplot2::coord_cartesian(xlim = c(-1, 1))

 p_bottom_left <- ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "Summary"), hjust = 0, fontface = "bold") + ggplot2::theme_void()

 p_bottom_right <- ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = scensum$summary_p_value), hjust = 0, fontface = "bold") + ggplot2::theme_void()


 layout <- c(
   patchwork::area(t = 0, l = 0, b = 30, r = 3),
   patchwork::area(t = 1, l = 4, b = 30, r = 9),
   patchwork::area(t = 0, l = 9, b = 30, r = 11),
   patchwork::area(t = 31, l = 4, b = 32, r = 9),
   patchwork::area(t = 31, l = 0, b = 32, r = 3),
   patchwork::area(t = 31, l =9, b = 32, r = 11))
 v <- p_left + p_mid + p_right + p_bottom + p_bottom_left + p_bottom_right + patchwork::plot_layout(design = layout)

 print(v)
 }
