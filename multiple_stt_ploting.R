load("D:/Projects/DAISIE/data/sim-DAISIE-1-6-1000-7.48223e-05-1-0.00222222222222222-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData")
initial <- out


load("D:/Projects/DAISIE/data/sim-DAISIE-1-6-1000-6.95551856253337-1.04545793795116-3.4223944509655-0.000769762889359161-13610.7669309479-100.RData")
final <- out

parsed_initial <- DAISIE_convert_to_classic_plot(initial)


parsed_final <- DAISIE_convert_to_classic_plot(final)


parsed_final_list <- list()
for (i in seq_along(final)) {
  parsed_final_list[i] <- DAISIE_convert_to_classic_plot(final[i])
}

rm(final_lists)

DAISIE_plot_comparison_stts(
  time = 6,
  plot_lists_simulations = parsed_initial,
  plot_lists_simulations_MLE = parsed_final,
  type = "all_species",
  kind_of_plot = "shade"
)

DAISIE_plot_comparison_stts(
  time = 6,
  plot_lists_simulations = parsed_initial,
  plot_lists_simulations_MLE = parsed_final_list,
  type = "all_species",
  
)

