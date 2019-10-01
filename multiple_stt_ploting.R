comparison_stt_results <- function(generating_simuluations = "sim-DAISIE-1-7-1000-7.48223e-05-1-0.05-0.001-1-1000-0.1-15-13500-0.1-1-9.RData",
                                   scenario = "long_time",
                                   time = 7,
                                   kind_of_plot = "line") {

  project_name <- get_project_name()
  project_folder <- get_project_folder(project_name)
  data_file_folder <- file.path(project_folder, scenario)
  generating_simulations <<- load_DAISIE_data(
    "sim-DAISIE-1-7-1000-7.48223e-05-1-0.05-0.001-1-1000-0.1-15-13500-0.1-1-9.RData",
    scenario = scenario
  )

  resulting_simulations <<- load_DAISIE_data(scenario = file.path("1000rep", scenario))


  parsed_initial <- DAISIE::DAISIE_convert_to_classic_plot(generating_simulations[[1]]$out)
  parsed_final <- list()


  # for (i in seq_along(resulting_simulations[[1]])) {
  for (i in 1:20) {
    parsed_replicates <- list()
    # for (k in seq_along(resulting_simulations[[1]][[i]])) {
    for (k in seq_along(resulting_simulations[[1]][[i]])) {
      parsed_replicates[[k]] <- DAISIE::DAISIE_convert_to_classic_plot(
        resulting_simulations[[1]][[i]][k]
      )
    }
    parsed_final[[i]] <- parsed_replicates
  }

  # return(parsed_final)

  for (i in seq_along(parsed_final))
  DAISIE::DAISIE_plot_comparison_stts(
    time = time,
    plot_lists_simulations = parsed_initial,
    plot_lists_simulations_MLE = parsed_final[[i]],
    type = "all_species",
    kind_of_plot = kind_of_plot
  )
  export::graph2png(file = "plot/")

}

