#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_nLTT_results <- function(generating_simuluations = "sim-DAISIE-1-7-1000-7.48223e-05-1-0.05-0.001-1-1000-0.1-15-13500-0.1-1-9.RData",
                              scenario = "long_time") {

  project_name <- get_project_name()
  project_folder <- get_project_folder(project_name)
  data_file_folder <- file.path(project_folder, scenario)
  generating_simulations <- load_DAISIE_data(
    "sim-DAISIE-1-7-1000-7.48223e-05-1-0.05-0.001-1-1000-0.1-15-13500-0.1-1-9.RData",
    scenario = scenario
  )

  resulting_simulations <- load_DAISIE_data(scenario = file.path("1000rep", scenario))
  DDD::brts2phylo(resulting_simulations[[1]][[1]][[1]][[3]]$branching_times)


}

