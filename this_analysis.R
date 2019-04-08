pars_22 <- args_22[4:8]
Epars_22 <- args_22[10:11]
totaltime <- args_22[2]
Apars_22 <- DAISIE::create_area_params(
  args_22[12],
  args_22[13],
  args_22[14],
  args_22[15]
)
library(DAISIE)
DAISIE_calc_sumstats_pcrates(
  pars = pars_22,
  totaltime = totaltime,
  Apars = Apars_22,
  Epars = Epars_22,
  island_ontogeny = 2
)

pars_23 <- args_23[4:8]
Epars_23 <- args_23[10:11]
totaltime <- args_23[2]
Apars_23 <- DAISIE::create_area_params(
  args_23[12],
  args_23[13],
  args_23[14],
  args_23[15]
)
DAISIE_calc_sumstats_pcrates(
  pars = pars_23,
  totaltime = totaltime,
  Apars = Apars_23,
  Epars = Epars_23,
  island_ontogeny = 2
)

load_DAISIE_results <- function(sim_file_name) {


  project_name <- get_project_name()
  project_folder <- get_project_folder(project_name)
  platform <- .Platform$OS.type

  if (platform == "windows") {
    results_folder <- local_data_folder <- file.path(project_folder, "results")
    testit::assert(dir.exists(data_folder))
  } else {
    results_folder <- file.path(get_project_name(), "results")
  }

  result_files <- list.files(results_folder)

  match_sims_to_load <- paste0("res-", substring(sim_file_name, 5))
  files_to_load <-
    result_files[grepl(tools::file_path_sans_ext(match_sims_to_load), result_files)]
  final_list <- list()
  for (file in seq_along(files_to_load)) {
    load(file.path(results_folder, files_to_load[file]))
    assign(paste("result", file, sep = "_"), out_results) #nolint
    final_list <- rbind(final_list, do.call(rbind.data.frame, get(paste("result", file, sep = "_"))))
  }
  return(final_list)
}

get_estimate_sumstats <- function(results) {
  med_lambda_c <- median(results$lambda_c, na.rm = T)
  med_mu <- median(results$mu, na.rm = T)
  med_K <- median(results$K, na.rm = T)
  med_gamma <- median(results$gamma, na.rm = T)
  med_lambda_a <- median(results$lambda_a, na.rm = T)

  mean_lambda_c <- mean(results$lambda_c, na.rm = T)
  mean_mu <- mean(results$mu, na.rm = T)
  mean_K <- mean(results$K, na.rm = T)
  mean_gamma <- mean(results$gamma, na.rm = T)
  mean_lambda_a <- mean(results$lambda_a, na.rm = T)

  out_list <- list(
    medians = c(
      med_lambda_c = med_lambda_c,
      med_mu = med_mu,
      med_K = med_K,
      med_gamma = med_gamma,
      med_lambda_a = med_lambda_a
      ),
    means = c(
      mean_lambda_c = mean_lambda_c,
      mean_mu = mean_mu,
      mean_K = mean_K,
      mean_gamma = mean_gamma,
      mean_lambda_a = mean_lambda_a
    )
  )
  return(out_list)
}


ontogeny_CS_df <- data.frame()
for (i in 1:11) {
  ontogeny_CS_df <- rbind(ontogeny_CS_df, do.call(rbind.data.frame, get(paste0("ontogeny_CS_pars_equal_ML_", i))))
}






