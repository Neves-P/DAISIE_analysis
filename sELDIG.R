# sELDIG simulations



execute_next_setup(
  project_name = get_project_name(),
  max_sims = ,
  account = get_p_number(),
  download_files = FALSE,
  partition = "regular",
  time = 10,
  M = 1000,
  lac = c(0.9, 1, 1.3),
  mu = c(0.3, 0.4, 0.6),
  K = c(5, 15, Inf),
  gam = c(0.0001, 0.0005, 0.001),
  laa = c(0.2, 0.4, 0.6),
  island_ontogeny = "const",
  replicates = 1000,
  complete_analysis = FALSE,
  branch = "@develop"
)

test_ntips <- DAISIE_sim(10, 1000, c(0.9, 0.4, Inf, 0.0005, 0.4), 100)


# Read sims
tree_directory <- "sELDIG"
for (n_sim in seq_along(list.files(tree_directory))) {
  sim_name <- list.files(tree_directory)[n_sim]
  sim_name <- gsub("-", "", sim_name)
  lac_change < gsub("0.9", "1", sim_name)
  lac_change < gsub("1", "2", lac_change)
  lac_change < gsub("1.3", "3", lac_change)

  mu_change < gsub("0.3", "1", lac_change)
  mu_change < gsub("0.4", "2", mu_change)
  mu_change < gsub("0.6", "3", mu_change)

  k_change < gsub("5", "1", mu_change)
  k_change < gsub("15", "2", k_change)
  k_change < gsub("Inf", "3", k_change)

  gam_change < gsub("0.0001", "1", k_change)
  gam_change < gsub("0.0005", "2", gam_change)
  gam_change < gsub("0.001", "3", gam_change)

  laa_change < gsub("0.2", "1", gam_change)
  laa_change < gsub("0.4", "2", laa_change)
  laa_change < gsub("0.6", "3", laa_change)

  
  
  
  
  load(file.path(tree_directory, list.files(tree_directory))[n_sim])

}
DAISIE::DAISIE_plot_(out[[1]])


for (i in seq_along(out)) {
  for (k in 2:length(out[[i]])) {
    tree <- DDD::brts2phylo(out[[i]][[k]]$branching_times)
  }
}
