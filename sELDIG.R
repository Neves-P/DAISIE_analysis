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
