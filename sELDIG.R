# sELDIG simulations


# CS
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

# IW
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
  replicates = 100,
  complete_analysis = FALSE,
  branch = "@master",
  divdepmodel = 2
)

test_ntips <- DAISIE_sim(10, 1000, c(0.9, 0.4, Inf, 0.0005, 0.4), 100)


# Read sims
tree_directory <- "sELDIG"
for (n_sim in seq_along(list.files(tree_directory))) {
  
  load(file.path(tree_directory, list.files(tree_directory))[n_sim])
  if (args[4] == 0.9) {
    lac <- "1"
  } else if (args[4] == 1) {
    lac <- "2"
  } else if (args[4] == 1.3) {
    lac <- "3"
  }
  
  if (args[5] == 0.3) {
    mu <- "1"
  } else if (args[5] == 0.4) {
    mu <- "2"
  } else if (args[5] == 0.6) {
    mu <- "3"
  }
  
  if (args[6] == 5) {
    k <- "1"
  } else if (args[6] == 15) {
    k <- "2"
  } else if (args[6] == Inf) {
    k <- "3"
  }
  
  if (args[7] == 0.0001) {
    gam <- "1"
  } else if (args[7] == 0.0005) {
    gam <- "2"
  } else if (args[7] == 0.001) {
    gam <- "3"
  }
  
  if (args[8] == 0.2) {
    laa <- "1"
  } else if (args[8] == 0.4) {
    laa <- "2"
  } else if (args[8] == 0.6) {
    laa <- "3"
  }
  
  tree_name <- paste0("etienne", lac, mu, k, gam, laa, ".tre")
  
  

  tree <- DDD::brts2phylo(out[[1]][[2]]$branching_times)
  ape::write.tree(phy = tree,file = paste0("newick_trees/", tree_name))    
  
}
DAISIE::DAISIE_plot_(out[[1]])

