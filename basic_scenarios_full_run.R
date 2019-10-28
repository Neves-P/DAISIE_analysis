

# 3 pilot job submission



# save(new_experiments, file = "pilot_study_pars.RData")
if (!exists("new_experiments")) {load("pilot_study_pars.RData")}

execute_next_setup(
  project_name = get_project_name(),
  account = get_p_number(),
  download_files = FALSE,
  partition = "gelifes",
  time = 6,
  M = 1000,
  lac = unique(new_experiments$lac),
  mu = unique(new_experiments$mu),
  K = new_experiments$K,
  gam = unique(new_experiments$gam),
  laa = unique(new_experiments$laa),
  island_ontogeny = "beta",
  replicates = 1000,
  mu_min = unique(new_experiments$mu_min),
  mu_max = unique(new_experiments$mu_max),
  Amax = unique(new_experiments$Amax),
  Apeak = unique(new_experiments$Apeak),
  Asharpness = unique(new_experiments$Asharpness),
  Atotalage = unique(new_experiments$Atotalage),
  branch = "@develop",
  max_sims = 100,
  complete_analysis = TRUE
)

# Higher mu
execute_next_setup(
  project_name = get_project_name(),
  account = get_p_number(),
  download_files = FALSE,
  partition = "gelifes",
  time = 6,
  M = 1000,
  lac = unique(new_experiments$lac),
  mu = unique(new_experiments$mu),
  K = new_experiments$K,
  gam = unique(new_experiments$gam),
  laa = unique(new_experiments$laa),
  island_ontogeny = "beta",
  replicates = 1000,
  mu_min = unique(new_experiments$mu_min),
  mu_max = 15, # This is absurd
  Amax = unique(new_experiments$Amax),
  Apeak = unique(new_experiments$Apeak),
  Asharpness = unique(new_experiments$Asharpness),
  Atotalage = unique(new_experiments$Atotalage),
  branch = "@develop",
  max_sims = 100,
  complete_analysis = TRUE
)


# Higher K (and mu)
higher_K <- c(0.01, 0.05)
execute_next_setup(
  project_name = get_project_name(),
  account = get_p_number(),
  download_files = FALSE,
  partition = "gelifes",
  time = 7,
  M = 1000,
  lac = unique(new_experiments$lac),
  mu = unique(new_experiments$mu),
  K = higher_K,
  gam = unique(new_experiments$gam),
  laa = unique(new_experiments$laa),
  island_ontogeny = "beta",
  replicates = 1000,
  mu_min = unique(new_experiments$mu_min),
  mu_max = 15, # This is absurd
  Amax = unique(new_experiments$Amax),
  Apeak = unique(new_experiments$Apeak),
  Asharpness = unique(new_experiments$Asharpness),
  Atotalage = unique(new_experiments$Atotalage),
  branch = "@develop",
  max_sims = 100,
  complete_analysis = TRUE
)




#21/10
execute_next_setup(
  project_name = project_name,
  account = account,
  download_files = FALSE,
  partition = partition,
  time = unique(pilot_study_parameters$time),
  M = unique(pilot_study_parameters$M),
  lac = unique(pilot_study_parameters$lac),
  mu = unique(pilot_study_parameters$mu),
  K = unique(pilot_study_parameters$K),
  gam = unique(pilot_study_parameters$gam),
  laa = unique(pilot_study_parameters$laa),
  divdepmodel = divdepmodel,
  island_ontogeny = island_ontogeny,
  replicates = 1,
  mu_min = unique(pilot_study_parameters$mu_min),
  mu_max = unique(pilot_study_parameters$mu_max, 0.2, 5),
  Amax = unique(pilot_study_parameters$Amax),
  Apeak = unique(pilot_study_parameters$Apeak),
  Asharpness = unique(pilot_study_parameters$Asharpness),
  Atotalage = unique(pilot_study_parameters$Atotalage),
  complete_analysis = TRUE,
  branch = branch,
  force = TRUE
)
