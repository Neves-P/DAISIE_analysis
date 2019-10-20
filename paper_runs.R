# 20/10 Post R&O meeting runs

library(utilSIE)
library(DAISIE)

# Local tests

pilot_study_parameters


create_daisie_params(
  time = pilot_study_parameters$time[1],
  M = 1000,
  pars = c(
    pilot_study_parameters$lac[1],
    pilot_study_parameters$mu[1],
    pilot_study_parameters$K[1],
    pilot_study_parameters$gam[1],
    pilot_study_parameters$laa[1]
  ),
  replicates = 1
)

pars = c(
  pilot_study_parameters$lac[1],
  pilot_study_parameters$mu[1],
  pilot_study_parameters$K[1],
  pilot_study_parameters$gam[1],
  pilot_study_parameters$laa[1]
)
DAISIE_sim(time = time, M = M, pars = pars, replicates = 10, divdepmodel = "CS", plot_sims = FALSE, island_ontogeny = "beta", Apars = )

# Cluster runs


data("pilot_study_parameters")
project_name = utilSIE::get_project_name()
account = utilSIE::get_available_accounts()
download_files = TRUE
partition = "gelifes"
time = pilot_study_parameters$time[1]
M = pilot_study_parameters$M[1]
lac = pilot_study_parameters$lac[1]
mu = pilot_study_parameters$mu[1]
K = pilot_study_parameters$K[1]
gam = pilot_study_parameters$gam[1]
laa = pilot_study_parameters$laa[1]
divdepmodel = 1
island_ontogeny = "beta"
replicates = pilot_study_parameters$replicates[1]
mu_min = pilot_study_parameters$mu_min[1]
mu_max = pilot_study_parameters$mu_max[1]
Amax = pilot_study_parameters$Amax[1]
Apeak = pilot_study_parameters$Apeak[1]
Asharpness = pilot_study_parameters$Asharpness[1]
Atotalage = pilot_study_parameters$Atotalage[1]
complete_analysis = FALSE
branch = "@develop"
force = TRUE


execute_next_setup(
  project_name = project_name,
  account = account,
  download_files = download_files,
  partition = partition,
  time = time,
  M = M,
  lac = lac,
  mu = mu,
  K = K,
  gam = gam,
  laa = laa,
  divdepmodel = divdepmodel,
  island_ontogeny = island_ontogeny,
  replicates = replicates,
  mu_min = mu_min,
  mu_max = mu_max,
  Amax = Amax,
  Apeak = Apeak,
  Asharpness = Asharpness,
  Atotalage = Atotalage,
  complete_analysis = complete_analysis,
  branch = branch,
  force = force
)
