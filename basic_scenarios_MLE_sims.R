download_results()

res_low_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.000740740740740741-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_med_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.00222222222222222-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_high_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.00740740740740741-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_low_clado[which(res_low_clado$lambda_c < 10)]


# Med
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_med_clado$lambda_c[which(res_med_clado$lambda_c < 10)][i],
    mu = res_med_clado$mu[which(res_med_clado$lambda_c < 10)][i],
    K = res_med_clado$K[which(res_med_clado$lambda_c < 10)][i],
    gam = res_med_clado$gamma[which(res_med_clado$lambda_c < 10)][i],
    laa = res_med_clado$lambda_a[which(res_med_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 100,
    complete_analysis = FALSE
  )
}

# High
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_high_clado$lambda_c[which(res_high_clado$lambda_c < 10)][i],
    mu = res_high_clado$mu[which(res_high_clado$lambda_c < 10)][i],
    K = res_high_clado$K[which(res_high_clado$lambda_c < 10)][i],
    gam = res_high_clado$gamma[which(res_high_clado$lambda_c < 10)][i],
    laa = res_high_clado$lambda_a[which(res_high_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 100,
    complete_analysis = FALSE
  )
}

# Low
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 100,
    complete_analysis = FALSE
  )
}

##### 1 replicate per estimate

# Med
for (i in 1:1000) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_med_clado$lambda_c[i],
    mu = res_med_clado$mu[i],
    K = res_med_clado$K[i],
    gam = res_med_clado$gamma[i],
    laa = res_med_clado$lambda_a[i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE
  )
}

# High
for (i in 1:1000) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_high_clado$lambda_c[which(res_high_clado$lambda_c < 10)][i],
    mu = res_high_clado$mu[which(res_high_clado$lambda_c < 10)][i],
    K = res_high_clado$K[which(res_high_clado$lambda_c < 10)][i],
    gam = res_high_clado$gamma[which(res_high_clado$lambda_c < 10)][i],
    laa = res_high_clado$lambda_a[which(res_high_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE
  )
}

# Low
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE
  )
}

#### Higher mu and K attempts ####



# High mu
for (i in 1:1000) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE,
    partition = "short"
  )
}

# Higher mu
for (i in 1:1000) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE,
    partition = "short"
  )
}

# High K
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE,
    partition = "short"
  )
}

# High K
for (i in 1:10) {
  execute_next_setup(
    time = 6,
    M = 1000,
    lac = res_low_clado$lambda_c[which(res_low_clado$lambda_c < 10)][i],
    mu = res_low_clado$mu[which(res_low_clado$lambda_c < 10)][i],
    K = res_low_clado$K[which(res_low_clado$lambda_c < 10)][i],
    gam = res_low_clado$gamma[which(res_low_clado$lambda_c < 10)][i],
    laa = res_low_clado$lambda_a[which(res_low_clado$lambda_c < 10)][i],
    island_ontogeny = FALSE ,
    branch = "@develop",
    replicates = 10,
    complete_analysis = FALSE
  )
}


# TEST
out1 <- DAISIE::DAISIE_sim(time = 6, M = 1000, pars = c(unique(new_experiments$lac), 1, new_experiments$K[1], unique(new_experiments$gam), unique(new_experiments$laa)), replicates = 10, island_ontogeny = "beta", Apars = DAISIE::create_area_params(unique(new_experiments$Amax), unique(new_experiments$Apeak), unique(new_experiments$Asharpness), unique(new_experiments$Atotalage)), Epars = c(unique(new_experiments$mu_min), 15))
