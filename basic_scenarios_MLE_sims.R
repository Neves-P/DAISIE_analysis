download_results()

res_low_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.000740740740740741-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_med_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.00222222222222222-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_high_clado <- load_DAISIE_results(sim_file_name = "sim-DAISIE-1-6-1000-7.48223e-05-1-0.00740740740740741-0.001-1-1000-0.1-0.11-13500-0.1-1-9.RData", ontogeny = TRUE)
res_low_clado[which(res_low_clado$lambda_c < 10)]

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
