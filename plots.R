

# Plot rates of the runs
library(gridExtra)
library(DAISIE)

pars <- c(time = args[2], lac = args[4], K = args[6], gam = args[7], laa = args[8])
Apars <- create_area_params(max_area = args[12], proportional_peak_t = args[13], peak_sharpness = args[14], total_island_age = args[15])
Epars <- c(args[10], args[11])

area_plot <- DAISIE_plot_area(
  totaltime = pars['time'],
  Apars = Apars,
  island_ontogeny = "beta",
  resolution = 0.001
)

clado_plot <- DAISIE_plot_cladogenesis(
  totaltime = pars['time'],
  K = pars['K'],
  Apars = Apars,
  lac = pars['lac'],
  island_ontogeny = "beta",
  removed_timepoints = 1,
  resolution = 0.001
)

ext_plot <- DAISIE_plot_extinction(
  totaltime = pars['time'],
  K = pars['K'],
  Apars = Apars,
  Epars = Epars,
  island_ontogeny = "beta",
  removed_timepoints = 1,
  resolution = 0.001)

immig_plot <- DAISIE_plot_immigration(
  totaltime = pars['time'],
  K = pars['K'],
  Apars = Apars,
  gam = pars['gam'],
  mainland_n = args[3],
  island_ontogeny = "beta",
  removed_timepoints = 1,
  resolution = 0.001
)

sims_plot <- DAISIE_plot_sims(out)

grid.arrange(area_plot, clado_plot, immig_plot, ext_plot, nrow = 2)
