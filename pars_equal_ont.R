library(ggplot2)

setwd("G:/My Drive/PhD/Projects/DAISIE/DAISIE_results/results/pars_equal/FORTRAN")

files <- list.files()
ont_filenames <- files[12:length(files)]


for (i in seq_along(ont_filenames)) {
  load(ont_filenames[i])
  assign(tools::file_path_sans_ext(ont_filenames[i]), ontogeny_CS_pars_equal_ML)
}
ontogeny_CS_pars_equal_ML_11 <- ontogeny_CS_pars_equal_ML_11[1]
setwd("G:/My Drive/PhD/Projects/DAISIE/DAISIE_results/DAISIE_analysis")



ontogeny_CS_df <- data.frame()
for (i in 1:11) {
  ontogeny_CS_df <- rbind(ontogeny_CS_df, do.call(rbind.data.frame, get(paste0("ontogeny_CS_pars_equal_ML_", i))))
}
nrow(ontogeny_CS_df)
nrow(unique(ontogeny_CS_df))
final_ont <- na.omit(ontogeny_CS_df)
final_ont <- final_ont[is.finite(final_ont$lambda_c) &
                               is.finite(final_ont$lambda_a) & is.finite(final_ont$mu) & 
                               is.finite(final_ont$K) & is.finite(final_ont$gamma),]
nrow(final_ont)
head(final_ont)
#### MU ####
tail(final_ont$mu)
mu_plot_ontogeny <- ggplot(data = final_ont, aes(x = final_ont$mu)) + 
  geom_histogram(fill = "deepskyblue") +
  geom_vline(xintercept = 2.2, color = "orange2", size = 1.2) +
  ggtitle("Estimates of µ \nwith island ontogeny") +
  xlab("µ") + ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )

ont_pars_equal_median <- median(final_ont$mu, na.rm = T)
ont_pars_equal_sd <- sd(final_ont$mu, na.rm = T)
