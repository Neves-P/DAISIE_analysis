library(ggplot2)

setwd("G:/My Drive/PhD/Projects/DAISIE/DAISIE_results/results/pars_equal/FORTRAN")

files <- list.files()

no_ont_filnames <- files[grep("no_ontogeny",files)]

for (i in seq_along(no_ont_filnames)) {
  load(no_ont_filnames[i])
  assign(tools::file_path_sans_ext(no_ont_filnames[i]), no_ontogeny_CS_pars_equal_ML)
}
no_ontogeny_CS_pars_equal_ML_11 <- no_ontogeny_CS_pars_equal_ML_11[1]
setwd("G:/My Drive/PhD/Projects/DAISIE/DAISIE_results/DAISIE_analysis")



no_ontogeny_CS_df <- data.frame()
for (i in 1:11) {
  no_ontogeny_CS_df <- rbind(no_ontogeny_CS_df, do.call(rbind.data.frame, get(paste0("no_ontogeny_CS_pars_equal_ML_", i))))
}

final_no_ont <- na.omit(no_ontogeny_CS_df)
final_no_ont <- final_no_ont[is.finite(final_no_ont$lambda_c) &
                            is.finite(final_no_ont$lambda_a) & is.finite(final_no_ont$mu) & 
                            is.finite(final_no_ont$K) & is.finite(final_no_ont$gamma),]


#### MU ####
tail(final_no_ont$mu)
mu_plot_no_ontogeny <- ggplot(data = final_no_ont, aes(x = mu)) + 
  geom_histogram(fill = "deepskyblue") +
  geom_vline(xintercept = 2.2, color = "orange2", size = 1.2) +
  ggtitle("Estimates of µ \nwithout island ontogeny") +
  xlab("µ") + ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )

median(final_no_ont$mu, na.rm = T)
sd(final_no_ont$mu, na.rm = T)
