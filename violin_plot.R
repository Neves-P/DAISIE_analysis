#' Title
#'
#' @param ont_list
#' @param no_ont_list
#' @param var
#' @param na_rm
#'
#' @return
#' @export
#'
#' @examples
create_plotting_df <- function(ont_list, no_ont_list, var, na_rm = TRUE) {

  if (na_rm) {
    ont_list <- na.omit(ont_list)
    no_ont_list <- na.omit(no_ont_list)
  }

  ontogeny <- factor(c(
    rep("island_ontogeny", length(ont_list[[var]])),
    rep("no_ontogeny", length(no_ont_list[[var]]))),
    levels = c("no_ontogeny", "island_ontogeny"), ordered = TRUE)

  var_df <- data.frame(
    var = c(ont_list[[var]], no_ont_list[[var]]),
    ontogeny = ontogeny)
  return(var_df)
}


#' Title
#'
#' @param ont_list
#' @param no_ont_list
#' @param var
#' @param sumstats_0
#' @param sumstats
#' @param statistic
#' @param log_transform
#' @param na_rm
#'
#' @return
#' @export
#'
#' @examples
plot_violin <- function(ont_list,
                        no_ont_list,
                        ont_pars,
                        no_ont_pars,
                        var,
                        statistic = "median",
                        log_transform = TRUE,
                        na_rm = TRUE,
                        lambda_minus_mu = FALSE,
                        export = FALSE,
                        file_type = NA) {
  var_dataframe <- create_plotting_df(
    ont_list = ont_list,
    no_ont_list = no_ont_list,
    var = var,
    na_rm = na_rm
  )


  sumstats <- DAISIE::DAISIE_calc_sumstats_pcrates(
    pars = c(ont_pars[4], ont_pars[5], ont_pars[6], ont_pars[7], ont_pars[8]),
    totaltime = 6,
    Apars = DAISIE::create_area_params(
      ont_pars[12],
      ont_pars[13],
      ont_pars[14],
      ont_pars[15]
    ),
    Epars = c(ont_pars[10], ont_pars[11]),
    island_ontogeny = 2
  )
  sumstats_0 <- get_estimate_sumstats(no_ont_list)

  if (!require(ggplot2)) {install.packages("ggplot2")}

  legend_name <- switch(var,
                        "lambda_c" = "cladogenesis rate",
                        "mu" = "extinction",
                        "K" = "carrying capacity",
                        "gamma" = "immigration rate",
                        "lambda_a" = "anagenesis rate"
  )

  if (var == "K" || var == "lambda_a") {
    if (var == "K" && log_transform) {
      sumstat_0 <- log(no_ont_pars[6])
      sumstat <- log(ont_pars[12] * ont_pars[6])
      var_dataframe$var <- log(var_dataframe$var)
    } else if (var == "lambda_a" && log_transform) {
      sumstat_0 <- log(no_ont_pars[8])
      sumstat <- log(ont_pars[7])
      var_dataframe$var <- log(var_dataframe$var)
    }

  } else if (statistic == "median") {
    prefix <- "med_"
    if (log_transform) {
      sumstat_0 <- log(sumstats_0[["medians"]])
      sumstat <- log(sumstats[["medians"]])
      var_dataframe$var <- log(var_dataframe$var)
    } else {
      sumstat_0 <- sumstats_0[["medians"]]
      sumstat <- sumstats["medians"]
    }
  } else if (statistic == "mean") {
    prefix <- "mean_"
    if (log_transform) {
      sumstat_0 <- log(sumstats_0[["means"]])
      sumstat <- log(sumstats[["means"]])
      var_dataframe <- log(var_dataframe)
    } else {
      sumstat_0 <- sumstats_0[["means"]]
      sumstat <- sumstats[["means"]]
    }
  }




  if (var == "K" || var == "lambda_a") {
    hlines_df <- data.frame(
      ontogeny = c("island_ontogeny", "no_ontogeny"),
      hline = c(
        sumstat,
        sumstat_0)
    )
  } else {
    hlines_df <- data.frame(
      ontogeny = c("island_ontogeny", "no_ontogeny"),
      hline = c(
        sumstat[[paste0(prefix, var)]],
        no_ont_pars[[5]])
    )
  }


  if (!lambda_minus_mu) {

    ggplot(data = var_dataframe, aes(x = ontogeny, y = var, fill = ontogeny)) +
      geom_violin(trim = FALSE) +
      scale_fill_manual(values = c("darkgreen", "red4")) +
      ggtitle(paste0("DAISIE ", legend_name," estimates in ontogeny \nand null-ontogeny scenarios"))  +
      ylab(if (log_transform) {paste0(legend_name,  " (log transformed)")} else {legend_name}) +
      xlab(element_blank()) +
      theme(
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        panel.background = element_blank()
      ) +
      scale_x_discrete(labels = c(
        "island_ontogeny" = "Island ontogeny",
        "no_ontogeny" = "Null-ontogeny")) +
      geom_errorbar( # Add horizontal bars in set postion (specify in hlines_df)
        data = hlines_df,
        aes(y = NULL, ymax = hline, ymin = hline),
        color = "orange2", size = 1.1
      )
  } else {
    lambda_c_df <- create_plotting_df(
      ont_list = ont_list,
      no_ont_list = no_ont_list,
      var = "lambda_c",
      na_rm = na_rm
    )
    mu_df <- create_plotting_df(
      ont_list = ont_list,
      no_ont_list = no_ont_list,
      var = "mu",
      na_rm = na_rm
    )

    lambda_minus_mu_ont_df <- data.frame(
      var = lambda_c_df$var - mu_df$var, ontogeny = lambda_c_df$ontogeny)

    ggplot(data = lambda_minus_mu_ont_df, aes(x = ontogeny, y = log(var), fill = ontogeny)) +
      geom_violin(trim = FALSE) +
      scale_fill_manual(values = c("darkgreen", "red4")) +
      ggtitle(paste0("DAISIE ", legend_name," estimates in ontogeny \nand null-ontogeny scenarios"))  +
      ylab(if (log_transform) {paste0(legend_name,  " (log transformed)")} else {legend_name}) +
      xlab(element_blank()) +
      theme(
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        panel.background = element_blank()
      ) +
      scale_x_discrete(labels = c(
        "island_ontogeny" = "Island ontogeny",
        "no_ontogeny" = "Null-ontogeny")) +
      geom_errorbar( # Add horizontal bars in set postion (specify in hlines_df)
        data = hlines_df,
        aes(y = NULL, ymax = hline, ymin = hline),
        color = "orange2", size = 1.1
      )
  }


  if (export) {

    if (!require(export)) {install.packages("export")}

    # Export to pdf
    if (is.na(file_type)) {
      is.na(file_type) = stop("File type not specified\n")
    } else if (file_type == "ppt") {
      "ppt" = export::graph2ppt(height = 4, width = 4, file = paste0("violin_plot_", var, ".ppt"))
    } else if (file_type == "png") {
      "png" = export::graph2png(height = 4, width = 4, file = paste0("violin_plot_", var, ".png"))
    }
  }
  Sys.sleep(20)
}
