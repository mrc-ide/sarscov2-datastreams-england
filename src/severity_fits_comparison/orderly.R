orderly2::orderly_parameters(short_run = TRUE, deterministic = TRUE)

regions <- sircovid::regions("england")

#library====

library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(data.table)
library(stringr)
library(RColorBrewer)
library(lubridate)
library(png)
library(gtable)
library(grid)
library(scales)
library(forestploter)
library(ragg)
library(rmarkdown)


# source ====
orderly2::orderly_shared_resource(global_util.R = "rtm_inference/util_new.R")
orderly2::orderly_resource("convergence_diagnostics.Rmd")
orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("global_util.R")
source("plot.R")
source("support.R")


#dependency====

data_changed <- c("original", "deaths_hosp", "deaths_comm", "icu", "general",
                  "hosp", "all_admission", "pillar2", "ons", "react", "strain",
                  "sero")

for (d in data_changed) {
  orderly2::orderly_dependency(
    "severity_fits_combined",
    quote(latest(parameter:data_changed == environment:d && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate == 1)),
    c("inputs/${d}/combined.rds" = "outputs/combined.rds"))
}

#artefact====

orderly2::orderly_artefact("Time series heatmaps",
                           c(paste0("figs/time_series_heatmap_HFR_", c(regions, "england"), ".png"),
                             paste0("figs/time_series_heatmap_IFR_", c(regions, "england"), ".png"),
                             paste0("figs/time_series_heatmap_IHR_", c(regions, "england"), ".png"),
                             paste0("figs/time_series_heatmap_Rt_", c(regions, "england"), ".png")))

orderly2::orderly_artefact("Forest plots",
                           c(paste0("figs/forest_plot_HFR_Wildtype", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_HFR_Alpha", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_HFR_Delta", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IFR_Wildtype", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IFR_Alpha", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IFR_Delta", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IHR_Wildtype", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IHR_Alpha", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_IHR_Delta", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_R0_Wildtype", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_R0_Alpha", c(regions, "england"), ".png"),
                             paste0("figs/forest_plot_R0_Delta", c(regions, "england"), ".png")))

orderly2::orderly_artefact("Heatmaps",
                           c(paste0("figs/metrics_heatmap_", c(regions, "england"), ".png"),
                           "figs/parameters_heatmap.pdf"))

orderly2::orderly_artefact("Convergence diagnostics",
                           "convergence_diagnostics.html")

#load data====

dat <- load_combined("inputs", data_changed)

#====

dir.create("figs", FALSE, TRUE)

for (r in c(regions, "england")) {
  message(sprintf("Creating figures for %s", r))
  
  # Time series heatmaps ====

  write_png(paste0("figs/time_series_heatmap_ifr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ifr, r, "effective IFR"))

  write_png(paste0("figs/time_series_heatmap_ihr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ihr, r, "effective IHR"))

  write_png(paste0("figs/time_series_heatmap_hfr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$hfr, r, "effective HFR"))

  write_png(paste0("figs/time_series_heatmap_Rt_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$Rt_eff, r, "effective Rt"))

  variants <- c("Wildtype", "Alpha", "Delta")
  for (v in variants) {
    write_png(paste0("figs/forest_plot_R0_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$R0, r, v, "R0"))

    write_png(paste0("figs/forest_plot_IFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_ifr, r, v, "Intrinsic IFR"))

    write_png(paste0("figs/forest_plot_IHR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_ihr, r, v, "Intrinsic IHR"))

    write_png(paste0("figs/forest_plot_HFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_hfr, r, v, "Intrinsic HFR"))
  }

  write_png(paste0("figs/metrics_heatmap_", r, ".png"),
            width = 4000, height = 3000, res = 400,
            plot_metrics_heatmap(dat, r))
  
}

write_pdf(paste0("figs/parameters_heatmap.pdf"),
          width = 11.69, height = 8.27,
          plot_parameters_heatmap(dat, regions))

rmarkdown::render("convergence_diagnostics.Rmd")
