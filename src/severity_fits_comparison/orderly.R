orderly2::orderly_parameters(short_run = TRUE, deterministic = TRUE)

regions <- sircovid::regions("england")
variants <- c("Wildtype", "Alpha", "Delta")

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
    quote(latest(parameter:data_changed == environment:d && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:percent_removed == 100)),
    c("inputs/${d}/combined.rds" = "outputs/combined.rds"))
}

for (pct in seq(10, 90, 10)) {
  orderly2::orderly_dependency(
    "severity_fits_combined",
    quote(latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:percent_removed == environment:pct)),
    c("inputs/deaths_comm_${pct}/combined.rds" = "outputs/combined.rds"))
}

#artefact====

orderly2::orderly_artefact("Time series heatmaps",
                           c(paste0("figs/all_time_series_heatmap_HFR_", c(regions, "england"), ".png"),
                             paste0("figs/all_time_series_heatmap_IFR_", c(regions, "england"), ".png"),
                             paste0("figs/all_time_series_heatmap_IHR_", c(regions, "england"), ".png"),
                             paste0("figs/all_time_series_heatmap_Rt_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_time_series_heatmap_HFR_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_time_series_heatmap_IFR_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_time_series_heatmap_IHR_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_time_series_heatmap_Rt_", c(regions, "england"), ".png")))

orderly2::orderly_artefact("Forest plots",
                           c(paste0("figs/all_forest_plot_HFR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_HFR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_HFR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IFR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IFR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IFR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IHR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IHR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_IHR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_R0_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_R0_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/all_forest_plot_R0_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_HFR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_HFR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_HFR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IFR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IFR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IFR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IHR_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IHR_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_IHR_Delta_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_R0_Wildtype_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_R0_Alpha_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_forest_plot_R0_Delta_", c(regions, "england"), ".png")))

orderly2::orderly_artefact("Heatmaps",
                           c(paste0("figs/all_metrics_heatmap_", c(regions, "england"), ".png"),
                             paste0("figs/deaths_comm_metrics_heatmap_", c(regions, "england"), ".png"),
                             "figs/all_parameters_heatmap.pdf",
                             "figs/deaths_comm_parameters_heatmap.pdf"))

orderly2::orderly_artefact("Convergence diagnostics",
                           "convergence_diagnostics.html")

#load data====

dat <- load_combined("inputs", data_changed, c(original = "reference"))
deaths_comm_names <- c(paste0("deaths_comm_", seq(10, 90, 10)), "deaths_comm")
dat2 <- load_combined("inputs", c("original", deaths_comm_names),
                      c(original = "reference", deaths_comm = "deaths_comm_100"))

#====

dir.create("figs", FALSE, TRUE)

for (r in c(regions, "england")) {
  message(sprintf("Creating figures for %s", region_to_title(r)))
  
  ## All datastreams
  
  # Time series heatmaps ====

  write_png(paste0("figs/all_time_series_heatmap_ifr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ifr, r, "effective IFR"))

  write_png(paste0("figs/all_time_series_heatmap_ihr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ihr, r, "effective IHR"))

  write_png(paste0("figs/all_time_series_heatmap_hfr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$hfr, r, "effective HFR"))

  write_png(paste0("figs/all_time_series_heatmap_Rt_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$Rt_eff, r, "effective Rt"))

  # Forest plots ====
  
  for (v in variants) {
    write_png(paste0("figs/all_forest_plot_R0_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$R0, r, v, "R0"))

    write_png(paste0("figs/all_forest_plot_IFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_ifr, r, v, "Intrinsic IFR"))

    write_png(paste0("figs/all_forest_plot_IHR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_ihr, r, v, "Intrinsic IHR"))

    write_png(paste0("figs/all_forest_plot_HFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat$intrinsic_hfr, r, v, "Intrinsic HFR"))
  }
  
  # Metrics heatmaps ====

  write_png(paste0("figs/all_metrics_heatmap_", r, ".png"),
            width = 4000, height = 3000, res = 400,
            plot_metrics_heatmap(dat, r))
  
  
  ## deaths_comm
  
  # Time series heatmaps ====
  write_png(paste0("figs/deaths_comm_time_series_heatmap_ifr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat2$ifr, r, "effective IFR"))
  
  write_png(paste0("figs/deaths_comm_time_series_heatmap_ihr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat2$ihr, r, "effective IHR"))
  
  write_png(paste0("figs/deaths_comm_time_series_heatmap_hfr_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat2$hfr, r, "effective HFR"))
  
  write_png(paste0("figs/deaths_comm_time_series_heatmap_Rt_", r, ".png"),
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat2$Rt_eff, r, "effective Rt"))
  
  # Forest plots ====
  for (v in variants) {
    write_png(paste0("figs/deaths_comm_forest_plot_R0_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat2$R0, r, v, "R0", "Community deaths"))
    
    write_png(paste0("figs/deaths_comm_forest_plot_IFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat2$intrinsic_ifr, r, v, "Intrinsic IFR", "Community deaths"))
    
    write_png(paste0("figs/deaths_comm_forest_plot_IHR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat2$intrinsic_ihr, r, v, "Intrinsic IHR", "Community deaths"))
    
    write_png(paste0("figs/deaths_comm_forest_plot_HFR_", v, "_", r, ".png"),
              units = "in", width = 8, height = 4, res = 600,
              plot_forest(dat2$intrinsic_hfr, r, v, "Intrinsic HFR", "Community deaths"))
  }
  
  # Metrics heatmaps ====
  write_png(paste0("figs/deaths_comm_metrics_heatmap_", r, ".png"),
            width = 4000, height = 3000, res = 400,
            plot_metrics_heatmap(dat2, r))
  
}

write_pdf(paste0("figs/all_parameters_heatmap.pdf"),
          width = 11.69, height = 8.27,
          plot_parameters_heatmap(dat, regions))

write_pdf(paste0("figs/deaths_comm_parameters_heatmap.pdf"),
          width = 11.69, height = 8.27,
          plot_parameters_heatmap(dat2, regions))

rmarkdown::render("convergence_diagnostics.Rmd")
