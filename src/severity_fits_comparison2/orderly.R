orderly2::orderly_parameters(short_run = FALSE, deterministic = TRUE)

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

#dependency====

orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "original" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/0/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0/england_R0.rds"="outputs/R0.rds",
    "inputs/0/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.1)',
  c("inputs/0.1/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.1/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.1/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.1/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.1/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.1/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.1/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.1/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.1/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.1/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.1/england_R0.rds"="outputs/R0.rds",
    "inputs/0.1/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.2)',
  c("inputs/0.2/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.2/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.2/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.2/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.2/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.2/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.2/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.2/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.2/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.2/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.2/england_R0.rds"="outputs/R0.rds",
    "inputs/0.2/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.3)',
  c("inputs/0.3/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.3/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.3/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.3/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.3/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.3/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.3/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.3/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.3/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.3/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.3/england_R0.rds"="outputs/R0.rds",
    "inputs/0.3/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.4)',
  c("inputs/0.4/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.4/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.4/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.4/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.4/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.4/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.4/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.4/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.4/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.4/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.4/england_R0.rds"="outputs/R0.rds",
    "inputs/0.4/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.5)',
  c("inputs/0.5/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.5/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.5/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.5/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.5/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.5/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.5/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.5/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.5/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.5/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.5/england_R0.rds"="outputs/R0.rds",
    "inputs/0.5/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.6)',
  c("inputs/0.6/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.6/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.6/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.6/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.6/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.6/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.6/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.6/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.6/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.6/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.6/england_R0.rds"="outputs/R0.rds",
    "inputs/0.6/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.7)',
  c("inputs/0.7/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.7/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.7/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.7/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.7/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.7/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.7/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.7/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.7/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.7/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.7/england_R0.rds"="outputs/R0.rds",
    "inputs/0.7/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.8)',
  c("inputs/0.8/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.8/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.8/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.8/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.8/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.8/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.8/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.8/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.8/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.8/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.8/england_R0.rds"="outputs/R0.rds",
    "inputs/0.8/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==0.9)',
  c("inputs/0.9/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/0.9/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/0.9/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/0.9/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/0.9/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/0.9/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/0.9/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/0.9/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/0.9/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/0.9/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/0.9/england_R0.rds"="outputs/R0.rds",
    "inputs/0.9/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/1/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/1/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/1/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/1/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/1/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/1/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/1/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/1/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/1/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/1/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/1/england_R0.rds"="outputs/R0.rds",
    "inputs/1/england/fit.rds"="outputs/england_severity.rds"))

#artefact====

#orderly2::orderly_artefact("csv files for storing KL divergence", c("outputs/KL_divergence/pars east_of_england.csv","outputs/KL_divergence/pars london.csv","outputs/KL_divergence/pars midlands.csv","outputs/KL_divergence/pars north_east_and_yorkshire.csv","outputs/KL_divergence/pars north_west.csv","outputs/KL_divergence/pars south_east.csv","outputs/KL_divergence/pars south_west.csv"))
#orderly2::orderly_artefact("heatmap of KL divergence",c("outputs/KL_divergence/KL divergence of parameters in east_of_england.pdf","outputs/KL_divergence/KL divergence of parameters in london.pdf","outputs/KL_divergence/KL divergence of parameters in midlands.pdf","outputs/KL_divergence/KL divergence of parameters in north_east_and_yorkshire.pdf","outputs/KL_divergence/KL divergence of parameters in north_west.pdf","outputs/KL_divergence/KL divergence of parameters in south_east.pdf","outputs/KL_divergence/KL divergence of parameters in south_west.pdf"))
#orderly2::orderly_artefact("ifr",c('outputs/east_of_england ifr distribution.pdf','outputs/london ifr distribution.pdf','outputs/midlands ifr distribution.pdf','outputs/north_east_and_yorkshire ifr distribution.pdf','outputs/north_west ifr distribution.pdf','outputs/south_east ifr distribution.pdf','outputs/south_west ifr distribution.pdf',"outputs/ifr_density_plots.rds","outputs/KL_ifr.rds","outputs/KL_divergence/ifr east_of_england.csv","outputs/KL_divergence/ifr london.csv","outputs/KL_divergence/ifr midlands.csv","outputs/KL_divergence/ifr north_east_and_yorkshire.csv","outputs/KL_divergence/ifr north_west.csv","outputs/KL_divergence/ifr south_east.csv","outputs/KL_divergence/ifr south_west.csv","outputs/ifr.rds"))
orderly2::orderly_artefact("R0",c("outputs/R0.rds",'outputs/KL_R0.rds',"outputs/KL divergence heatmap of R0.png",'outputs/R0_density_plots.rds','outputs/east_of_england R0 distribution.pdf','outputs/london R0 distribution.pdf','outputs/midlands R0 distribution.pdf','outputs/north_east_and_yorkshire R0 distribution.pdf','outputs/north_west R0 distribution.pdf','outputs/south_east R0 distribution.pdf','outputs/south_west R0 distribution.pdf'))
#orderly2::orderly_artefact("Rt",c('outputs/Rt distribution.pdf','outputs/Rt_density_plots.rds','outputs/KL_Rt.rds','outputs/Rt.rds'))
orderly2::orderly_artefact("HFR",c("outputs/HFR.rds",'outputs/KL_HFR.rds','outputs/HFR_density_plots.rds','outputs/east_of_england HFR distribution.pdf','outputs/london HFR distribution.pdf','outputs/midlands HFR distribution.pdf','outputs/north_east_and_yorkshire HFR distribution.pdf','outputs/north_west HFR distribution.pdf','outputs/south_east HFR distribution.pdf','outputs/south_west HFR distribution.pdf'))
orderly2::orderly_artefact("IFR",c("outputs/IFR.rds",'outputs/KL_IFR.rds','outputs/IFR_density_plots.rds','outputs/east_of_england IFR distribution.pdf','outputs/london IFR distribution.pdf','outputs/midlands IFR distribution.pdf','outputs/north_east_and_yorkshire IFR distribution.pdf','outputs/north_west IFR distribution.pdf','outputs/south_east IFR distribution.pdf','outputs/south_west IFR distribution.pdf'))
orderly2::orderly_artefact("IHR",c("outputs/IHR.rds",'outputs/KL_IHR.rds',"outputs/KL divergence heatmap of IHR,IFR,HFR.pdf",'outputs/IHR_density_plots.rds','outputs/east_of_england IHR distribution.pdf','outputs/london IHR distribution.pdf','outputs/midlands IHR distribution.pdf','outputs/north_east_and_yorkshire IHR distribution.pdf','outputs/north_west IHR distribution.pdf','outputs/south_east IHR distribution.pdf','outputs/south_west IHR distribution.pdf'))
orderly2::orderly_artefact("parameters",c("outputs/KL_pars.rds","outputs/pars_density_plots.rds","outputs/pars.rds","outputs/KL divergence heatmap of parameters.pdf",'outputs/east_of_england parameter distribution.pdf','outputs/london parameter distribution.pdf','outputs/midlands parameter distribution.pdf','outputs/north_east_and_yorkshire parameter distribution.pdf','outputs/north_west parameter distribution.pdf','outputs/south_east parameter distribution.pdf','outputs/south_west parameter distribution.pdf'))
orderly2::orderly_artefact("ifr",c("outputs/ifr_continuous.rds","outputs/KL_ifr_continuous.rds"))
orderly2::orderly_artefact("ihr",c("outputs/ihr_continuous.rds","outputs/KL_ihr_continuous.rds"))
orderly2::orderly_artefact("hfr",c("outputs/hfr_continuous.rds","outputs/KL_hfr_continuous.rds"))
orderly2::orderly_artefact("diagnostics","outputs/diagnostics.rds")

#load data====

fit_east_0 <- readRDS("inputs/0/east_of_england/fit.rds")
fit_london_0 <- readRDS("inputs/0/london/fit.rds")
fit_midlands_0 <- readRDS("inputs/0/midlands/fit.rds")
fit_northeast_0 <- readRDS("inputs/0/north_east_and_yorkshire/fit.rds")
fit_northwest_0 <- readRDS("inputs/0/north_west/fit.rds")
fit_southeast_0 <- readRDS("inputs/0/south_east/fit.rds")
fit_southwest_0 <- readRDS("inputs/0/south_west/fit.rds")
fit_england_0 <- readRDS("inputs/0/england/fit.rds")

fit_east_0.1 <- readRDS("inputs/0.1/east_of_england/fit.rds")
fit_london_0.1 <- readRDS("inputs/0.1/london/fit.rds")
fit_midlands_0.1 <- readRDS("inputs/0.1/midlands/fit.rds")
fit_northeast_0.1 <- readRDS("inputs/0.1/north_east_and_yorkshire/fit.rds")
fit_northwest_0.1 <- readRDS("inputs/0.1/north_west/fit.rds")
fit_southeast_0.1 <- readRDS("inputs/0.1/south_east/fit.rds")
fit_southwest_0.1 <- readRDS("inputs/0.1/south_west/fit.rds")
fit_england_0.1 <- readRDS("inputs/0.1/england/fit.rds")

fit_east_0.2 <- readRDS("inputs/0.2/east_of_england/fit.rds")
fit_london_0.2 <- readRDS("inputs/0.2/london/fit.rds")
fit_midlands_0.2 <- readRDS("inputs/0.2/midlands/fit.rds")
fit_northeast_0.2 <- readRDS("inputs/0.2/north_east_and_yorkshire/fit.rds")
fit_northwest_0.2 <- readRDS("inputs/0.2/north_west/fit.rds")
fit_southeast_0.2 <- readRDS("inputs/0.2/south_east/fit.rds")
fit_southwest_0.2 <- readRDS("inputs/0.2/south_west/fit.rds")
fit_england_0.2 <- readRDS("inputs/0.2/england/fit.rds")

fit_east_0.3 <- readRDS("inputs/0.3/east_of_england/fit.rds")
fit_london_0.3 <- readRDS("inputs/0.3/london/fit.rds")
fit_midlands_0.3 <- readRDS("inputs/0.3/midlands/fit.rds")
fit_northeast_0.3 <- readRDS("inputs/0.3/north_east_and_yorkshire/fit.rds")
fit_northwest_0.3 <- readRDS("inputs/0.3/north_west/fit.rds")
fit_southeast_0.3 <- readRDS("inputs/0.3/south_east/fit.rds")
fit_southwest_0.3 <- readRDS("inputs/0.3/south_west/fit.rds")
fit_england_0.3 <- readRDS("inputs/0.3/england/fit.rds")

fit_east_0.4 <- readRDS("inputs/0.4/east_of_england/fit.rds")
fit_london_0.4 <- readRDS("inputs/0.4/london/fit.rds")
fit_midlands_0.4 <- readRDS("inputs/0.4/midlands/fit.rds")
fit_northeast_0.4 <- readRDS("inputs/0.4/north_east_and_yorkshire/fit.rds")
fit_northwest_0.4 <- readRDS("inputs/0.4/north_west/fit.rds")
fit_southeast_0.4 <- readRDS("inputs/0.4/south_east/fit.rds")
fit_southwest_0.4 <- readRDS("inputs/0.4/south_west/fit.rds")
fit_england_0.4 <- readRDS("inputs/0.4/england/fit.rds")

fit_east_0.5 <- readRDS("inputs/0.5/east_of_england/fit.rds")
fit_london_0.5 <- readRDS("inputs/0.5/london/fit.rds")
fit_midlands_0.5 <- readRDS("inputs/0.5/midlands/fit.rds")
fit_northeast_0.5 <- readRDS("inputs/0.5/north_east_and_yorkshire/fit.rds")
fit_northwest_0.5 <- readRDS("inputs/0.5/north_west/fit.rds")
fit_southeast_0.5 <- readRDS("inputs/0.5/south_east/fit.rds")
fit_southwest_0.5 <- readRDS("inputs/0.5/south_west/fit.rds")
fit_england_0.5 <- readRDS("inputs/0.5/england/fit.rds")

fit_east_0.6 <- readRDS("inputs/0.6/east_of_england/fit.rds")
fit_london_0.6 <- readRDS("inputs/0.6/london/fit.rds")
fit_midlands_0.6 <- readRDS("inputs/0.6/midlands/fit.rds")
fit_northeast_0.6 <- readRDS("inputs/0.6/north_east_and_yorkshire/fit.rds")
fit_northwest_0.6 <- readRDS("inputs/0.6/north_west/fit.rds")
fit_southeast_0.6 <- readRDS("inputs/0.6/south_east/fit.rds")
fit_southwest_0.6 <- readRDS("inputs/0.6/south_west/fit.rds")
fit_england_0.6 <- readRDS("inputs/0.6/england/fit.rds")

fit_east_0.7 <- readRDS("inputs/0.7/east_of_england/fit.rds")
fit_london_0.7 <- readRDS("inputs/0.7/london/fit.rds")
fit_midlands_0.7 <- readRDS("inputs/0.7/midlands/fit.rds")
fit_northeast_0.7 <- readRDS("inputs/0.7/north_east_and_yorkshire/fit.rds")
fit_northwest_0.7 <- readRDS("inputs/0.7/north_west/fit.rds")
fit_southeast_0.7 <- readRDS("inputs/0.7/south_east/fit.rds")
fit_southwest_0.7 <- readRDS("inputs/0.7/south_west/fit.rds")
fit_england_0.7 <- readRDS("inputs/0.7/england/fit.rds")

fit_east_0.8 <- readRDS("inputs/0.8/east_of_england/fit.rds")
fit_london_0.8 <- readRDS("inputs/0.8/london/fit.rds")
fit_midlands_0.8 <- readRDS("inputs/0.8/midlands/fit.rds")
fit_northeast_0.8 <- readRDS("inputs/0.8/north_east_and_yorkshire/fit.rds")
fit_northwest_0.8 <- readRDS("inputs/0.8/north_west/fit.rds")
fit_southeast_0.8 <- readRDS("inputs/0.8/south_east/fit.rds")
fit_southwest_0.8 <- readRDS("inputs/0.8/south_west/fit.rds")
fit_england_0.8 <- readRDS("inputs/0.8/england/fit.rds")

fit_east_0.9 <- readRDS("inputs/0.9/east_of_england/fit.rds")
fit_london_0.9 <- readRDS("inputs/0.9/london/fit.rds")
fit_midlands_0.9 <- readRDS("inputs/0.9/midlands/fit.rds")
fit_northeast_0.9 <- readRDS("inputs/0.9/north_east_and_yorkshire/fit.rds")
fit_northwest_0.9 <- readRDS("inputs/0.9/north_west/fit.rds")
fit_southeast_0.9 <- readRDS("inputs/0.9/south_east/fit.rds")
fit_southwest_0.9 <- readRDS("inputs/0.9/south_west/fit.rds")
fit_england_0.9 <- readRDS("inputs/0.9/england/fit.rds")

fit_east_1 <- readRDS("inputs/1/east_of_england/fit.rds")
fit_london_1 <- readRDS("inputs/1/london/fit.rds")
fit_midlands_1 <- readRDS("inputs/1/midlands/fit.rds")
fit_northeast_1 <- readRDS("inputs/1/north_east_and_yorkshire/fit.rds")
fit_northwest_1 <- readRDS("inputs/1/north_west/fit.rds")
fit_southeast_1 <- readRDS("inputs/1/south_east/fit.rds")
fit_southwest_1 <- readRDS("inputs/1/south_west/fit.rds")
fit_england_1 <- readRDS("inputs/1/england/fit.rds")

england_R0_0 <- readRDS("inputs/0/england_R0.rds")
england_R0_0.1 <- readRDS("inputs/0.1/england_R0.rds")
england_R0_0.2 <- readRDS("inputs/0.2/england_R0.rds")
england_R0_0.3 <- readRDS("inputs/0.3/england_R0.rds")
england_R0_0.4 <- readRDS("inputs/0.4/england_R0.rds")
england_R0_0.5 <- readRDS("inputs/0.5/england_R0.rds")
england_R0_0.6 <- readRDS("inputs/0.6/england_R0.rds")
england_R0_0.7 <- readRDS("inputs/0.7/england_R0.rds")
england_R0_0.8 <- readRDS("inputs/0.8/england_R0.rds")
england_R0_0.9 <- readRDS("inputs/0.9/england_R0.rds")
england_R0_1 <- readRDS("inputs/1/england_R0.rds")

england_instrinsic_0 <- readRDS("inputs/0/england_intrinsic_severity.rds")
england_instrinsic_0.1 <- readRDS("inputs/0.1/england_intrinsic_severity.rds")
england_instrinsic_0.2 <- readRDS("inputs/0.2/england_intrinsic_severity.rds")
england_instrinsic_0.3 <- readRDS("inputs/0.3/england_intrinsic_severity.rds")
england_instrinsic_0.4 <- readRDS("inputs/0.4/england_intrinsic_severity.rds")
england_instrinsic_0.5 <- readRDS("inputs/0.5/england_intrinsic_severity.rds")
england_instrinsic_0.6 <- readRDS("inputs/0.6/england_intrinsic_severity.rds")
england_instrinsic_0.7 <- readRDS("inputs/0.7/england_intrinsic_severity.rds")
england_instrinsic_0.8 <- readRDS("inputs/0.8/england_intrinsic_severity.rds")
england_instrinsic_0.9 <- readRDS("inputs/0.9/england_intrinsic_severity.rds")
england_instrinsic_1 <- readRDS("inputs/1/england_intrinsic_severity.rds")

Rt_england_0 <- readRDS("inputs/0/Rt_england.rds")
Rt_england_0.1 <- readRDS("inputs/0.1/Rt_england.rds")
Rt_england_0.2 <- readRDS("inputs/0.2/Rt_england.rds")
Rt_england_0.3 <- readRDS("inputs/0.3/Rt_england.rds")
Rt_england_0.4 <- readRDS("inputs/0.4/Rt_england.rds")
Rt_england_0.5 <- readRDS("inputs/0.5/Rt_england.rds")
Rt_england_0.6 <- readRDS("inputs/0.6/Rt_england.rds")
Rt_england_0.7 <- readRDS("inputs/0.7/Rt_england.rds")
Rt_england_0.8 <- readRDS("inputs/0.8/Rt_england.rds")
Rt_england_0.9 <- readRDS("inputs/0.9/Rt_england.rds")
Rt_england_1 <- readRDS("inputs/1/Rt_england.rds")

#ifr continuous data====

ifr_east=list(reference=fit_east_0$severity$ifr,remove_10=fit_east_0.1$severity$ifr,remove_20=fit_east_0.2$severity$ifr,remove_30=fit_east_0.3$severity$ifr,remove_40=fit_east_0.4$severity$ifr,remove_50=fit_east_0.5$severity$ifr,remove_60=fit_east_0.6$severity$ifr,remove_70=fit_east_0.7$severity$ifr,remove_80=fit_east_0.8$severity$ifr,remove_90=fit_east_0.9$severity$ifr,remove_100=fit_east_1$severity$ifr)
ifr_london=list(reference=fit_london_0$severity$ifr,remove_10=fit_london_0.1$severity$ifr,remove_20=fit_london_0.2$severity$ifr,remove_30=fit_london_0.3$severity$ifr,remove_40=fit_london_0.4$severity$ifr,remove_50=fit_london_0.5$severity$ifr,remove_60=fit_london_0.6$severity$ifr,remove_70=fit_london_0.7$severity$ifr,remove_80=fit_london_0.8$severity$ifr,remove_90=fit_london_0.9$severity$ifr,remove_100=fit_london_1$severity$ifr)
ifr_midlands=list(reference=fit_midlands_0$severity$ifr,remove_10=fit_midlands_0.1$severity$ifr,remove_20=fit_midlands_0.2$severity$ifr,remove_30=fit_midlands_0.3$severity$ifr,remove_40=fit_midlands_0.4$severity$ifr,remove_50=fit_midlands_0.5$severity$ifr,remove_60=fit_midlands_0.6$severity$ifr,remove_70=fit_midlands_0.7$severity$ifr,remove_80=fit_midlands_0.8$severity$ifr,remove_90=fit_midlands_0.9$severity$ifr,remove_100=fit_midlands_1$severity$ifr)
ifr_northeast=list(reference=fit_northeast_0$severity$ifr,remove_10=fit_northeast_0.1$severity$ifr,remove_20=fit_northeast_0.2$severity$ifr,remove_30=fit_northeast_0.3$severity$ifr,remove_40=fit_northeast_0.4$severity$ifr,remove_50=fit_northeast_0.5$severity$ifr,remove_60=fit_northeast_0.6$severity$ifr,remove_70=fit_northeast_0.7$severity$ifr,remove_80=fit_northeast_0.8$severity$ifr,remove_90=fit_northeast_0.9$severity$ifr,remove_100=fit_northeast_1$severity$ifr)
ifr_northwest=list(reference=fit_northwest_0$severity$ifr,remove_10=fit_northwest_0.1$severity$ifr,remove_20=fit_northwest_0.2$severity$ifr,remove_30=fit_northwest_0.3$severity$ifr,remove_40=fit_northwest_0.4$severity$ifr,remove_50=fit_northwest_0.5$severity$ifr,remove_60=fit_northwest_0.6$severity$ifr,remove_70=fit_northwest_0.7$severity$ifr,remove_80=fit_northwest_0.8$severity$ifr,remove_90=fit_northwest_0.9$severity$ifr,remove_100=fit_northwest_1$severity$ifr)
ifr_southeast=list(reference=fit_southeast_0$severity$ifr,remove_10=fit_southeast_0.1$severity$ifr,remove_20=fit_southeast_0.2$severity$ifr,remove_30=fit_southeast_0.3$severity$ifr,remove_40=fit_southeast_0.4$severity$ifr,remove_50=fit_southeast_0.5$severity$ifr,remove_60=fit_southeast_0.6$severity$ifr,remove_70=fit_southeast_0.7$severity$ifr,remove_80=fit_southeast_0.8$severity$ifr,remove_90=fit_southeast_0.9$severity$ifr,remove_100=fit_southeast_1$severity$ifr)
ifr_southwest=list(reference=fit_southwest_0$severity$ifr,remove_10=fit_southwest_0.1$severity$ifr,remove_20=fit_southwest_0.2$severity$ifr,remove_30=fit_southwest_0.3$severity$ifr,remove_40=fit_southwest_0.4$severity$ifr,remove_50=fit_southwest_0.5$severity$ifr,remove_60=fit_southwest_0.6$severity$ifr,remove_70=fit_southwest_0.7$severity$ifr,remove_80=fit_southwest_0.8$severity$ifr,remove_90=fit_southwest_0.9$severity$ifr,remove_100=fit_southwest_1$severity$ifr)
ifr_england=list(reference=fit_england_0$ifr,remove_10=fit_england_0.1$ifr,remove_20=fit_england_0.2$ifr,remove_30=fit_england_0.3$ifr,remove_40=fit_england_0.4$ifr,remove_50=fit_england_0.5$ifr,remove_60=fit_england_0.6$ifr,remove_70=fit_england_0.7$ifr,remove_80=fit_england_0.8$ifr,remove_90=fit_england_0.9$ifr,remove_100=fit_england_1$ifr)
ifr_continuous<-list(east_of_england=ifr_east,london=ifr_london,midlands=ifr_midlands,north_east_and_yorkshire=ifr_northeast,north_west=ifr_northwest,south_east=ifr_southeast,south_west=ifr_southwest,england=ifr_england)

#ihr continuous data====

ihr_east=list(reference=fit_east_0$severity$ihr,remove_10=fit_east_0.1$severity$ihr,remove_20=fit_east_0.2$severity$ihr,remove_30=fit_east_0.3$severity$ihr,remove_40=fit_east_0.4$severity$ihr,remove_50=fit_east_0.5$severity$ihr,remove_60=fit_east_0.6$severity$ihr,remove_70=fit_east_0.7$severity$ihr,remove_80=fit_east_0.8$severity$ihr,remove_90=fit_east_0.9$severity$ihr,remove_100=fit_east_1$severity$ihr)
ihr_london=list(reference=fit_london_0$severity$ihr,remove_10=fit_london_0.1$severity$ihr,remove_20=fit_london_0.2$severity$ihr,remove_30=fit_london_0.3$severity$ihr,remove_40=fit_london_0.4$severity$ihr,remove_50=fit_london_0.5$severity$ihr,remove_60=fit_london_0.6$severity$ihr,remove_70=fit_london_0.7$severity$ihr,remove_80=fit_london_0.8$severity$ihr,remove_90=fit_london_0.9$severity$ihr,remove_100=fit_london_1$severity$ihr)
ihr_midlands=list(reference=fit_midlands_0$severity$ihr,remove_10=fit_midlands_0.1$severity$ihr,remove_20=fit_midlands_0.2$severity$ihr,remove_30=fit_midlands_0.3$severity$ihr,remove_40=fit_midlands_0.4$severity$ihr,remove_50=fit_midlands_0.5$severity$ihr,remove_60=fit_midlands_0.6$severity$ihr,remove_70=fit_midlands_0.7$severity$ihr,remove_80=fit_midlands_0.8$severity$ihr,remove_90=fit_midlands_0.9$severity$ihr,remove_100=fit_midlands_1$severity$ihr)
ihr_northeast=list(reference=fit_northeast_0$severity$ihr,remove_10=fit_northeast_0.1$severity$ihr,remove_20=fit_northeast_0.2$severity$ihr,remove_30=fit_northeast_0.3$severity$ihr,remove_40=fit_northeast_0.4$severity$ihr,remove_50=fit_northeast_0.5$severity$ihr,remove_60=fit_northeast_0.6$severity$ihr,remove_70=fit_northeast_0.7$severity$ihr,remove_80=fit_northeast_0.8$severity$ihr,remove_90=fit_northeast_0.9$severity$ihr,remove_100=fit_northeast_1$severity$ihr)
ihr_northwest=list(reference=fit_northwest_0$severity$ihr,remove_10=fit_northwest_0.1$severity$ihr,remove_20=fit_northwest_0.2$severity$ihr,remove_30=fit_northwest_0.3$severity$ihr,remove_40=fit_northwest_0.4$severity$ihr,remove_50=fit_northwest_0.5$severity$ihr,remove_60=fit_northwest_0.6$severity$ihr,remove_70=fit_northwest_0.7$severity$ihr,remove_80=fit_northwest_0.8$severity$ihr,remove_90=fit_northwest_0.9$severity$ihr,remove_100=fit_northwest_1$severity$ihr)
ihr_southeast=list(reference=fit_southeast_0$severity$ihr,remove_10=fit_southeast_0.1$severity$ihr,remove_20=fit_southeast_0.2$severity$ihr,remove_30=fit_southeast_0.3$severity$ihr,remove_40=fit_southeast_0.4$severity$ihr,remove_50=fit_southeast_0.5$severity$ihr,remove_60=fit_southeast_0.6$severity$ihr,remove_70=fit_southeast_0.7$severity$ihr,remove_80=fit_southeast_0.8$severity$ihr,remove_90=fit_southeast_0.9$severity$ihr,remove_100=fit_southeast_1$severity$ihr)
ihr_southwest=list(reference=fit_southwest_0$severity$ihr,remove_10=fit_southwest_0.1$severity$ihr,remove_20=fit_southwest_0.2$severity$ihr,remove_30=fit_southwest_0.3$severity$ihr,remove_40=fit_southwest_0.4$severity$ihr,remove_50=fit_southwest_0.5$severity$ihr,remove_60=fit_southwest_0.6$severity$ihr,remove_70=fit_southwest_0.7$severity$ihr,remove_80=fit_southwest_0.8$severity$ihr,remove_90=fit_southwest_0.9$severity$ihr,remove_100=fit_southwest_1$severity$ihr)
ihr_england=list(reference=fit_england_0$ihr,remove_10=fit_england_0.1$ihr,remove_20=fit_england_0.2$ihr,remove_30=fit_england_0.3$ihr,remove_40=fit_england_0.4$ihr,remove_50=fit_england_0.5$ihr,remove_60=fit_england_0.6$ihr,remove_70=fit_england_0.7$ihr,remove_80=fit_england_0.8$ihr,remove_90=fit_england_0.9$ihr,remove_100=fit_england_1$ihr)
ihr_continuous<-list(east_of_england=ihr_east,london=ihr_london,midlands=ihr_midlands,north_east_and_yorkshire=ihr_northeast,north_west=ihr_northwest,south_east=ihr_southeast,south_west=ihr_southwest,england=ihr_england)

#hfr continuous data====

hfr_east=list(reference=fit_east_0$severity$hfr,remove_10=fit_east_0.1$severity$hfr,remove_20=fit_east_0.2$severity$hfr,remove_30=fit_east_0.3$severity$hfr,remove_40=fit_east_0.4$severity$hfr,remove_50=fit_east_0.5$severity$hfr,remove_60=fit_east_0.6$severity$hfr,remove_70=fit_east_0.7$severity$hfr,remove_80=fit_east_0.8$severity$hfr,remove_90=fit_east_0.9$severity$hfr,remove_100=fit_east_1$severity$hfr)
hfr_london=list(reference=fit_london_0$severity$hfr,remove_10=fit_london_0.1$severity$hfr,remove_20=fit_london_0.2$severity$hfr,remove_30=fit_london_0.3$severity$hfr,remove_40=fit_london_0.4$severity$hfr,remove_50=fit_london_0.5$severity$hfr,remove_60=fit_london_0.6$severity$hfr,remove_70=fit_london_0.7$severity$hfr,remove_80=fit_london_0.8$severity$hfr,remove_90=fit_london_0.9$severity$hfr,remove_100=fit_london_1$severity$hfr)
hfr_midlands=list(reference=fit_midlands_0$severity$hfr,remove_10=fit_midlands_0.1$severity$hfr,remove_20=fit_midlands_0.2$severity$hfr,remove_30=fit_midlands_0.3$severity$hfr,remove_40=fit_midlands_0.4$severity$hfr,remove_50=fit_midlands_0.5$severity$hfr,remove_60=fit_midlands_0.6$severity$hfr,remove_70=fit_midlands_0.7$severity$hfr,remove_80=fit_midlands_0.8$severity$hfr,remove_90=fit_midlands_0.9$severity$hfr,remove_100=fit_midlands_1$severity$hfr)
hfr_northeast=list(reference=fit_northeast_0$severity$hfr,remove_10=fit_northeast_0.1$severity$hfr,remove_20=fit_northeast_0.2$severity$hfr,remove_30=fit_northeast_0.3$severity$hfr,remove_40=fit_northeast_0.4$severity$hfr,remove_50=fit_northeast_0.5$severity$hfr,remove_60=fit_northeast_0.6$severity$hfr,remove_70=fit_northeast_0.7$severity$hfr,remove_80=fit_northeast_0.8$severity$hfr,remove_90=fit_northeast_0.9$severity$hfr,remove_100=fit_northeast_1$severity$hfr)
hfr_northwest=list(reference=fit_northwest_0$severity$hfr,remove_10=fit_northwest_0.1$severity$hfr,remove_20=fit_northwest_0.2$severity$hfr,remove_30=fit_northwest_0.3$severity$hfr,remove_40=fit_northwest_0.4$severity$hfr,remove_50=fit_northwest_0.5$severity$hfr,remove_60=fit_northwest_0.6$severity$hfr,remove_70=fit_northwest_0.7$severity$hfr,remove_80=fit_northwest_0.8$severity$hfr,remove_90=fit_northwest_0.9$severity$hfr,remove_100=fit_northwest_1$severity$hfr)
hfr_southeast=list(reference=fit_southeast_0$severity$hfr,remove_10=fit_southeast_0.1$severity$hfr,remove_20=fit_southeast_0.2$severity$hfr,remove_30=fit_southeast_0.3$severity$hfr,remove_40=fit_southeast_0.4$severity$hfr,remove_50=fit_southeast_0.5$severity$hfr,remove_60=fit_southeast_0.6$severity$hfr,remove_70=fit_southeast_0.7$severity$hfr,remove_80=fit_southeast_0.8$severity$hfr,remove_90=fit_southeast_0.9$severity$hfr,remove_100=fit_southeast_1$severity$hfr)
hfr_southwest=list(reference=fit_southwest_0$severity$hfr,remove_10=fit_southwest_0.1$severity$hfr,remove_20=fit_southwest_0.2$severity$hfr,remove_30=fit_southwest_0.3$severity$hfr,remove_40=fit_southwest_0.4$severity$hfr,remove_50=fit_southwest_0.5$severity$hfr,remove_60=fit_southwest_0.6$severity$hfr,remove_70=fit_southwest_0.7$severity$hfr,remove_80=fit_southwest_0.8$severity$hfr,remove_90=fit_southwest_0.9$severity$hfr,remove_100=fit_southwest_1$severity$hfr)
hfr_england=list(reference=fit_england_0$hfr,remove_10=fit_england_0.1$hfr,remove_20=fit_england_0.2$hfr,remove_30=fit_england_0.3$hfr,remove_40=fit_england_0.4$hfr,remove_50=fit_england_0.5$hfr,remove_60=fit_england_0.6$hfr,remove_70=fit_england_0.7$hfr,remove_80=fit_england_0.8$hfr,remove_90=fit_england_0.9$hfr,remove_100=fit_england_1$hfr)
hfr_continuous<-list(east_of_england=hfr_east,london=hfr_london,midlands=hfr_midlands,north_east_and_yorkshire=hfr_northeast,north_west=hfr_northwest,south_east=hfr_southeast,south_west=hfr_southwest,england=hfr_england)

#ifr emergency3 data====

IFR_east<-list(reference=fit_east_0$intrinsic_severity$IFR[3,,],remove_10=fit_east_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_east_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_east_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_east_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_east_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_east_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_east_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_east_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_east_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_east_1$intrinsic_severity$IFR[3,,])
IFR_london<-list(reference=fit_london_0$intrinsic_severity$IFR[3,,],remove_10=fit_london_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_london_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_london_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_london_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_london_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_london_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_london_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_london_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_london_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_london_1$intrinsic_severity$IFR[3,,])
IFR_midlands<-list(reference=fit_midlands_0$intrinsic_severity$IFR[3,,],remove_10=fit_midlands_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_midlands_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_midlands_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_midlands_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_midlands_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_midlands_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_midlands_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_midlands_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_midlands_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_midlands_1$intrinsic_severity$IFR[3,,])
IFR_northeast<-list(reference=fit_northeast_0$intrinsic_severity$IFR[3,,],remove_10=fit_northeast_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_northeast_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_northeast_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_northeast_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_northeast_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_northeast_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_northeast_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_northeast_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_northeast_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_northeast_1$intrinsic_severity$IFR[3,,])
IFR_northwest<-list(reference=fit_northwest_0$intrinsic_severity$IFR[3,,],remove_10=fit_northwest_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_northwest_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_northwest_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_northwest_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_northwest_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_northwest_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_northwest_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_northwest_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_northwest_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_northwest_1$intrinsic_severity$IFR[3,,])
IFR_southeast<-list(reference=fit_southeast_0$intrinsic_severity$IFR[3,,],remove_10=fit_southeast_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_southeast_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_southeast_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_southeast_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_southeast_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_southeast_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_southeast_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_southeast_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_southeast_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_southeast_1$intrinsic_severity$IFR[3,,])
IFR_southwest<-list(reference=fit_southwest_0$intrinsic_severity$IFR[3,,],remove_10=fit_southwest_0.1$intrinsic_severity$IFR[3,,],remove_20=fit_southwest_0.2$intrinsic_severity$IFR[3,,],remove_30=fit_southwest_0.3$intrinsic_severity$IFR[3,,],remove_40=fit_southwest_0.4$intrinsic_severity$IFR[3,,],remove_50=fit_southwest_0.5$intrinsic_severity$IFR[3,,],remove_60=fit_southwest_0.6$intrinsic_severity$IFR[3,,],remove_70=fit_southwest_0.7$intrinsic_severity$IFR[3,,],remove_80=fit_southwest_0.8$intrinsic_severity$IFR[3,,],remove_90=fit_southwest_0.9$intrinsic_severity$IFR[3,,],remove_100=fit_southwest_1$intrinsic_severity$IFR[3,,])
IFR_england<-list(reference=england_instrinsic_0$IFR[3,,],remove_10=england_instrinsic_0.1$IFR[3,,],remove_20=england_instrinsic_0.2$IFR[3,,],remove_30=england_instrinsic_0.3$IFR[3,,],remove_40=england_instrinsic_0.4$IFR[3,,],remove_50=england_instrinsic_0.5$IFR[3,,],remove_60=england_instrinsic_0.6$IFR[3,,],remove_70=england_instrinsic_0.7$IFR[3,,],remove_80=england_instrinsic_0.8$IFR[3,,],remove_90=england_instrinsic_0.9$IFR[3,,],remove_100=england_instrinsic_1$IFR[3,,])

IFR<-list(east_of_england=IFR_east,london=IFR_london,midlands=IFR_midlands,north_east_and_yorkshire=IFR_northeast,north_west=IFR_northwest,south_east=IFR_southeast,south_west=IFR_southwest,england=IFR_england)

#ihr emergency3 data====

IHR_east<-list(reference=fit_east_0$intrinsic_severity$IHR[3,,],remove_10=fit_east_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_east_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_east_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_east_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_east_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_east_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_east_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_east_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_east_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_east_1$intrinsic_severity$IHR[3,,])
IHR_london<-list(reference=fit_london_0$intrinsic_severity$IHR[3,,],remove_10=fit_london_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_london_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_london_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_london_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_london_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_london_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_london_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_london_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_london_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_london_1$intrinsic_severity$IHR[3,,])
IHR_midlands<-list(reference=fit_midlands_0$intrinsic_severity$IHR[3,,],remove_10=fit_midlands_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_midlands_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_midlands_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_midlands_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_midlands_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_midlands_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_midlands_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_midlands_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_midlands_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_midlands_1$intrinsic_severity$IHR[3,,])
IHR_northeast<-list(reference=fit_northeast_0$intrinsic_severity$IHR[3,,],remove_10=fit_northeast_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_northeast_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_northeast_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_northeast_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_northeast_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_northeast_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_northeast_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_northeast_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_northeast_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_northeast_1$intrinsic_severity$IHR[3,,])
IHR_northwest<-list(reference=fit_northwest_0$intrinsic_severity$IHR[3,,],remove_10=fit_northwest_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_northwest_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_northwest_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_northwest_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_northwest_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_northwest_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_northwest_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_northwest_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_northwest_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_northwest_1$intrinsic_severity$IHR[3,,])
IHR_southeast<-list(reference=fit_southeast_0$intrinsic_severity$IHR[3,,],remove_10=fit_southeast_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_southeast_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_southeast_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_southeast_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_southeast_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_southeast_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_southeast_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_southeast_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_southeast_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_southeast_1$intrinsic_severity$IHR[3,,])
IHR_southwest<-list(reference=fit_southwest_0$intrinsic_severity$IHR[3,,],remove_10=fit_southwest_0.1$intrinsic_severity$IHR[3,,],remove_20=fit_southwest_0.2$intrinsic_severity$IHR[3,,],remove_30=fit_southwest_0.3$intrinsic_severity$IHR[3,,],remove_40=fit_southwest_0.4$intrinsic_severity$IHR[3,,],remove_50=fit_southwest_0.5$intrinsic_severity$IHR[3,,],remove_60=fit_southwest_0.6$intrinsic_severity$IHR[3,,],remove_70=fit_southwest_0.7$intrinsic_severity$IHR[3,,],remove_80=fit_southwest_0.8$intrinsic_severity$IHR[3,,],remove_90=fit_southwest_0.9$intrinsic_severity$IHR[3,,],remove_100=fit_southwest_1$intrinsic_severity$IHR[3,,])
IHR_england<-list(reference=england_instrinsic_0$IHR[3,,],remove_10=england_instrinsic_0.1$IHR[3,,],remove_20=england_instrinsic_0.2$IHR[3,,],remove_30=england_instrinsic_0.3$IHR[3,,],remove_40=england_instrinsic_0.4$IHR[3,,],remove_50=england_instrinsic_0.5$IHR[3,,],remove_60=england_instrinsic_0.6$IHR[3,,],remove_70=england_instrinsic_0.7$IHR[3,,],remove_80=england_instrinsic_0.8$IHR[3,,],remove_90=england_instrinsic_0.9$IHR[3,,],remove_100=england_instrinsic_1$IHR[3,,])

IHR<-list(east_of_england=IHR_east,london=IHR_london,midlands=IHR_midlands,north_east_and_yorkshire=IHR_northeast,north_west=IHR_northwest,south_east=IHR_southeast,south_west=IHR_southwest,england=IHR_england)

#hfr emergency3 data====

HFR_east<-list(reference=fit_east_0$intrinsic_severity$HFR[3,,],remove_10=fit_east_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_east_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_east_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_east_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_east_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_east_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_east_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_east_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_east_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_east_1$intrinsic_severity$HFR[3,,])
HFR_london<-list(reference=fit_london_0$intrinsic_severity$HFR[3,,],remove_10=fit_london_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_london_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_london_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_london_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_london_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_london_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_london_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_london_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_london_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_london_1$intrinsic_severity$HFR[3,,])
HFR_midlands<-list(reference=fit_midlands_0$intrinsic_severity$HFR[3,,],remove_10=fit_midlands_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_midlands_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_midlands_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_midlands_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_midlands_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_midlands_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_midlands_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_midlands_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_midlands_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_midlands_1$intrinsic_severity$HFR[3,,])
HFR_northeast<-list(reference=fit_northeast_0$intrinsic_severity$HFR[3,,],remove_10=fit_northeast_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_northeast_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_northeast_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_northeast_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_northeast_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_northeast_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_northeast_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_northeast_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_northeast_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_northeast_1$intrinsic_severity$HFR[3,,])
HFR_northwest<-list(reference=fit_northwest_0$intrinsic_severity$HFR[3,,],remove_10=fit_northwest_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_northwest_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_northwest_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_northwest_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_northwest_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_northwest_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_northwest_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_northwest_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_northwest_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_northwest_1$intrinsic_severity$HFR[3,,])
HFR_southeast<-list(reference=fit_southeast_0$intrinsic_severity$HFR[3,,],remove_10=fit_southeast_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_southeast_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_southeast_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_southeast_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_southeast_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_southeast_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_southeast_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_southeast_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_southeast_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_southeast_1$intrinsic_severity$HFR[3,,])
HFR_southwest<-list(reference=fit_southwest_0$intrinsic_severity$HFR[3,,],remove_10=fit_southwest_0.1$intrinsic_severity$HFR[3,,],remove_20=fit_southwest_0.2$intrinsic_severity$HFR[3,,],remove_30=fit_southwest_0.3$intrinsic_severity$HFR[3,,],remove_40=fit_southwest_0.4$intrinsic_severity$HFR[3,,],remove_50=fit_southwest_0.5$intrinsic_severity$HFR[3,,],remove_60=fit_southwest_0.6$intrinsic_severity$HFR[3,,],remove_70=fit_southwest_0.7$intrinsic_severity$HFR[3,,],remove_80=fit_southwest_0.8$intrinsic_severity$HFR[3,,],remove_90=fit_southwest_0.9$intrinsic_severity$HFR[3,,],remove_100=fit_southwest_1$intrinsic_severity$HFR[3,,])
HFR_england<-list(reference=england_instrinsic_0$HFR[3,,],remove_10=england_instrinsic_0.1$HFR[3,,],remove_20=england_instrinsic_0.2$HFR[3,,],remove_30=england_instrinsic_0.3$HFR[3,,],remove_40=england_instrinsic_0.4$HFR[3,,],remove_50=england_instrinsic_0.5$HFR[3,,],remove_60=england_instrinsic_0.6$HFR[3,,],remove_70=england_instrinsic_0.7$HFR[3,,],remove_80=england_instrinsic_0.8$HFR[3,,],remove_90=england_instrinsic_0.9$HFR[3,,],remove_100=england_instrinsic_1$HFR[3,,])

HFR<-list(east_of_england=HFR_east,london=HFR_london,midlands=HFR_midlands,north_east_and_yorkshire=HFR_northeast,north_west=HFR_northwest,south_east=HFR_southeast,south_west=HFR_southwest,england=HFR_england)

#Rt_eff data====

Rt_eff_east=list(reference=fit_east_0$rt$eff_Rt[,3,],remove_10=fit_east_0.1$rt$eff_Rt[,3,],remove_20=fit_east_0.2$rt$eff_Rt[,3,],remove_30=fit_east_0.3$rt$eff_Rt[,3,],remove_40=fit_east_0.4$rt$eff_Rt[,3,],remove_50=fit_east_0.5$rt$eff_Rt[,3,],remove_60=fit_east_0.6$rt$eff_Rt[,3,],remove_70=fit_east_0.7$rt$eff_Rt[,3,],remove_80=fit_east_0.8$rt$eff_Rt[,3,],remove_90=fit_east_0.9$rt$eff_Rt[,3,],remove_100=fit_east_1$rt$eff_Rt[,3,])
Rt_eff_london=list(reference=fit_london_0$rt$eff_Rt[,3,],remove_10=fit_london_0.1$rt$eff_Rt[,3,],remove_20=fit_london_0.2$rt$eff_Rt[,3,],remove_30=fit_london_0.3$rt$eff_Rt[,3,],remove_40=fit_london_0.4$rt$eff_Rt[,3,],remove_50=fit_london_0.5$rt$eff_Rt[,3,],remove_60=fit_london_0.6$rt$eff_Rt[,3,],remove_70=fit_london_0.7$rt$eff_Rt[,3,],remove_80=fit_london_0.8$rt$eff_Rt[,3,],remove_90=fit_london_0.9$rt$eff_Rt[,3,],remove_100=fit_london_1$rt$eff_Rt[,3,])
Rt_eff_midlands=list(reference=fit_midlands_0$rt$eff_Rt[,3,],remove_10=fit_midlands_0.1$rt$eff_Rt[,3,],remove_20=fit_midlands_0.2$rt$eff_Rt[,3,],remove_30=fit_midlands_0.3$rt$eff_Rt[,3,],remove_40=fit_midlands_0.4$rt$eff_Rt[,3,],remove_50=fit_midlands_0.5$rt$eff_Rt[,3,],remove_60=fit_midlands_0.6$rt$eff_Rt[,3,],remove_70=fit_midlands_0.7$rt$eff_Rt[,3,],remove_80=fit_midlands_0.8$rt$eff_Rt[,3,],remove_90=fit_midlands_0.9$rt$eff_Rt[,3,],remove_100=fit_midlands_1$rt$eff_Rt[,3,])
Rt_eff_northeast=list(reference=fit_northeast_0$rt$eff_Rt[,3,],remove_10=fit_northeast_0.1$rt$eff_Rt[,3,],remove_20=fit_northeast_0.2$rt$eff_Rt[,3,],remove_30=fit_northeast_0.3$rt$eff_Rt[,3,],remove_40=fit_northeast_0.4$rt$eff_Rt[,3,],remove_50=fit_northeast_0.5$rt$eff_Rt[,3,],remove_60=fit_northeast_0.6$rt$eff_Rt[,3,],remove_70=fit_northeast_0.7$rt$eff_Rt[,3,],remove_80=fit_northeast_0.8$rt$eff_Rt[,3,],remove_90=fit_northeast_0.9$rt$eff_Rt[,3,],remove_100=fit_northeast_1$rt$eff_Rt[,3,])
Rt_eff_northwest=list(reference=fit_northwest_0$rt$eff_Rt[,3,],remove_10=fit_northwest_0.1$rt$eff_Rt[,3,],remove_20=fit_northwest_0.2$rt$eff_Rt[,3,],remove_30=fit_northwest_0.3$rt$eff_Rt[,3,],remove_40=fit_northwest_0.4$rt$eff_Rt[,3,],remove_50=fit_northwest_0.5$rt$eff_Rt[,3,],remove_60=fit_northwest_0.6$rt$eff_Rt[,3,],remove_70=fit_northwest_0.7$rt$eff_Rt[,3,],remove_80=fit_northwest_0.8$rt$eff_Rt[,3,],remove_90=fit_northwest_0.9$rt$eff_Rt[,3,],remove_100=fit_northwest_1$rt$eff_Rt[,3,])
Rt_eff_southeast=list(reference=fit_southeast_0$rt$eff_Rt[,3,],remove_10=fit_southeast_0.1$rt$eff_Rt[,3,],remove_20=fit_southeast_0.2$rt$eff_Rt[,3,],remove_30=fit_southeast_0.3$rt$eff_Rt[,3,],remove_40=fit_southeast_0.4$rt$eff_Rt[,3,],remove_50=fit_southeast_0.5$rt$eff_Rt[,3,],remove_60=fit_southeast_0.6$rt$eff_Rt[,3,],remove_70=fit_southeast_0.7$rt$eff_Rt[,3,],remove_80=fit_southeast_0.8$rt$eff_Rt[,3,],remove_90=fit_southeast_0.9$rt$eff_Rt[,3,],remove_100=fit_southeast_1$rt$eff_Rt[,3,])
Rt_eff_southwest=list(reference=fit_southwest_0$rt$eff_Rt[,3,],remove_10=fit_southwest_0.1$rt$eff_Rt[,3,],remove_20=fit_southwest_0.2$rt$eff_Rt[,3,],remove_30=fit_southwest_0.3$rt$eff_Rt[,3,],remove_40=fit_southwest_0.4$rt$eff_Rt[,3,],remove_50=fit_southwest_0.5$rt$eff_Rt[,3,],remove_60=fit_southwest_0.6$rt$eff_Rt[,3,],remove_70=fit_southwest_0.7$rt$eff_Rt[,3,],remove_80=fit_southwest_0.8$rt$eff_Rt[,3,],remove_90=fit_southwest_0.9$rt$eff_Rt[,3,],remove_100=fit_southwest_1$rt$eff_Rt[,3,])
Rt_eff_england=list(reference=Rt_england_0$eff_Rt[,3,],remove_10=Rt_england_0.1$eff_Rt[,3,],remove_20=Rt_england_0.2$eff_Rt[,3,],remove_30=Rt_england_0.3$eff_Rt[,3,],remove_40=Rt_england_0.4$eff_Rt[,3,],remove_50=Rt_england_0.5$eff_Rt[,3,],remove_60=Rt_england_0.6$eff_Rt[,3,],remove_70=Rt_england_0.7$eff_Rt[,3,],remove_80=Rt_england_0.8$eff_Rt[,3,],remove_90=Rt_england_0.9$eff_Rt[,3,],remove_100=Rt_england_1$eff_Rt[,3,])

Rt_eff<-list(east_of_england=Rt_eff_east,london=Rt_eff_london,midlands=Rt_eff_midlands,north_east_and_yorkshire=Rt_eff_northeast,north_west=Rt_eff_northwest,south_east=Rt_eff_southeast,south_west=Rt_eff_southwest,england=Rt_eff_england)

#R0 data====

get_R0 <- function(fit) {
  pars <- fit$samples$pars
  variant_names <- names(fit$parameters$base$strain_epochs)
  R0_variants <- list()
  for (nm in variant_names) {
    if (nm == "Wildtype") {
      R0 <- fit$rt$Rt_general[1, "weighted", ]
    } else {
      R0 <- R0 * pars[, paste0("ta_", tolower(nm))]
    }
    R0_variants[[nm]] <- R0
  }
  R0_variants  
}

R0_east_0 <- get_R0(fit_east_0)
R0_london_0 <- get_R0(fit_london_0)
R0_midlands_0 <- get_R0(fit_midlands_0)
R0_northeast_0 <- get_R0(fit_northeast_0)
R0_northwest_0 <- get_R0(fit_northwest_0)
R0_southeast_0 <- get_R0(fit_southeast_0)
R0_southwest_0 <- get_R0(fit_southwest_0)
R0_east_0.1 <- get_R0(fit_east_0.1)
R0_london_0.1 <- get_R0(fit_london_0.1)
R0_midlands_0.1 <- get_R0(fit_midlands_0.1)
R0_northeast_0.1 <- get_R0(fit_northeast_0.1)
R0_northwest_0.1 <- get_R0(fit_northwest_0.1)
R0_southeast_0.1 <- get_R0(fit_southeast_0.1)
R0_southwest_0.1 <- get_R0(fit_southwest_0.1)
R0_east_0.2 <- get_R0(fit_east_0.2)
R0_london_0.2 <- get_R0(fit_london_0.2)
R0_midlands_0.2 <- get_R0(fit_midlands_0.2)
R0_northeast_0.2 <- get_R0(fit_northeast_0.2)
R0_northwest_0.2 <- get_R0(fit_northwest_0.2)
R0_southeast_0.2 <- get_R0(fit_southeast_0.2)
R0_southwest_0.2 <- get_R0(fit_southwest_0.2)
R0_east_0.3 <- get_R0(fit_east_0.3)
R0_london_0.3 <- get_R0(fit_london_0.3)
R0_midlands_0.3 <- get_R0(fit_midlands_0.3)
R0_northeast_0.3 <- get_R0(fit_northeast_0.3)
R0_northwest_0.3 <- get_R0(fit_northwest_0.3)
R0_southeast_0.3 <- get_R0(fit_southeast_0.3)
R0_southwest_0.3 <- get_R0(fit_southwest_0.3)
R0_east_0.4 <- get_R0(fit_east_0.4)
R0_london_0.4 <- get_R0(fit_london_0.4)
R0_midlands_0.4 <- get_R0(fit_midlands_0.4)
R0_northeast_0.4 <- get_R0(fit_northeast_0.4)
R0_northwest_0.4 <- get_R0(fit_northwest_0.4)
R0_southeast_0.4 <- get_R0(fit_southeast_0.4)
R0_southwest_0.4 <- get_R0(fit_southwest_0.4)
R0_east_0.5 <- get_R0(fit_east_0.5)
R0_london_0.5 <- get_R0(fit_london_0.5)
R0_midlands_0.5 <- get_R0(fit_midlands_0.5)
R0_northeast_0.5 <- get_R0(fit_northeast_0.5)
R0_northwest_0.5 <- get_R0(fit_northwest_0.5)
R0_southeast_0.5 <- get_R0(fit_southeast_0.5)
R0_southwest_0.5 <- get_R0(fit_southwest_0.5)
R0_east_0.6 <- get_R0(fit_east_0.6)
R0_london_0.6 <- get_R0(fit_london_0.6)
R0_midlands_0.6 <- get_R0(fit_midlands_0.6)
R0_northeast_0.6 <- get_R0(fit_northeast_0.6)
R0_northwest_0.6 <- get_R0(fit_northwest_0.6)
R0_southeast_0.6 <- get_R0(fit_southeast_0.6)
R0_southwest_0.6 <- get_R0(fit_southwest_0.6)
R0_east_0.7 <- get_R0(fit_east_0.7)
R0_london_0.7 <- get_R0(fit_london_0.7)
R0_midlands_0.7 <- get_R0(fit_midlands_0.7)
R0_northeast_0.7 <- get_R0(fit_northeast_0.7)
R0_northwest_0.7 <- get_R0(fit_northwest_0.7)
R0_southeast_0.7 <- get_R0(fit_southeast_0.7)
R0_southwest_0.7 <- get_R0(fit_southwest_0.7)
R0_east_0.8 <- get_R0(fit_east_0.8)
R0_london_0.8 <- get_R0(fit_london_0.8)
R0_midlands_0.8 <- get_R0(fit_midlands_0.8)
R0_northeast_0.8 <- get_R0(fit_northeast_0.8)
R0_northwest_0.8 <- get_R0(fit_northwest_0.8)
R0_southeast_0.8 <- get_R0(fit_southeast_0.8)
R0_southwest_0.8 <- get_R0(fit_southwest_0.8)
R0_east_0.9 <- get_R0(fit_east_0.9)
R0_london_0.9 <- get_R0(fit_london_0.9)
R0_midlands_0.9 <- get_R0(fit_midlands_0.9)
R0_northeast_0.9 <- get_R0(fit_northeast_0.9)
R0_northwest_0.9 <- get_R0(fit_northwest_0.9)
R0_southeast_0.9 <- get_R0(fit_southeast_0.9)
R0_southwest_0.9 <- get_R0(fit_southwest_0.9)
R0_east_1 <- get_R0(fit_east_1)
R0_london_1 <- get_R0(fit_london_1)
R0_midlands_1 <- get_R0(fit_midlands_1)
R0_northeast_1 <- get_R0(fit_northeast_1)
R0_northwest_1 <- get_R0(fit_northwest_1)
R0_southeast_1 <- get_R0(fit_southeast_1)
R0_southwest_1 <- get_R0(fit_southwest_1)

R0_east_of_england=list(reference=R0_east_0,remove_10=R0_east_0.1,remove_20=R0_east_0.2,remove_30=R0_east_0.3,remove_40=R0_east_0.4,remove_50=R0_east_0.5,remove_60=R0_east_0.6,remove_70=R0_east_0.7,remove_80=R0_east_0.8,remove_90=R0_east_0.9,remove_100=R0_east_1)
R0_london=list(reference=R0_london_0,remove_10=R0_london_0.1,remove_20=R0_london_0.2,remove_30=R0_london_0.3,remove_40=R0_london_0.4,remove_50=R0_london_0.5,remove_60=R0_london_0.6,remove_70=R0_london_0.7,remove_80=R0_london_0.8,remove_90=R0_london_0.9,remove_100=R0_london_1)
R0_midlands=list(reference=R0_midlands_0,remove_10=R0_midlands_0.1,remove_20=R0_midlands_0.2,remove_30=R0_midlands_0.3,remove_40=R0_midlands_0.4,remove_50=R0_midlands_0.5,remove_60=R0_midlands_0.6,remove_70=R0_midlands_0.7,remove_80=R0_midlands_0.8,remove_90=R0_midlands_0.9,remove_100=R0_midlands_1)
R0_north_east_and_yorksire=list(reference=R0_northeast_0,remove_10=R0_northeast_0.1,remove_20=R0_northeast_0.2,remove_30=R0_northeast_0.3,remove_40=R0_northeast_0.4,remove_50=R0_northeast_0.5,remove_60=R0_northeast_0.6,remove_70=R0_northeast_0.7,remove_80=R0_northeast_0.8,remove_90=R0_northeast_0.9,remove_100=R0_northeast_1)
R0_north_west=list(reference=R0_northwest_0,remove_10=R0_northwest_0.1,remove_20=R0_northwest_0.2,remove_30=R0_northwest_0.3,remove_40=R0_northwest_0.4,remove_50=R0_northwest_0.5,remove_60=R0_northwest_0.6,remove_70=R0_northwest_0.7,remove_80=R0_northwest_0.8,remove_90=R0_northwest_0.9,remove_100=R0_northwest_1)
R0_south_east=list(reference=R0_southeast_0,remove_10=R0_southeast_0.1,remove_20=R0_southeast_0.2,remove_30=R0_southeast_0.3,remove_40=R0_southeast_0.4,remove_50=R0_southeast_0.5,remove_60=R0_southeast_0.6,remove_70=R0_southeast_0.7,remove_80=R0_southeast_0.8,remove_90=R0_southeast_0.9,remove_100=R0_southeast_1)
R0_south_west=list(reference=R0_southwest_0,remove_10=R0_southwest_0.1,remove_20=R0_southwest_0.2,remove_30=R0_southwest_0.3,remove_40=R0_southwest_0.4,remove_50=R0_southwest_0.5,remove_60=R0_southwest_0.6,remove_70=R0_southwest_0.7,remove_80=R0_southwest_0.8,remove_90=R0_southwest_0.9,remove_100=R0_southwest_1)
R0_england=list(reference=england_R0_0,remove_10=england_R0_0.1,remove_20=england_R0_0.2,remove_30=england_R0_0.3,remove_40=england_R0_0.4,remove_50=england_R0_0.5,remove_60=england_R0_0.6,remove_70=england_R0_0.7,remove_80=england_R0_0.8,remove_90=england_R0_0.9,remove_100=england_R0_1)

R0=list(east_of_england=R0_east_of_england,london=R0_london,midlands=R0_midlands,north_east_and_yorkshire=R0_north_east_and_yorksire,north_west=R0_north_west,south_east=R0_south_east,south_west=R0_south_west,england=R0_england)

#parameter data====

pars_east<-list(reference=fit_east_0$samples$pars,remove_10=fit_east_0.1$samples$pars,remove_20=fit_east_0.2$samples$pars,remove_30=fit_east_0.3$samples$pars)
pars_london<-list(reference=fit_london_0$samples$pars,remove_10=fit_london_0.1$samples$pars,remove_20=fit_london_0.2$samples$pars,remove_30=fit_london_0.3$samples$pars)
pars_midlands<-list(reference=fit_midlands_0$samples$pars,remove_10=fit_midlands_0.1$samples$pars,remove_20=fit_midlands_0.2$samples$pars,remove_30=fit_midlands_0.3$samples$pars)
pars_northeast<-list(reference=fit_northeast_0$samples$pars,remove_10=fit_northeast_0.1$samples$pars,remove_20=fit_northeast_0.2$samples$pars,remove_30=fit_northeast_0.3$samples$pars)
pars_northwest<-list(reference=fit_northwest_0$samples$pars,remove_10=fit_northwest_0.1$samples$pars,remove_20=fit_northwest_0.2$samples$pars,remove_30=fit_northwest_0.3$samples$pars)
pars_southeast<-list(reference=fit_southeast_0$samples$pars,remove_10=fit_southeast_0.1$samples$pars,remove_20=fit_southeast_0.2$samples$pars,remove_30=fit_southeast_0.3$samples$pars)
pars_southwest<-list(reference=fit_southwest_0$samples$pars,remove_10=fit_southwest_0.1$samples$pars,remove_20=fit_southwest_0.2$samples$pars,remove_30=fit_southwest_0.3$samples$pars)
pars_east2<-list(remove_40=fit_east_0.4$samples$pars,remove_50=fit_east_0.5$samples$pars)
pars_london2<-list(remove_40=fit_london_0.4$samples$pars,remove_50=fit_london_0.5$samples$pars)
pars_midlands2<-list(remove_40=fit_midlands_0.4$samples$pars,remove_50=fit_midlands_0.5$samples$pars)
pars_northeast2<-list(remove_40=fit_northeast_0.4$samples$pars,remove_50=fit_northeast_0.5$samples$pars)
pars_northwest2<-list(remove_40=fit_northwest_0.4$samples$pars,remove_50=fit_northwest_0.5$samples$pars)
pars_southeast2<-list(remove_40=fit_southeast_0.4$samples$pars,remove_50=fit_southeast_0.5$samples$pars)
pars_southwest2<-list(remove_40=fit_southwest_0.4$samples$pars,remove_50=fit_southwest_0.5$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)
pars_east2<-list(remove_60=fit_east_0.6$samples$pars,remove_70=fit_east_0.7$samples$pars,remove_80=fit_east_0.8$samples$pars)
pars_london2<-list(remove_60=fit_london_0.6$samples$pars,remove_70=fit_london_0.7$samples$pars,remove_80=fit_london_0.8$samples$pars)
pars_midlands2<-list(remove_60=fit_midlands_0.6$samples$pars,remove_70=fit_midlands_0.7$samples$pars,remove_80=fit_midlands_0.8$samples$pars)
pars_northeast2<-list(remove_60=fit_northeast_0.6$samples$pars,remove_70=fit_northeast_0.7$samples$pars,remove_80=fit_northeast_0.8$samples$pars)
pars_northwest2<-list(remove_60=fit_northwest_0.6$samples$pars,remove_70=fit_northwest_0.7$samples$pars,remove_80=fit_northwest_0.8$samples$pars)
pars_southeast2<-list(remove_60=fit_southeast_0.6$samples$pars,remove_70=fit_southeast_0.7$samples$pars,remove_80=fit_southeast_0.8$samples$pars)
pars_southwest2<-list(remove_60=fit_southwest_0.6$samples$pars,remove_70=fit_southwest_0.7$samples$pars,remove_80=fit_southwest_0.8$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)
pars_east2<-list(remove_90=fit_east_0.9$samples$pars,remove_100=fit_east_1$samples$pars)
pars_london2<-list(remove_90=fit_london_0.9$samples$pars,remove_100=fit_london_1$samples$pars)
pars_midlands2<-list(remove_90=fit_midlands_0.9$samples$pars,remove_100=fit_midlands_1$samples$pars)
pars_northeast2<-list(remove_90=fit_northeast_0.9$samples$pars,remove_100=fit_northeast_1$samples$pars)
pars_northwest2<-list(remove_90=fit_northwest_0.9$samples$pars,remove_100=fit_northwest_1$samples$pars)
pars_southeast2<-list(remove_90=fit_southeast_0.9$samples$pars,remove_100=fit_southeast_1$samples$pars)
pars_southwest2<-list(remove_90=fit_southwest_0.9$samples$pars,remove_100=fit_southwest_1$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)

pars<-list(east_of_england=pars_east,london=pars_london,midlands=pars_midlands,north_east_and_yorkshire=pars_northeast,north_west=pars_northwest,south_east=pars_southeast,south_west=pars_southwest)

#diagnostics data====

diagnostics_0 <- readRDS("inputs/0/diagnostics.rds")
diagnostics_0.1 <- readRDS("inputs/0.1/diagnostics.rds")
diagnostics_0.2 <- readRDS("inputs/0.2/diagnostics.rds")
diagnostics_0.3 <- readRDS("inputs/0.3/diagnostics.rds")
diagnostics_0.4 <- readRDS("inputs/0.4/diagnostics.rds")
diagnostics_0.5 <- readRDS("inputs/0.5/diagnostics.rds")
diagnostics_0.6 <- readRDS("inputs/0.6/diagnostics.rds")
diagnostics_0.7 <- readRDS("inputs/0.7/diagnostics.rds")
diagnostics_0.8 <- readRDS("inputs/0.8/diagnostics.rds")
diagnostics_0.9 <- readRDS("inputs/0.9/diagnostics.rds")
diagnostics_1 <- readRDS("inputs/1/diagnostics.rds")

diagnostics<-list(reference=diagnostics_0,remove_10=diagnostics_0.1,remove_20=diagnostics_0.2,remove_30=diagnostics_0.3,remove_40=diagnostics_0.4,remove_50=diagnostics_0.5,remove_60=diagnostics_0.6,remove_70=diagnostics_0.7,remove_80=diagnostics_0.8,remove_90=diagnostics_0.9,remove_100=diagnostics_1)

#====

if (!dir.exists('outputs')) {
  dir.create('outputs')
}

#save data====

saveRDS(pars,file = 'outputs/pars.rds')
saveRDS(ifr_continuous,file = 'outputs/ifr_continuous.rds')
saveRDS(ihr_continuous,file = 'outputs/ihr_continuous.rds')
saveRDS(hfr_continuous,file = 'outputs/hfr_continuous.rds')
#saveRDS(Rt,file = 'outputs/Rt.rds')
saveRDS(R0,file = 'outputs/R0.rds')
saveRDS(IFR,file = 'outputs/IFR.rds')
saveRDS(HFR,file = 'outputs/HFR.rds')
saveRDS(IHR,file = 'outputs/IHR.rds')
saveRDS(diagnostics,file = "outputs/diagnostics.rds")
saveRDS(Rt_eff,file="outputs/Rt_eff.rds")

#====
regions <- sircovid::regions("england")

#ifr continuous KLD====

KL_ifr_continuous=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_ifr_continuous)=c(regions,"england")
for (r in c(regions,"england")){
  KL_ifr_continuous[[r]]= matrix(nrow = length(names(ifr_continuous[[r]])),ncol=dim(ifr_continuous[[r]]$reference)[1])
  colnames(KL_ifr_continuous[[r]]) = paste0("date", c(0,76:622))
  rownames(KL_ifr_continuous[[r]]) = names(ifr_continuous[[r]])
  for (i in 1:548){
    for (dataOff in names(ifr_continuous[[r]])){
      if (dataOff=='reference'){
        KL_ifr_continuous[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(ifr_continuous[[r]]$reference[i,],ifr_continuous[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        if(anyNA(temp)){
          KL <- 0
        }else{
          reference <- density(temp[,1])
          changed <- density(temp[,2])
          common_support <- sort(union(reference$x,changed$x))
          reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
          changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
          reference_interp[is.na(reference_interp)] <- 0
          changed_interp[is.na(changed_interp)] <- 0
          
          X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
          #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
          KL<-philentropy::KL(X)
        }
        KL_ifr_continuous[[r]][dataOff,i] <- KL
      }
    }
  }
}
saveRDS(KL_ifr_continuous,file = 'outputs/KL_ifr_continuous.rds')

for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  
  temp=melt(KL_ifr_continuous[[r]])
  names(temp)=c("change_rate","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,change_Rate:=factor(change_rate,rownames(KL_ifr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=change_rate,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective IFR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "remove_10" = "remove 10%", "remove_20" = "remove 20%", "remove_30" = "remove 30%", "remove_40" = "remove 40%", "remove_50" = "remove 50%", "remove_60" = "remove 60%", "remove_70" = "remove 70%", "remove_80" = "remove 80%", "remove_90" = "remove 90%", "remove_100" = "remove 100%"))
  ggsave(paste0("outputs/ifr heatmap of ",r,".png"),p,dpi=600,width=15,height=4.5)
}

#ihr continuous KLD====

KL_ihr_continuous=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_ihr_continuous)=c(regions,"england")
for (r in c(regions,"england")){
  KL_ihr_continuous[[r]]= matrix(nrow = length(names(ihr_continuous[[r]])),ncol=dim(ihr_continuous[[r]]$reference)[1])
  colnames(KL_ihr_continuous[[r]]) = paste0("date", c(0,76:622))
  rownames(KL_ihr_continuous[[r]]) = names(ihr_continuous[[r]])
  for (i in 1:548){
    for (dataOff in names(ihr_continuous[[r]])){
      if (dataOff=='reference'){
        KL_ihr_continuous[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(ihr_continuous[[r]]$reference[i,],ihr_continuous[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        if(anyNA(temp)){
          KL<-0
        }else{
          reference <- density(temp[,1])
          changed <- density(temp[,2])
          common_support <- sort(union(reference$x,changed$x))
          reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
          changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
          reference_interp[is.na(reference_interp)] <- 0
          changed_interp[is.na(changed_interp)] <- 0
          
          X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
          #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
          KL<-philentropy::KL(X)
        }
        
        KL_ihr_continuous[[r]][dataOff,i] <- KL
      }
    }
  }
}
saveRDS(KL_ihr_continuous,file = 'outputs/KL_ihr_continuous.rds')

for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  
  temp=melt(KL_ihr_continuous[[r]])
  names(temp)=c("change_rate","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,change_rate:=factor(change_rate,rownames(KL_ihr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=change_rate,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective IHR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "remove_10" = "remove 10%", "remove_20" = "remove 20%", "remove_30" = "remove 30%", "remove_40" = "remove 40%", "remove_50" = "remove 50%", "remove_60" = "remove 60%", "remove_70" = "remove 70%", "remove_80" = "remove 80%", "remove_90" = "remove 90%", "remove_100" = "remove 100%"))
  ggsave(paste0("outputs/ihr heatmap of ",r,".png"),p,dpi=600,width=15,height=4.5)
}

#hfr continuous KLD====

KL_hfr_continuous=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_hfr_continuous)=c(regions,"england")
for (r in c(regions,"england")){
  KL_hfr_continuous[[r]]= matrix(nrow = length(names(hfr_continuous[[r]])),ncol=dim(hfr_continuous[[r]]$reference)[1])
  colnames(KL_hfr_continuous[[r]]) = paste0("date", c(0,76:622))
  rownames(KL_hfr_continuous[[r]]) = names(hfr_continuous[[r]])
  for (i in 1:dim(hfr_continuous[[r]]$reference)[1]){
    for (dataOff in names(hfr_continuous[[r]])){
      if (dataOff=='reference'){
        KL_hfr_continuous[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(hfr_continuous[[r]]$reference[i,],hfr_continuous[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        if(anyNA(temp)){
          KL<-0
        }else{
          reference <- density(temp[,1])
          changed <- density(temp[,2])
          common_support <- sort(union(reference$x,changed$x))
          reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
          changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
          reference_interp[is.na(reference_interp)] <- 0
          changed_interp[is.na(changed_interp)] <- 0
          
          X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
          #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
          KL<-philentropy::KL(X)
        }
        KL_hfr_continuous[[r]][dataOff,i] <- KL
      }
    }
  }
}
saveRDS(KL_hfr_continuous,file = 'outputs/KL_hfr_continuous.rds')

for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  
  temp=melt(KL_hfr_continuous[[r]])
  names(temp)=c("change_rate","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,change_rate:=factor(change_rate,rownames(KL_hfr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=change_rate,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective HFR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "remove_10" = "remove 10%", "remove_20" = "remove 20%", "remove_30" = "remove 30%", "remove_40" = "remove 40%", "remove_50" = "remove 50%", "remove_60" = "remove 60%", "remove_70" = "remove 70%", "remove_80" = "remove 80%", "remove_90" = "remove 90%", "remove_100" = "remove 100%"))
  ggsave(paste0("outputs/hfr heatmap of ",r,".png"),p,dpi=600,width=15,height=4.5)
}

#Rt_eff continuous KLD====

KL_Rt_eff=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_Rt_eff)=c(regions,"england")
for (r in c(regions,"england")){
  KL_Rt_eff[[r]]= matrix(nrow = length(names(Rt_eff[[r]])),ncol=dim(Rt_eff[[r]]$reference)[1])
  colnames(KL_Rt_eff[[r]]) = paste0("date", c(0,76:622))
  rownames(KL_Rt_eff[[r]]) = names(Rt_eff[[r]])
  for (i in 1:dim(Rt_eff[[r]]$reference)[1]){
    for (dataOff in names(Rt_eff[[r]])){
      if (dataOff=='reference'){
        KL_Rt_eff[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(Rt_eff[[r]]$reference[i,],Rt_eff[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        if(anyNA(temp)){
          KL<-0
        }else{
          reference <- density(temp[,1])
          changed <- density(temp[,2])
          common_support <- sort(union(reference$x,changed$x))
          reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
          changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
          reference_interp[is.na(reference_interp)] <- 0
          changed_interp[is.na(changed_interp)] <- 0
          
          X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
          #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
          KL<-philentropy::KL(X)
        }
        
        KL_Rt_eff[[r]][dataOff,i] <- KL
      }
    }
  }
}
saveRDS(KL_Rt_eff,file = 'outputs/KL_Rt_eff.rds')

for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  temp=melt(KL_Rt_eff[[r]])
  names(temp)=c("change_rate","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,change_rate:=factor(change_rate,rownames(KL_Rt_eff[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=change_rate,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective Rt in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "remove_10" = "remove 10%", "remove_20" = "remove 20%", "remove_30" = "remove 30%", "remove_40" = "remove 40%", "remove_50" = "remove 50%", "remove_60" = "remove 60%", "remove_70" = "remove 70%", "remove_80" = "remove 80%", "remove_90" = "remove 90%", "remove_100" = "remove 100%"))
  ggsave(paste0("outputs/Rt_eff heatmap of ",r,".png"),p,dpi=600,width=15,height=4.5)
}

#ifr emergency3 KLD&density====

IFR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(IFR_density_plots)=c(regions,"england")
KL_IFR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_IFR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_IFR[[r]]= matrix(nrow = length(names(IFR[[r]])),ncol=3)
  colnames(KL_IFR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_IFR[[r]]) = names(IFR[[r]])
  IFR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(IFR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  IFR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(IFR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")
  
  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    IFR_density_plots[[r]][[tempname]] <- as.list(matrix(nrow=length(names(IFR[[r]])),ncol=1))
    names(IFR_density_plots[[r]][[tempname]]) = names(IFR[[r]])
    for (dataOff in names(IFR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(IFR[[r]]$reference[i,])
        colnames(temp)=tempname
        IFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_IFR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(IFR[[r]]$reference[i,],IFR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        
        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0
        
        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
        KL<-philentropy::KL(X)
        
        KL_IFR[[r]][dataOff,i] <- KL
        
        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        IFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=IFR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    IFR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' IFR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(IFR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(IFR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(IFR_density_plots,file = 'outputs/IFR_density_plots.rds')
graphics.off()
saveRDS(KL_IFR,file = 'outputs/KL_IFR.rds')

#ihr emergency3 KLD&density====

IHR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(IHR_density_plots)=c(regions,"england")
KL_IHR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_IHR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_IHR[[r]]= matrix(nrow = length(names(IHR[[r]])),ncol=3)
  colnames(KL_IHR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_IHR[[r]]) = names(IHR[[r]])
  IHR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(IHR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  IHR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(IHR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")
  
  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    IHR_density_plots[[r]][[tempname]] = as.list(matrix(nrow=length(names(IHR[[r]])),ncol=1))
    names(IHR_density_plots[[r]][[tempname]]) = names(IHR[[r]])
    for (dataOff in names(IHR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(IHR[[r]]$reference[i,])
        colnames(temp)=tempname
        IHR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_IHR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(IHR[[r]]$reference[i,],IHR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        
        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0
        
        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-philentropy::KL(X)
        
        KL_IHR[[r]][dataOff,i] <- KL
        
        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        IHR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=IHR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    IHR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' IHR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(IHR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(IHR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(IHR_density_plots,file = 'outputs/IHR_density_plots.rds')
graphics.off()
saveRDS(KL_IHR,file = 'outputs/KL_IHR.rds')

#hfr emergency3 KLD&density====

HFR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(HFR_density_plots)=c(regions,"england")
KL_HFR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_HFR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_HFR[[r]]= matrix(nrow = length(names(HFR[[r]])),ncol=3)
  colnames(KL_HFR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_HFR[[r]]) = names(HFR[[r]])
  HFR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(HFR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  HFR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(HFR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")
  
  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    HFR_density_plots[[r]][[tempname]] = as.list(matrix(nrow=length(names(HFR[[r]])),ncol=1))
    names(HFR_density_plots[[r]][[tempname]]) = names(HFR[[r]])
    for (dataOff in names(HFR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(HFR[[r]]$reference[i,])
        colnames(temp)=tempname
        HFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_HFR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(HFR[[r]]$reference[i,],HFR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)
        
        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0
        
        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-philentropy::KL(X)
        
        KL_HFR[[r]][dataOff,i] <- KL
        
        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        HFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=HFR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    HFR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' HFR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(HFR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(HFR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(HFR_density_plots,file = 'outputs/HFR_density_plots.rds')
graphics.off()
saveRDS(KL_HFR,file = 'outputs/KL_HFR.rds')

#R0 KLD&density====

R0_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(R0_density_plots)=c(regions,"england")
KL_R0=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_R0)=c(regions,"england")
for (r in c(regions,"england")){
  KL_R0[[r]]= matrix(nrow = length(names(R0[[r]])),ncol=length(names(R0[[r]]$reference)))
  colnames(KL_R0[[r]]) = names(R0[[r]]$reference)
  rownames(KL_R0[[r]]) = names(R0[[r]])
  
  R0_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(R0_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  R0_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(R0_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")
  
  for (variant in names(R0[[r]]$reference)){
    R0_density_plots[[r]][[variant]] = as.list(matrix(nrow=length(names(R0[[r]])),ncol=1))
    names(R0_density_plots[[r]][[variant]]) = names(R0[[r]])
    
    for (dataOff in names(R0[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(R0[[r]]$reference)
        temp=as.data.frame(temp[,colnames(temp) %in% variant])
        colnames(temp)=variant
        R0_density_plots[[r]][[variant]][[dataOff]] <-
          ggplot(temp, aes(get(variant)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(variant)
        KL_R0[[r]][dataOff,variant] <- 0
      }else{
        temp=data.frame(R0[[r]]$reference[[variant]],R0[[r]][[dataOff]][[variant]])
        colnames(temp)=c('reference',dataOff)
        
        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0
        
        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-philentropy::KL(X)
        
        KL_R0[[r]][dataOff,variant] <- KL
        
        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        R0_density_plots[[r]][[variant]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(variant)
      }
    }
    title=paste(variant,'in', r)
    grided<-grid.arrange(grobs=R0_density_plots[[r]][[variant]],newpage = TRUE)
    R0_density_plots[[r]]$grided_plots[[variant]]=grided
    graphics.off()
  }
  pdf(paste0('outputs/',r,' R0 distribution.pdf'),width =11.69, height =8.27)
  for (i in names(R0_density_plots[[r]]$grided_plots)){
    print(grid.arrange(R0_density_plots[[r]]$grided_plots[[i]]))
  }
  dev.off()
  graphics.off()
}
saveRDS(R0_density_plots,file = 'outputs/R0_density_plots.rds')
graphics.off()
saveRDS(KL_R0,file = 'outputs/KL_R0.rds')

#parameter KLD&density====

pars_density_plots=as.list(matrix(nrow = length(regions), ncol=1))
names(pars_density_plots)=regions
KL_pars=as.list(matrix(nrow = length(regions), ncol=1))
names(KL_pars)=regions
for (r in regions){
  pars_density_plots[[r]]= as.list(matrix(nrow = length(colnames(pars[[r]]$reference))+1,ncol=1))
  names(pars_density_plots[[r]]) = c('grided_plots',colnames(pars[[r]]$reference))
  pars_density_plots[[r]]$grided_plots = as.list(matrix(nrow=length(colnames(pars[[r]]$reference)),ncol=1))
  names(pars_density_plots[[r]]$grided_plots) = colnames(pars[[r]]$reference)
  KL_pars[[r]]= matrix(nrow = length(names(pars[[r]])),ncol=length(colnames(pars[[r]]$reference)))
  colnames(KL_pars[[r]]) = colnames(pars[[r]]$reference)
  rownames(KL_pars[[r]]) = names(pars[[r]])
  
  for (parameter in colnames(pars[[r]]$reference)){
    pars_density_plots[[r]][[parameter]] = as.list(matrix(nrow=length(names(pars[[r]])),ncol=1))
    names(pars_density_plots[[r]][[parameter]]) = names(pars[[r]])

    for (dataOff in names(pars[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(pars[[r]]$reference)
        temp=as.data.frame(temp[,colnames(temp) %in% parameter])
        colnames(temp)=parameter
        pars_density_plots[[r]][[parameter]][[dataOff]] <-
          ggplot(temp, aes(get(parameter)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(parameter)
        KL_pars[[r]][dataOff,parameter] <- 0
      }else{
        temp=data.frame(pars[[r]]$reference[,colnames(pars[[r]]$reference) %in% parameter],pars[[r]][[dataOff]][,colnames(pars[[r]][[dataOff]]) %in% parameter])
        colnames(temp)=c('reference',dataOff)
        
        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        # X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
        KL<-philentropy::KL(X)
        
        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        pars_density_plots[[r]][[parameter]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(parameter)
        KL_pars[[r]][dataOff,parameter] <- KL
      }
    }
    title=paste(parameter,'in', r)
    grided<-grid.arrange(grobs=pars_density_plots[[r]][[parameter]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    pars_density_plots[[r]]$grided_plots[[parameter]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' parameter distribution.pdf'),width =11.69, height =8.27)
  for (i in names(pars_density_plots[[r]]$grided_plots)){
    print(grid.arrange(pars_density_plots[[r]]$grided_plots[[i]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(pars_density_plots,file = 'outputs/pars_density_plots.rds')
graphics.off()
saveRDS(KL_pars,file = 'outputs/KL_pars.rds')

#heatmap of parameter; IHR IFR HFR emergency3; R0====

pdf(paste0("outputs/KL divergence heatmap of parameters.pdf"),width =11.69, height =8.27)
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  
  data_melt <- melt(KL_pars[[r]])
  tmp=paste0("KL divergence heatmap of parameters in ",R)
  ggp <- ggplot(data_melt, aes(Var1, Var2)) +                         
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(tmp)+
    theme(axis.text.x = element_text(angle = 50, vjust = 0.5))+
    scale_x_discrete(labels = c("reference" = "reference", "remove_10" = "remove 10%", "remove_20" = "remove 20%", "remove_30" = "remove 30%", "remove_40" = "remove 40%", "remove_50" = "remove 50%", "remove_60" = "remove 60%", "remove_70" = "remove 70%", "remove_80" = "remove 80%", "remove_90" = "remove 90%", "remove_100" = "remove 100%"))+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))
  #agg_png(paste0("outputs2/KL divergence heatmap of parameters in ",r,".png"),res=600,height = 8000,width = 4500)
  print(ggp) 
  #dev.off()
}
dev.off()

pdf(paste0("outputs/KL divergence heatmap of IHR,IFR,HFR.pdf"),width =11, height =7)
for (r in names(KL_IFR)){
  data_melt <- melt(KL_IFR[[r]])
  tmp=paste0("IFR in ",r)
  ggp_IFR <- ggplot(data_melt, aes(Var2, Var1)) +                         
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
    ggtitle(tmp)+
    guides(fill = FALSE)+
    coord_fixed() 
  data_melt <- melt(KL_IHR[[r]])
  tmp=paste0("IHR in ",r)
  ggp_IHR <- ggplot(data_melt, aes(Var2, Var1)) +                         
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(tmp)+
    guides(fill = FALSE)+
    coord_fixed() 
  data_melt <- melt(KL_HFR[[r]])
  tmp=paste0("HFR in ",r)
  ggp_HFR <- ggplot(data_melt, aes(Var2, Var1)) +                         
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
    ggtitle(tmp)+
    coord_fixed() 
  grid.arrange(ggp_IHR, ggp_IFR, ggp_HFR, ncol= 3,top=NULL)
}
dev.off()

ggp=as.list(matrix(nrow = length(names(KL_R0)),ncol=1))
names(ggp)=names(KL_R0)
for (r in names(KL_R0)){
  if (r==names(KL_R0)[1]){
    data_melt <- melt(KL_R0[[r]])
    tmp=paste0("R0 in ",r)
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +                         
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
      ggtitle(r)+
      coord_fixed()+
      guides(fill = FALSE) 
  }else if(r==names(KL_R0)[8]){
    data_melt <- melt(KL_R0[[r]])
    tmp=paste0("R0 in ",r)
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +                         
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
      ggtitle(r)+
      coord_fixed() 
  }else{
    data_melt <- melt(KL_R0[[r]])
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +                         
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
      ggtitle(r)+
      coord_fixed()+ 
      guides(fill = FALSE) 
  }
}
p<-grid.arrange(grobs=ggp,ncol=length(names(ggp)),top=NULL)
ggsave(paste0("outputs/KL divergence heatmap of R0.png"),p,dpi=600,width=21,height=4.5)
dev.off()


#R0 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(1.5,3.5,2.0,2.5,3.0)
    }else if(i==2){
      ticks=c(3,5.5,3.65,4.25,4.85)
    }else{
      ticks=c(5,10,6.25,7.5,8.5)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 11, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(R0[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(R0[[r]]), levels = names(R0[[r]]))
    for (dataOff in names(R0$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(R0[[r]][[dataOff]][[variant]])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_R0[[r]][dataOff,variant]
      temp=quantile(R0[[r]][[dataOff]][[variant]],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }
    
    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8, 
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Community deaths`=c("reference","remove 10%","remove 20%","remove 30%","remove 40%","remove 50%","remove 60%","remove 70%","remove 80%","remove 90%","remove 100%"),Mean=sprintf("%.3f",mean),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf("%.3f",lower_bound),sprintf("%.3f",upper_bound),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 2.60,
                     # xlim=c(2.35,2.85),
                     # ticks_at=c(2.475,2.60,2.725),
                     # ref_line = 4.25,
                     # xlim=c(4,4.5),
                     # ticks_at=c(4.125,4.25,4.375),
                     # ref_line = 7.15,
                     # xlim=c(6.9,7.4),
                     # ticks_at=c(7.025,7.15,7.275),
                     theme = tm,
                     xlab = paste("R0 of",variant),
                     title = paste("R0 of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=14,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/R0 of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/R0 forest plots.rds")

#IFR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.0045,0.013,0.007,0.009,0.011)
    }else if(i==2){
      ticks=c(0.008,0.04,0.016,0.024,0.032)
    }else{
      ticks=c(0.0075,0.045,0.017,0.026,0.038)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 11, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(IFR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(IFR[[r]]), levels = names(IFR[[r]]))
    for (dataOff in names(IFR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(IFR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_IFR[[r]][dataOff,variant]
      temp=quantile(IFR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }
    
    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8, 
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Community deaths`=c("reference","remove 10%","remove 20%","remove 30%","remove 40%","remove 50%","remove 60%","remove 70%","remove 80%","remove 90%","remove 100%"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.018,
                     # xlim=c(0.0075,0.028),
                     # ticks_at=c(0.013,0.018,0.023),
                     theme = tm,
                     xlab = paste("IFR of",variant),
                     title = paste("Intrinsic IFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=14,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/IFR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/IFR forest plots.rds")

#IHR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.0185,0.0325,0.022,0.025,0.029)
    }else if(i==2){
      ticks=c(0.0225,0.049,0.029,0.036,0.043)
    }else{
      ticks=c(0.029,0.054,0.036,0.042,0.048)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 11, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(IHR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(IHR[[r]]), levels = names(IHR[[r]]))
    for (dataOff in names(IHR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(IHR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_IHR[[r]][dataOff,variant]
      temp=quantile(IHR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }
    
    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8, 
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Community deaths`=c("reference","remove 10%","remove 20%","remove 30%","remove 40%","remove 50%","remove 60%","remove 70%","remove 80%","remove 90%","remove 100%"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.035,
                     # xlim=c(0.022,0.048),
                     # ticks_at=c(0.028,0.035,0.042),
                     theme = tm,
                     xlab = paste("IHR of",variant),
                     title = paste("Intrinsic IHR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=14,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/IHR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/IHR forest plots.rds")


#HFR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.20,0.36,0.24,0.280,0.32)
    }else if(i==2){
      ticks=c(0.18,0.75,0.32,0.46,0.60)
    }else{
      ticks=c(0.15,0.7,0.287,0.425,0.563)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 11, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(HFR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(HFR[[r]]), levels = names(HFR[[r]]))
    for (dataOff in names(HFR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(HFR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_HFR[[r]][dataOff,variant]
      temp=quantile(HFR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }
    
    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8, 
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Community deaths`=c("reference","remove 10%","remove 20%","remove 30%","remove 40%","remove 50%","remove 60%","remove 70%","remove 80%","remove 90%","remove 100%"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.36,
                     # xlim=c(0.20,0.52),
                     # ticks_at=c(0.28,0.36,0.44),
                     theme = tm,
                     xlab = paste("HFR of",variant),
                     title = paste("Intrinsic HFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=14,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/HFR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/HFR forest plots.rds")

#====
