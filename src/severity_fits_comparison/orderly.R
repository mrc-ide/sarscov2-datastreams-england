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
  c("inputs/original/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/original/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/original/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/original/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/original/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/original/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/original/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/original/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/original/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/original/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/original/england_R0.rds"="outputs/R0.rds",
    "inputs/original/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_hosp" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/deaths_hosp/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/deaths_hosp/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/deaths_hosp/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/deaths_hosp/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/deaths_hosp/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/deaths_hosp/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/deaths_hosp/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/deaths_hosp/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/deaths_hosp/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/deaths_hosp/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/deaths_hosp/england_R0.rds"="outputs/R0.rds",
    "inputs/deaths_hosp/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "deaths_comm" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/deaths_comm/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/deaths_comm/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/deaths_comm/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/deaths_comm/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/deaths_comm/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/deaths_comm/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/deaths_comm/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/deaths_comm/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/deaths_comm/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/deaths_comm/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/deaths_comm/england_R0.rds"="outputs/R0.rds",
    "inputs/deaths_comm/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "icu" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/icu/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/icu/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/icu/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/icu/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/icu/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/icu/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/icu/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/icu/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/icu/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/icu/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/icu/england_R0.rds"="outputs/R0.rds",
    "inputs/icu/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "general" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/general/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/general/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/general/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/general/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/general/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/general/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/general/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/general/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/general/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/general/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/general/england_R0.rds"="outputs/R0.rds",
    "inputs/general/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "hosp" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/hosp/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/hosp/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/hosp/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/hosp/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/hosp/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/hosp/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/hosp/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/hosp/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/hosp/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/hosp/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/hosp/england_R0.rds"="outputs/R0.rds",
    "inputs/hosp/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "all_admission" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/all_admission/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/all_admission/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/all_admission/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/all_admission/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/all_admission/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/all_admission/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/all_admission/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/all_admission/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/all_admission/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/all_admission/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/all_admission/england_R0.rds"="outputs/R0.rds",
    "inputs/all_admission/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "pillar2" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/pillar2/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/pillar2/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/pillar2/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/pillar2/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/pillar2/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/pillar2/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/pillar2/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/pillar2/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/pillar2/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/pillar2/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/pillar2/england_R0.rds"="outputs/R0.rds",
    "inputs/pillar2/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "ons" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/ons/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/ons/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/ons/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/ons/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/ons/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/ons/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/ons/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/ons/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/ons/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/ons/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/ons/england_R0.rds"="outputs/R0.rds",
    "inputs/ons/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "react" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/react/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/react/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/react/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/react/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/react/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/react/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/react/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/react/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/react/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/react/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/react/england_R0.rds"="outputs/R0.rds",
    "inputs/react/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "strain" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/strain/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/strain/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/strain/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/strain/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/strain/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/strain/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/strain/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/strain/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/strain/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/strain/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/strain/england_R0.rds"="outputs/R0.rds",
    "inputs/strain/england/fit.rds"="outputs/england_severity.rds"))
orderly2::orderly_dependency(
  "severity_fits_combined",
  'latest(parameter:data_changed == "sero" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate==1)',
  c("inputs/sero/east_of_england/fit.rds" = "regional_results/east_of_england/fit.rds",
    "inputs/sero/london/fit.rds" = "regional_results/london/fit.rds",
    "inputs/sero/midlands/fit.rds" = "regional_results/midlands/fit.rds",
    "inputs/sero/north_east_and_yorkshire/fit.rds" = "regional_results/north_east_and_yorkshire/fit.rds",
    "inputs/sero/north_west/fit.rds" = "regional_results/north_west/fit.rds",
    "inputs/sero/south_east/fit.rds" = "regional_results/south_east/fit.rds",
    "inputs/sero/south_west/fit.rds" = "regional_results/south_west/fit.rds",
    "inputs/sero/Rt_england.rds"="regional_results/Rt_england.rds",
    "inputs/sero/diagnostics.rds"="outputs/diagnostics.rds",
    "inputs/sero/england_intrinsic_severity.rds"="outputs/england_intrinsic_severity.rds",
    "inputs/sero/england_R0.rds"="outputs/R0.rds",
    "inputs/sero/england/fit.rds"="outputs/england_severity.rds"))

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

fit_east_original <- readRDS("inputs/original/east_of_england/fit.rds")
fit_london_original <- readRDS("inputs/original/london/fit.rds")
fit_midlands_original <- readRDS("inputs/original/midlands/fit.rds")
fit_northeast_original <- readRDS("inputs/original/north_east_and_yorkshire/fit.rds")
fit_northwest_original <- readRDS("inputs/original/north_west/fit.rds")
fit_southeast_original <- readRDS("inputs/original/south_east/fit.rds")
fit_southwest_original <- readRDS("inputs/original/south_west/fit.rds")
fit_england_original <- readRDS("inputs/original/england/fit.rds")

fit_east_deaths_hosp <- readRDS("inputs/deaths_hosp/east_of_england/fit.rds")
fit_london_deaths_hosp <- readRDS("inputs/deaths_hosp/london/fit.rds")
fit_midlands_deaths_hosp <- readRDS("inputs/deaths_hosp/midlands/fit.rds")
fit_northeast_deaths_hosp <- readRDS("inputs/deaths_hosp/north_east_and_yorkshire/fit.rds")
fit_northwest_deaths_hosp <- readRDS("inputs/deaths_hosp/north_west/fit.rds")
fit_southeast_deaths_hosp <- readRDS("inputs/deaths_hosp/south_east/fit.rds")
fit_southwest_deaths_hosp <- readRDS("inputs/deaths_hosp/south_west/fit.rds")
fit_england_deaths_hosp <- readRDS("inputs/deaths_hosp/england/fit.rds")

fit_east_deaths_comm <- readRDS("inputs/deaths_comm/east_of_england/fit.rds")
fit_london_deaths_comm <- readRDS("inputs/deaths_comm/london/fit.rds")
fit_midlands_deaths_comm <- readRDS("inputs/deaths_comm/midlands/fit.rds")
fit_northeast_deaths_comm <- readRDS("inputs/deaths_comm/north_east_and_yorkshire/fit.rds")
fit_northwest_deaths_comm <- readRDS("inputs/deaths_comm/north_west/fit.rds")
fit_southeast_deaths_comm <- readRDS("inputs/deaths_comm/south_east/fit.rds")
fit_southwest_deaths_comm <- readRDS("inputs/deaths_comm/south_west/fit.rds")
fit_england_deaths_comm <- readRDS("inputs/deaths_comm/england/fit.rds")

fit_east_icu <- readRDS("inputs/icu/east_of_england/fit.rds")
fit_london_icu <- readRDS("inputs/icu/london/fit.rds")
fit_midlands_icu <- readRDS("inputs/icu/midlands/fit.rds")
fit_northeast_icu <- readRDS("inputs/icu/north_east_and_yorkshire/fit.rds")
fit_northwest_icu <- readRDS("inputs/icu/north_west/fit.rds")
fit_southeast_icu <- readRDS("inputs/icu/south_east/fit.rds")
fit_southwest_icu <- readRDS("inputs/icu/south_west/fit.rds")
fit_england_icu <- readRDS("inputs/icu/england/fit.rds")

fit_east_general <- readRDS("inputs/general/east_of_england/fit.rds")
fit_london_general <- readRDS("inputs/general/london/fit.rds")
fit_midlands_general <- readRDS("inputs/general/midlands/fit.rds")
fit_northeast_general <- readRDS("inputs/general/north_east_and_yorkshire/fit.rds")
fit_northwest_general <- readRDS("inputs/general/north_west/fit.rds")
fit_southeast_general <- readRDS("inputs/general/south_east/fit.rds")
fit_southwest_general <- readRDS("inputs/general/south_west/fit.rds")
fit_england_general <- readRDS("inputs/general/england/fit.rds")

fit_east_hosp <- readRDS("inputs/hosp/east_of_england/fit.rds")
fit_london_hosp <- readRDS("inputs/hosp/london/fit.rds")
fit_midlands_hosp <- readRDS("inputs/hosp/midlands/fit.rds")
fit_northeast_hosp <- readRDS("inputs/hosp/north_east_and_yorkshire/fit.rds")
fit_northwest_hosp <- readRDS("inputs/hosp/north_west/fit.rds")
fit_southeast_hosp <- readRDS("inputs/hosp/south_east/fit.rds")
fit_southwest_hosp <- readRDS("inputs/hosp/south_west/fit.rds")
fit_england_hosp <- readRDS("inputs/hosp/england/fit.rds")

fit_east_all_admission <- readRDS("inputs/all_admission/east_of_england/fit.rds")
fit_london_all_admission <- readRDS("inputs/all_admission/london/fit.rds")
fit_midlands_all_admission <- readRDS("inputs/all_admission/midlands/fit.rds")
fit_northeast_all_admission <- readRDS("inputs/all_admission/north_east_and_yorkshire/fit.rds")
fit_northwest_all_admission <- readRDS("inputs/all_admission/north_west/fit.rds")
fit_southeast_all_admission <- readRDS("inputs/all_admission/south_east/fit.rds")
fit_southwest_all_admission <- readRDS("inputs/all_admission/south_west/fit.rds")
fit_england_all_admission <- readRDS("inputs/all_admission/england/fit.rds")

fit_east_pillar2 <- readRDS("inputs/pillar2/east_of_england/fit.rds")
fit_london_pillar2 <- readRDS("inputs/pillar2/london/fit.rds")
fit_midlands_pillar2 <- readRDS("inputs/pillar2/midlands/fit.rds")
fit_northeast_pillar2 <- readRDS("inputs/pillar2/north_east_and_yorkshire/fit.rds")
fit_northwest_pillar2 <- readRDS("inputs/pillar2/north_west/fit.rds")
fit_southeast_pillar2 <- readRDS("inputs/pillar2/south_east/fit.rds")
fit_southwest_pillar2 <- readRDS("inputs/pillar2/south_west/fit.rds")
fit_england_pillar2 <- readRDS("inputs/pillar2/england/fit.rds")

fit_east_ons <- readRDS("inputs/ons/east_of_england/fit.rds")
fit_london_ons <- readRDS("inputs/ons/london/fit.rds")
fit_midlands_ons <- readRDS("inputs/ons/midlands/fit.rds")
fit_northeast_ons <- readRDS("inputs/ons/north_east_and_yorkshire/fit.rds")
fit_northwest_ons <- readRDS("inputs/ons/north_west/fit.rds")
fit_southeast_ons <- readRDS("inputs/ons/south_east/fit.rds")
fit_southwest_ons <- readRDS("inputs/ons/south_west/fit.rds")
fit_england_ons <- readRDS("inputs/ons/england/fit.rds")

fit_east_react <- readRDS("inputs/react/east_of_england/fit.rds")
fit_london_react <- readRDS("inputs/react/london/fit.rds")
fit_midlands_react <- readRDS("inputs/react/midlands/fit.rds")
fit_northeast_react <- readRDS("inputs/react/north_east_and_yorkshire/fit.rds")
fit_northwest_react <- readRDS("inputs/react/north_west/fit.rds")
fit_southeast_react <- readRDS("inputs/react/south_east/fit.rds")
fit_southwest_react <- readRDS("inputs/react/south_west/fit.rds")
fit_england_react <- readRDS("inputs/react/england/fit.rds")

fit_east_strain <- readRDS("inputs/strain/east_of_england/fit.rds")
fit_london_strain <- readRDS("inputs/strain/london/fit.rds")
fit_midlands_strain <- readRDS("inputs/strain/midlands/fit.rds")
fit_northeast_strain <- readRDS("inputs/strain/north_east_and_yorkshire/fit.rds")
fit_northwest_strain <- readRDS("inputs/strain/north_west/fit.rds")
fit_southeast_strain <- readRDS("inputs/strain/south_east/fit.rds")
fit_southwest_strain <- readRDS("inputs/strain/south_west/fit.rds")
fit_england_strain <- readRDS("inputs/strain/england/fit.rds")

fit_east_sero <- readRDS("inputs/sero/east_of_england/fit.rds")
fit_london_sero <- readRDS("inputs/sero/london/fit.rds")
fit_midlands_sero <- readRDS("inputs/sero/midlands/fit.rds")
fit_northeast_sero <- readRDS("inputs/sero/north_east_and_yorkshire/fit.rds")
fit_northwest_sero <- readRDS("inputs/sero/north_west/fit.rds")
fit_southeast_sero <- readRDS("inputs/sero/south_east/fit.rds")
fit_southwest_sero <- readRDS("inputs/sero/south_west/fit.rds")
fit_england_sero <- readRDS("inputs/sero/england/fit.rds")

england_R0_original <- readRDS("inputs/original/england_R0.rds")
england_R0_deaths_hosp <- readRDS("inputs/deaths_hosp/england_R0.rds")
england_R0_deaths_comm <- readRDS("inputs/deaths_comm/england_R0.rds")
england_R0_icu <- readRDS("inputs/icu/england_R0.rds")
england_R0_general <- readRDS("inputs/general/england_R0.rds")
england_R0_hosp <- readRDS("inputs/hosp/england_R0.rds")
england_R0_all_admission <- readRDS("inputs/all_admission/england_R0.rds")
england_R0_pillar2 <- readRDS("inputs/pillar2/england_R0.rds")
england_R0_ons <- readRDS("inputs/ons/england_R0.rds")
england_R0_react <- readRDS("inputs/react/england_R0.rds")
england_R0_strain <- readRDS("inputs/strain/england_R0.rds")
england_R0_sero <- readRDS("inputs/sero/england_R0.rds")

england_instrinsic_original <- readRDS("inputs/original/england_intrinsic_severity.rds")
england_instrinsic_deaths_hosp <- readRDS("inputs/deaths_hosp/england_intrinsic_severity.rds")
england_instrinsic_deaths_comm <- readRDS("inputs/deaths_comm/england_intrinsic_severity.rds")
england_instrinsic_icu <- readRDS("inputs/icu/england_intrinsic_severity.rds")
england_instrinsic_general <- readRDS("inputs/general/england_intrinsic_severity.rds")
england_instrinsic_hosp <- readRDS("inputs/hosp/england_intrinsic_severity.rds")
england_instrinsic_all_admission <- readRDS("inputs/all_admission/england_intrinsic_severity.rds")
england_instrinsic_pillar2 <- readRDS("inputs/pillar2/england_intrinsic_severity.rds")
england_instrinsic_ons <- readRDS("inputs/ons/england_intrinsic_severity.rds")
england_instrinsic_react <- readRDS("inputs/react/england_intrinsic_severity.rds")
england_instrinsic_strain <- readRDS("inputs/strain/england_intrinsic_severity.rds")
england_instrinsic_sero <- readRDS("inputs/sero/england_intrinsic_severity.rds")

Rt_england_original <- readRDS("inputs/original/Rt_england.rds")
Rt_england_deaths_hosp <- readRDS("inputs/deaths_hosp/Rt_england.rds")
Rt_england_deaths_comm <- readRDS("inputs/deaths_comm/Rt_england.rds")
Rt_england_icu <- readRDS("inputs/icu/Rt_england.rds")
Rt_england_general <- readRDS("inputs/general/Rt_england.rds")
Rt_england_hosp <- readRDS("inputs/hosp/Rt_england.rds")
Rt_england_all_admission <- readRDS("inputs/all_admission/Rt_england.rds")
Rt_england_pillar2 <- readRDS("inputs/pillar2/Rt_england.rds")
Rt_england_ons <- readRDS("inputs/ons/Rt_england.rds")
Rt_england_react <- readRDS("inputs/react/Rt_england.rds")
Rt_england_strain <- readRDS("inputs/strain/Rt_england.rds")
Rt_england_sero <- readRDS("inputs/sero/Rt_england.rds")

#ifr continuous data====

ifr_east=list(reference=fit_east_original$severity$ifr,hospital_deaths=fit_east_deaths_hosp$severity$ifr,community_deaths=fit_east_deaths_comm$severity$ifr,icu_occupancy=fit_east_icu$severity$ifr,general_bed_occupancy=fit_east_general$severity$ifr,hospital_bed_occupancy=fit_east_hosp$severity$ifr,hospital_admissions=fit_east_all_admission$severity$ifr,pillar2=fit_east_pillar2$severity$ifr,ons_pcr_testing=fit_east_ons$severity$ifr,react=fit_east_react$severity$ifr,strain=fit_east_strain$severity$ifr,serology=fit_east_sero$severity$ifr)
ifr_london=list(reference=fit_london_original$severity$ifr,hospital_deaths=fit_london_deaths_hosp$severity$ifr,community_deaths=fit_london_deaths_comm$severity$ifr,icu_occupancy=fit_london_icu$severity$ifr,general_bed_occupancy=fit_london_general$severity$ifr,hospital_bed_occupancy=fit_london_hosp$severity$ifr,hospital_admissions=fit_london_all_admission$severity$ifr,pillar2=fit_london_pillar2$severity$ifr,ons_pcr_testing=fit_london_ons$severity$ifr,react=fit_london_react$severity$ifr,strain=fit_london_strain$severity$ifr,serology=fit_london_sero$severity$ifr)
ifr_midlands=list(reference=fit_midlands_original$severity$ifr,hospital_deaths=fit_midlands_deaths_hosp$severity$ifr,community_deaths=fit_midlands_deaths_comm$severity$ifr,icu_occupancy=fit_midlands_icu$severity$ifr,general_bed_occupancy=fit_midlands_general$severity$ifr,hospital_bed_occupancy=fit_midlands_hosp$severity$ifr,hospital_admissions=fit_midlands_all_admission$severity$ifr,pillar2=fit_midlands_pillar2$severity$ifr,ons_pcr_testing=fit_midlands_ons$severity$ifr,react=fit_midlands_react$severity$ifr,strain=fit_midlands_strain$severity$ifr,serology=fit_midlands_sero$severity$ifr)
ifr_northeast=list(reference=fit_northeast_original$severity$ifr,hospital_deaths=fit_northeast_deaths_hosp$severity$ifr,community_deaths=fit_northeast_deaths_comm$severity$ifr,icu_occupancy=fit_northeast_icu$severity$ifr,general_bed_occupancy=fit_northeast_general$severity$ifr,hospital_bed_occupancy=fit_northeast_hosp$severity$ifr,hospital_admissions=fit_northeast_all_admission$severity$ifr,pillar2=fit_northeast_pillar2$severity$ifr,ons_pcr_testing=fit_northeast_ons$severity$ifr,react=fit_northeast_react$severity$ifr,strain=fit_northeast_strain$severity$ifr,serology=fit_northeast_sero$severity$ifr)
ifr_northwest=list(reference=fit_northwest_original$severity$ifr,hospital_deaths=fit_northwest_deaths_hosp$severity$ifr,community_deaths=fit_northwest_deaths_comm$severity$ifr,icu_occupancy=fit_northwest_icu$severity$ifr,general_bed_occupancy=fit_northwest_general$severity$ifr,hospital_bed_occupancy=fit_northwest_hosp$severity$ifr,hospital_admissions=fit_northwest_all_admission$severity$ifr,pillar2=fit_northwest_pillar2$severity$ifr,ons_pcr_testing=fit_northwest_ons$severity$ifr,react=fit_northwest_react$severity$ifr,strain=fit_northwest_strain$severity$ifr,serology=fit_northwest_sero$severity$ifr)
ifr_southeast=list(reference=fit_southeast_original$severity$ifr,hospital_deaths=fit_southeast_deaths_hosp$severity$ifr,community_deaths=fit_southeast_deaths_comm$severity$ifr,icu_occupancy=fit_southeast_icu$severity$ifr,general_bed_occupancy=fit_southeast_general$severity$ifr,hospital_bed_occupancy=fit_southeast_hosp$severity$ifr,hospital_admissions=fit_southeast_all_admission$severity$ifr,pillar2=fit_southeast_pillar2$severity$ifr,ons_pcr_testing=fit_southeast_ons$severity$ifr,react=fit_southeast_react$severity$ifr,strain=fit_southeast_strain$severity$ifr,serology=fit_southeast_sero$severity$ifr)
ifr_southwest=list(reference=fit_southwest_original$severity$ifr,hospital_deaths=fit_southwest_deaths_hosp$severity$ifr,community_deaths=fit_southwest_deaths_comm$severity$ifr,icu_occupancy=fit_southwest_icu$severity$ifr,general_bed_occupancy=fit_southwest_general$severity$ifr,hospital_bed_occupancy=fit_southwest_hosp$severity$ifr,hospital_admissions=fit_southwest_all_admission$severity$ifr,pillar2=fit_southwest_pillar2$severity$ifr,ons_pcr_testing=fit_southwest_ons$severity$ifr,react=fit_southwest_react$severity$ifr,strain=fit_southwest_strain$severity$ifr,serology=fit_southwest_sero$severity$ifr)
ifr_england=list(reference=fit_england_original$ifr,hospital_deaths=fit_england_deaths_hosp$ifr,community_deaths=fit_england_deaths_comm$ifr,icu_occupancy=fit_england_icu$ifr,general_bed_occupancy=fit_england_general$ifr,hospital_bed_occupancy=fit_england_hosp$ifr,hospital_admissions=fit_england_all_admission$ifr,pillar2=fit_england_pillar2$ifr,ons_pcr_testing=fit_england_ons$ifr,react=fit_england_react$ifr,strain=fit_england_strain$ifr,serology=fit_england_sero$ifr)
ifr_continuous<-list(east_of_england=ifr_east,london=ifr_london,midlands=ifr_midlands,north_east_and_yorkshire=ifr_northeast,north_west=ifr_northwest,south_east=ifr_southeast,south_west=ifr_southwest,england=ifr_england)

#ihr continuous data====

ihr_east=list(reference=fit_east_original$severity$ihr,hospital_deaths=fit_east_deaths_hosp$severity$ihr,community_deaths=fit_east_deaths_comm$severity$ihr,icu_occupancy=fit_east_icu$severity$ihr,general_bed_occupancy=fit_east_general$severity$ihr,hospital_bed_occupancy=fit_east_hosp$severity$ihr,hospital_admissions=fit_east_all_admission$severity$ihr,pillar2=fit_east_pillar2$severity$ihr,ons_pcr_testing=fit_east_ons$severity$ihr,react=fit_east_react$severity$ihr,strain=fit_east_strain$severity$ihr,serology=fit_east_sero$severity$ihr)
ihr_london=list(reference=fit_london_original$severity$ihr,hospital_deaths=fit_london_deaths_hosp$severity$ihr,community_deaths=fit_london_deaths_comm$severity$ihr,icu_occupancy=fit_london_icu$severity$ihr,general_bed_occupancy=fit_london_general$severity$ihr,hospital_bed_occupancy=fit_london_hosp$severity$ihr,hospital_admissions=fit_london_all_admission$severity$ihr,pillar2=fit_london_pillar2$severity$ihr,ons_pcr_testing=fit_london_ons$severity$ihr,react=fit_london_react$severity$ihr,strain=fit_london_strain$severity$ihr,serology=fit_london_sero$severity$ihr)
ihr_midlands=list(reference=fit_midlands_original$severity$ihr,hospital_deaths=fit_midlands_deaths_hosp$severity$ihr,community_deaths=fit_midlands_deaths_comm$severity$ihr,icu_occupancy=fit_midlands_icu$severity$ihr,general_bed_occupancy=fit_midlands_general$severity$ihr,hospital_bed_occupancy=fit_midlands_hosp$severity$ihr,hospital_admissions=fit_midlands_all_admission$severity$ihr,pillar2=fit_midlands_pillar2$severity$ihr,ons_pcr_testing=fit_midlands_ons$severity$ihr,react=fit_midlands_react$severity$ihr,strain=fit_midlands_strain$severity$ihr,serology=fit_midlands_sero$severity$ihr)
ihr_northeast=list(reference=fit_northeast_original$severity$ihr,hospital_deaths=fit_northeast_deaths_hosp$severity$ihr,community_deaths=fit_northeast_deaths_comm$severity$ihr,icu_occupancy=fit_northeast_icu$severity$ihr,general_bed_occupancy=fit_northeast_general$severity$ihr,hospital_bed_occupancy=fit_northeast_hosp$severity$ihr,hospital_admissions=fit_northeast_all_admission$severity$ihr,pillar2=fit_northeast_pillar2$severity$ihr,ons_pcr_testing=fit_northeast_ons$severity$ihr,react=fit_northeast_react$severity$ihr,strain=fit_northeast_strain$severity$ihr,serology=fit_northeast_sero$severity$ihr)
ihr_northwest=list(reference=fit_northwest_original$severity$ihr,hospital_deaths=fit_northwest_deaths_hosp$severity$ihr,community_deaths=fit_northwest_deaths_comm$severity$ihr,icu_occupancy=fit_northwest_icu$severity$ihr,general_bed_occupancy=fit_northwest_general$severity$ihr,hospital_bed_occupancy=fit_northwest_hosp$severity$ihr,hospital_admissions=fit_northwest_all_admission$severity$ihr,pillar2=fit_northwest_pillar2$severity$ihr,ons_pcr_testing=fit_northwest_ons$severity$ihr,react=fit_northwest_react$severity$ihr,strain=fit_northwest_strain$severity$ihr,serology=fit_northwest_sero$severity$ihr)
ihr_southeast=list(reference=fit_southeast_original$severity$ihr,hospital_deaths=fit_southeast_deaths_hosp$severity$ihr,community_deaths=fit_southeast_deaths_comm$severity$ihr,icu_occupancy=fit_southeast_icu$severity$ihr,general_bed_occupancy=fit_southeast_general$severity$ihr,hospital_bed_occupancy=fit_southeast_hosp$severity$ihr,hospital_admissions=fit_southeast_all_admission$severity$ihr,pillar2=fit_southeast_pillar2$severity$ihr,ons_pcr_testing=fit_southeast_ons$severity$ihr,react=fit_southeast_react$severity$ihr,strain=fit_southeast_strain$severity$ihr,serology=fit_southeast_sero$severity$ihr)
ihr_southwest=list(reference=fit_southwest_original$severity$ihr,hospital_deaths=fit_southwest_deaths_hosp$severity$ihr,community_deaths=fit_southwest_deaths_comm$severity$ihr,icu_occupancy=fit_southwest_icu$severity$ihr,general_bed_occupancy=fit_southwest_general$severity$ihr,hospital_bed_occupancy=fit_southwest_hosp$severity$ihr,hospital_admissions=fit_southwest_all_admission$severity$ihr,pillar2=fit_southwest_pillar2$severity$ihr,ons_pcr_testing=fit_southwest_ons$severity$ihr,react=fit_southwest_react$severity$ihr,strain=fit_southwest_strain$severity$ihr,serology=fit_southwest_sero$severity$ihr)
ihr_england=list(reference=fit_england_original$ihr,hospital_deaths=fit_england_deaths_hosp$ihr,community_deaths=fit_england_deaths_comm$ihr,icu_occupancy=fit_england_icu$ihr,general_bed_occupancy=fit_england_general$ihr,hospital_bed_occupancy=fit_england_hosp$ihr,hospital_admissions=fit_england_all_admission$ihr,pillar2=fit_england_pillar2$ihr,ons_pcr_testing=fit_england_ons$ihr,react=fit_england_react$ihr,strain=fit_england_strain$ihr,serology=fit_england_sero$ihr)
ihr_continuous<-list(east_of_england=ihr_east,london=ihr_london,midlands=ihr_midlands,north_east_and_yorkshire=ihr_northeast,north_west=ihr_northwest,south_east=ihr_southeast,south_west=ihr_southwest,england=ihr_england)

#hfr continuous data====

hfr_east=list(reference=fit_east_original$severity$hfr,hospital_deaths=fit_east_deaths_hosp$severity$hfr,community_deaths=fit_east_deaths_comm$severity$hfr,icu_occupancy=fit_east_icu$severity$hfr,general_bed_occupancy=fit_east_general$severity$hfr,hospital_bed_occupancy=fit_east_hosp$severity$hfr,hospital_admissions=fit_east_all_admission$severity$hfr,pillar2=fit_east_pillar2$severity$hfr,ons_pcr_testing=fit_east_ons$severity$hfr,react=fit_east_react$severity$hfr,strain=fit_east_strain$severity$hfr,serology=fit_east_sero$severity$hfr)
hfr_london=list(reference=fit_london_original$severity$hfr,hospital_deaths=fit_london_deaths_hosp$severity$hfr,community_deaths=fit_london_deaths_comm$severity$hfr,icu_occupancy=fit_london_icu$severity$hfr,general_bed_occupancy=fit_london_general$severity$hfr,hospital_bed_occupancy=fit_london_hosp$severity$hfr,hospital_admissions=fit_london_all_admission$severity$hfr,pillar2=fit_london_pillar2$severity$hfr,ons_pcr_testing=fit_london_ons$severity$hfr,react=fit_london_react$severity$hfr,strain=fit_london_strain$severity$hfr,serology=fit_london_sero$severity$hfr)
hfr_midlands=list(reference=fit_midlands_original$severity$hfr,hospital_deaths=fit_midlands_deaths_hosp$severity$hfr,community_deaths=fit_midlands_deaths_comm$severity$hfr,icu_occupancy=fit_midlands_icu$severity$hfr,general_bed_occupancy=fit_midlands_general$severity$hfr,hospital_bed_occupancy=fit_midlands_hosp$severity$hfr,hospital_admissions=fit_midlands_all_admission$severity$hfr,pillar2=fit_midlands_pillar2$severity$hfr,ons_pcr_testing=fit_midlands_ons$severity$hfr,react=fit_midlands_react$severity$hfr,strain=fit_midlands_strain$severity$hfr,serology=fit_midlands_sero$severity$hfr)
hfr_northeast=list(reference=fit_northeast_original$severity$hfr,hospital_deaths=fit_northeast_deaths_hosp$severity$hfr,community_deaths=fit_northeast_deaths_comm$severity$hfr,icu_occupancy=fit_northeast_icu$severity$hfr,general_bed_occupancy=fit_northeast_general$severity$hfr,hospital_bed_occupancy=fit_northeast_hosp$severity$hfr,hospital_admissions=fit_northeast_all_admission$severity$hfr,pillar2=fit_northeast_pillar2$severity$hfr,ons_pcr_testing=fit_northeast_ons$severity$hfr,react=fit_northeast_react$severity$hfr,strain=fit_northeast_strain$severity$hfr,serology=fit_northeast_sero$severity$hfr)
hfr_northwest=list(reference=fit_northwest_original$severity$hfr,hospital_deaths=fit_northwest_deaths_hosp$severity$hfr,community_deaths=fit_northwest_deaths_comm$severity$hfr,icu_occupancy=fit_northwest_icu$severity$hfr,general_bed_occupancy=fit_northwest_general$severity$hfr,hospital_bed_occupancy=fit_northwest_hosp$severity$hfr,hospital_admissions=fit_northwest_all_admission$severity$hfr,pillar2=fit_northwest_pillar2$severity$hfr,ons_pcr_testing=fit_northwest_ons$severity$hfr,react=fit_northwest_react$severity$hfr,strain=fit_northwest_strain$severity$hfr,serology=fit_northwest_sero$severity$hfr)
hfr_southeast=list(reference=fit_southeast_original$severity$hfr,hospital_deaths=fit_southeast_deaths_hosp$severity$hfr,community_deaths=fit_southeast_deaths_comm$severity$hfr,icu_occupancy=fit_southeast_icu$severity$hfr,general_bed_occupancy=fit_southeast_general$severity$hfr,hospital_bed_occupancy=fit_southeast_hosp$severity$hfr,hospital_admissions=fit_southeast_all_admission$severity$hfr,pillar2=fit_southeast_pillar2$severity$hfr,ons_pcr_testing=fit_southeast_ons$severity$hfr,react=fit_southeast_react$severity$hfr,strain=fit_southeast_strain$severity$hfr,serology=fit_southeast_sero$severity$hfr)
hfr_england=list(reference=fit_england_original$hfr,hospital_deaths=fit_england_deaths_hosp$hfr,community_deaths=fit_england_deaths_comm$hfr,icu_occupancy=fit_england_icu$hfr,general_bed_occupancy=fit_england_general$hfr,hospital_bed_occupancy=fit_england_hosp$hfr,hospital_admissions=fit_england_all_admission$hfr,pillar2=fit_england_pillar2$hfr,ons_pcr_testing=fit_england_ons$hfr,react=fit_england_react$hfr,strain=fit_england_strain$hfr,serology=fit_england_sero$hfr)
hfr_southwest=list(reference=fit_southwest_original$severity$hfr,hospital_deaths=fit_southwest_deaths_hosp$severity$hfr,community_deaths=fit_southwest_deaths_comm$severity$hfr,icu_occupancy=fit_southwest_icu$severity$hfr,general_bed_occupancy=fit_southwest_general$severity$hfr,hospital_bed_occupancy=fit_southwest_hosp$severity$hfr,hospital_admissions=fit_southwest_all_admission$severity$hfr,pillar2=fit_southwest_pillar2$severity$hfr,ons_pcr_testing=fit_southwest_ons$severity$hfr,react=fit_southwest_react$severity$hfr,strain=fit_southwest_strain$severity$hfr,serology=fit_southwest_sero$severity$hfr)

hfr_continuous<-list(east_of_england=hfr_east,london=hfr_london,midlands=hfr_midlands,north_east_and_yorkshire=hfr_northeast,north_west=hfr_northwest,south_east=hfr_southeast,south_west=hfr_southwest,england=hfr_england)

#ifr emergency3 data====

IFR_east<-list(reference=fit_east_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_east_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_east_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_east_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_east_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_east_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_east_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_east_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_east_ons$intrinsic_severity$IFR[3,,],react=fit_east_react$intrinsic_severity$IFR[3,,],strain=fit_east_strain$intrinsic_severity$IFR[3,,],serology=fit_east_sero$intrinsic_severity$IFR[3,,])
IFR_london<-list(reference=fit_london_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_london_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_london_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_london_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_london_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_london_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_london_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_london_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_london_ons$intrinsic_severity$IFR[3,,],react=fit_london_react$intrinsic_severity$IFR[3,,],strain=fit_london_strain$intrinsic_severity$IFR[3,,],serology=fit_london_sero$intrinsic_severity$IFR[3,,])
IFR_midlands<-list(reference=fit_midlands_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_midlands_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_midlands_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_midlands_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_midlands_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_midlands_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_midlands_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_midlands_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_midlands_ons$intrinsic_severity$IFR[3,,],react=fit_midlands_react$intrinsic_severity$IFR[3,,],strain=fit_midlands_strain$intrinsic_severity$IFR[3,,],serology=fit_midlands_sero$intrinsic_severity$IFR[3,,])
IFR_northeast<-list(reference=fit_northeast_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_northeast_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_northeast_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_northeast_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_northeast_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_northeast_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_northeast_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_northeast_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_northeast_ons$intrinsic_severity$IFR[3,,],react=fit_northeast_react$intrinsic_severity$IFR[3,,],strain=fit_northeast_strain$intrinsic_severity$IFR[3,,],serology=fit_northeast_sero$intrinsic_severity$IFR[3,,])
IFR_northwest<-list(reference=fit_northwest_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_northwest_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_northwest_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_northwest_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_northwest_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_northwest_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_northwest_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_northwest_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_northwest_ons$intrinsic_severity$IFR[3,,],react=fit_northwest_react$intrinsic_severity$IFR[3,,],strain=fit_northwest_strain$intrinsic_severity$IFR[3,,],serology=fit_northwest_sero$intrinsic_severity$IFR[3,,])
IFR_southeast<-list(reference=fit_southeast_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_southeast_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_southeast_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_southeast_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_southeast_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_southeast_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_southeast_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_southeast_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_southeast_ons$intrinsic_severity$IFR[3,,],react=fit_southeast_react$intrinsic_severity$IFR[3,,],strain=fit_southeast_strain$intrinsic_severity$IFR[3,,],serology=fit_southeast_sero$intrinsic_severity$IFR[3,,])
IFR_southwest<-list(reference=fit_southwest_original$intrinsic_severity$IFR[3,,],hospital_deaths=fit_southwest_deaths_hosp$intrinsic_severity$IFR[3,,],community_deaths=fit_southwest_deaths_comm$intrinsic_severity$IFR[3,,],icu_occupancy=fit_southwest_icu$intrinsic_severity$IFR[3,,],general_bed_occupancy=fit_southwest_general$intrinsic_severity$IFR[3,,],hospital_bed_occupancy=fit_southwest_hosp$intrinsic_severity$IFR[3,,],hospital_admissions=fit_southwest_all_admission$intrinsic_severity$IFR[3,,],pillar2=fit_southwest_pillar2$intrinsic_severity$IFR[3,,],ons_pcr_testing=fit_southwest_ons$intrinsic_severity$IFR[3,,],react=fit_southwest_react$intrinsic_severity$IFR[3,,],strain=fit_southwest_strain$intrinsic_severity$IFR[3,,],serology=fit_southwest_sero$intrinsic_severity$IFR[3,,])
IFR_england<-list(reference=england_instrinsic_original$IFR[3,,],hospital_deaths=england_instrinsic_deaths_hosp$IFR[3,,],community_deaths=england_instrinsic_deaths_comm$IFR[3,,],icu_occupancy=england_instrinsic_icu$IFR[3,,],general_bed_occupancy=england_instrinsic_general$IFR[3,,],hospital_bed_occupancy=england_instrinsic_hosp$IFR[3,,],hospital_admissions=england_instrinsic_all_admission$IFR[3,,],pillar2=england_instrinsic_pillar2$IFR[3,,],ons_pcr_testing=england_instrinsic_ons$IFR[3,,],react=england_instrinsic_react$IFR[3,,],strain=england_instrinsic_strain$IFR[3,,],serology=england_instrinsic_sero$IFR[3,,])

IFR<-list(east_of_england=IFR_east,london=IFR_london,midlands=IFR_midlands,north_east_and_yorkshire=IFR_northeast,north_west=IFR_northwest,south_east=IFR_southeast,south_west=IFR_southwest,england=IFR_england)

#ihr emergency3 data====

IHR_east<-list(reference=fit_east_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_east_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_east_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_east_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_east_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_east_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_east_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_east_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_east_ons$intrinsic_severity$IHR[3,,],react=fit_east_react$intrinsic_severity$IHR[3,,],strain=fit_east_strain$intrinsic_severity$IHR[3,,],serology=fit_east_sero$intrinsic_severity$IHR[3,,])
IHR_london<-list(reference=fit_london_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_london_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_london_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_london_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_london_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_london_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_london_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_london_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_london_ons$intrinsic_severity$IHR[3,,],react=fit_london_react$intrinsic_severity$IHR[3,,],strain=fit_london_strain$intrinsic_severity$IHR[3,,],serology=fit_london_sero$intrinsic_severity$IHR[3,,])
IHR_midlands<-list(reference=fit_midlands_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_midlands_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_midlands_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_midlands_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_midlands_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_midlands_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_midlands_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_midlands_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_midlands_ons$intrinsic_severity$IHR[3,,],react=fit_midlands_react$intrinsic_severity$IHR[3,,],strain=fit_midlands_strain$intrinsic_severity$IHR[3,,],serology=fit_midlands_sero$intrinsic_severity$IHR[3,,])
IHR_northeast<-list(reference=fit_northeast_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_northeast_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_northeast_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_northeast_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_northeast_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_northeast_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_northeast_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_northeast_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_northeast_ons$intrinsic_severity$IHR[3,,],react=fit_northeast_react$intrinsic_severity$IHR[3,,],strain=fit_northeast_strain$intrinsic_severity$IHR[3,,],serology=fit_northeast_sero$intrinsic_severity$IHR[3,,])
IHR_northwest<-list(reference=fit_northwest_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_northwest_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_northwest_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_northwest_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_northwest_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_northwest_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_northwest_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_northwest_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_northwest_ons$intrinsic_severity$IHR[3,,],react=fit_northwest_react$intrinsic_severity$IHR[3,,],strain=fit_northwest_strain$intrinsic_severity$IHR[3,,],serology=fit_northwest_sero$intrinsic_severity$IHR[3,,])
IHR_southeast<-list(reference=fit_southeast_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_southeast_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_southeast_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_southeast_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_southeast_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_southeast_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_southeast_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_southeast_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_southeast_ons$intrinsic_severity$IHR[3,,],react=fit_southeast_react$intrinsic_severity$IHR[3,,],strain=fit_southeast_strain$intrinsic_severity$IHR[3,,],serology=fit_southeast_sero$intrinsic_severity$IHR[3,,])
IHR_southwest<-list(reference=fit_southwest_original$intrinsic_severity$IHR[3,,],hospital_deaths=fit_southwest_deaths_hosp$intrinsic_severity$IHR[3,,],community_deaths=fit_southwest_deaths_comm$intrinsic_severity$IHR[3,,],icu_occupancy=fit_southwest_icu$intrinsic_severity$IHR[3,,],general_bed_occupancy=fit_southwest_general$intrinsic_severity$IHR[3,,],hospital_bed_occupancy=fit_southwest_hosp$intrinsic_severity$IHR[3,,],hospital_admissions=fit_southwest_all_admission$intrinsic_severity$IHR[3,,],pillar2=fit_southwest_pillar2$intrinsic_severity$IHR[3,,],ons_pcr_testing=fit_southwest_ons$intrinsic_severity$IHR[3,,],react=fit_southwest_react$intrinsic_severity$IHR[3,,],strain=fit_southwest_strain$intrinsic_severity$IHR[3,,],serology=fit_southwest_sero$intrinsic_severity$IHR[3,,])
IHR_england<-list(reference=england_instrinsic_original$IHR[3,,],hospital_deaths=england_instrinsic_deaths_hosp$IHR[3,,],community_deaths=england_instrinsic_deaths_comm$IHR[3,,],icu_occupancy=england_instrinsic_icu$IHR[3,,],general_bed_occupancy=england_instrinsic_general$IHR[3,,],hospital_bed_occupancy=england_instrinsic_hosp$IHR[3,,],hospital_admissions=england_instrinsic_all_admission$IHR[3,,],pillar2=england_instrinsic_pillar2$IHR[3,,],ons_pcr_testing=england_instrinsic_ons$IHR[3,,],react=england_instrinsic_react$IHR[3,,],strain=england_instrinsic_strain$IHR[3,,],serology=england_instrinsic_sero$IHR[3,,])

IHR<-list(east_of_england=IHR_east,london=IHR_london,midlands=IHR_midlands,north_east_and_yorkshire=IHR_northeast,north_west=IHR_northwest,south_east=IHR_southeast,south_west=IHR_southwest,england=IHR_england)

#hfr emergency3 data====

HFR_east<-list(reference=fit_east_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_east_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_east_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_east_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_east_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_east_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_east_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_east_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_east_ons$intrinsic_severity$HFR[3,,],react=fit_east_react$intrinsic_severity$HFR[3,,],strain=fit_east_strain$intrinsic_severity$HFR[3,,],serology=fit_east_sero$intrinsic_severity$HFR[3,,])
HFR_london<-list(reference=fit_london_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_london_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_london_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_london_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_london_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_london_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_london_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_london_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_london_ons$intrinsic_severity$HFR[3,,],react=fit_london_react$intrinsic_severity$HFR[3,,],strain=fit_london_strain$intrinsic_severity$HFR[3,,],serology=fit_london_sero$intrinsic_severity$HFR[3,,])
HFR_midlands<-list(reference=fit_midlands_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_midlands_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_midlands_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_midlands_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_midlands_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_midlands_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_midlands_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_midlands_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_midlands_ons$intrinsic_severity$HFR[3,,],react=fit_midlands_react$intrinsic_severity$HFR[3,,],strain=fit_midlands_strain$intrinsic_severity$HFR[3,,],serology=fit_midlands_sero$intrinsic_severity$HFR[3,,])
HFR_northeast<-list(reference=fit_northeast_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_northeast_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_northeast_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_northeast_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_northeast_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_northeast_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_northeast_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_northeast_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_northeast_ons$intrinsic_severity$HFR[3,,],react=fit_northeast_react$intrinsic_severity$HFR[3,,],strain=fit_northeast_strain$intrinsic_severity$HFR[3,,],serology=fit_northeast_sero$intrinsic_severity$HFR[3,,])
HFR_northwest<-list(reference=fit_northwest_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_northwest_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_northwest_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_northwest_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_northwest_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_northwest_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_northwest_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_northwest_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_northwest_ons$intrinsic_severity$HFR[3,,],react=fit_northwest_react$intrinsic_severity$HFR[3,,],strain=fit_northwest_strain$intrinsic_severity$HFR[3,,],serology=fit_northwest_sero$intrinsic_severity$HFR[3,,])
HFR_southeast<-list(reference=fit_southeast_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_southeast_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_southeast_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_southeast_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_southeast_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_southeast_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_southeast_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_southeast_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_southeast_ons$intrinsic_severity$HFR[3,,],react=fit_southeast_react$intrinsic_severity$HFR[3,,],strain=fit_southeast_strain$intrinsic_severity$HFR[3,,],serology=fit_southeast_sero$intrinsic_severity$HFR[3,,])
HFR_southwest<-list(reference=fit_southwest_original$intrinsic_severity$HFR[3,,],hospital_deaths=fit_southwest_deaths_hosp$intrinsic_severity$HFR[3,,],community_deaths=fit_southwest_deaths_comm$intrinsic_severity$HFR[3,,],icu_occupancy=fit_southwest_icu$intrinsic_severity$HFR[3,,],general_bed_occupancy=fit_southwest_general$intrinsic_severity$HFR[3,,],hospital_bed_occupancy=fit_southwest_hosp$intrinsic_severity$HFR[3,,],hospital_admissions=fit_southwest_all_admission$intrinsic_severity$HFR[3,,],pillar2=fit_southwest_pillar2$intrinsic_severity$HFR[3,,],ons_pcr_testing=fit_southwest_ons$intrinsic_severity$HFR[3,,],react=fit_southwest_react$intrinsic_severity$HFR[3,,],strain=fit_southwest_strain$intrinsic_severity$HFR[3,,],serology=fit_southwest_sero$intrinsic_severity$HFR[3,,])
HFR_england<-list(reference=england_instrinsic_original$HFR[3,,],hospital_deaths=england_instrinsic_deaths_hosp$HFR[3,,],community_deaths=england_instrinsic_deaths_comm$HFR[3,,],icu_occupancy=england_instrinsic_icu$HFR[3,,],general_bed_occupancy=england_instrinsic_general$HFR[3,,],hospital_bed_occupancy=england_instrinsic_hosp$HFR[3,,],hospital_admissions=england_instrinsic_all_admission$HFR[3,,],pillar2=england_instrinsic_pillar2$HFR[3,,],ons_pcr_testing=england_instrinsic_ons$HFR[3,,],react=england_instrinsic_react$HFR[3,,],strain=england_instrinsic_strain$HFR[3,,],serology=england_instrinsic_sero$HFR[3,,])

HFR<-list(east_of_england=HFR_east,london=HFR_london,midlands=HFR_midlands,north_east_and_yorkshire=HFR_northeast,north_west=HFR_northwest,south_east=HFR_southeast,south_west=HFR_southwest,england=HFR_england)

#Rt_eff data====

Rt_eff_east<-list(reference=fit_east_original$rt$eff_Rt[,3,])
Rt_eff_east=list(reference=fit_east_original$rt$eff_Rt[,3,],hospital_deaths=fit_east_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_east_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_east_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_east_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_east_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_east_all_admission$rt$eff_Rt[,3,],pillar2=fit_east_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_east_ons$rt$eff_Rt[,3,],react=fit_east_react$rt$eff_Rt[,3,],strain=fit_east_strain$rt$eff_Rt[,3,],serology=fit_east_sero$rt$eff_Rt[,3,])
Rt_eff_london=list(reference=fit_london_original$rt$eff_Rt[,3,],hospital_deaths=fit_london_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_london_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_london_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_london_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_london_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_london_all_admission$rt$eff_Rt[,3,],pillar2=fit_london_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_london_ons$rt$eff_Rt[,3,],react=fit_london_react$rt$eff_Rt[,3,],strain=fit_london_strain$rt$eff_Rt[,3,],serology=fit_london_sero$rt$eff_Rt[,3,])
Rt_eff_midlands=list(reference=fit_midlands_original$rt$eff_Rt[,3,],hospital_deaths=fit_midlands_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_midlands_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_midlands_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_midlands_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_midlands_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_midlands_all_admission$rt$eff_Rt[,3,],pillar2=fit_midlands_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_midlands_ons$rt$eff_Rt[,3,],react=fit_midlands_react$rt$eff_Rt[,3,],strain=fit_midlands_strain$rt$eff_Rt[,3,],serology=fit_midlands_sero$rt$eff_Rt[,3,])
Rt_eff_northeast=list(reference=fit_northeast_original$rt$eff_Rt[,3,],hospital_deaths=fit_northeast_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_northeast_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_northeast_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_northeast_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_northeast_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_northeast_all_admission$rt$eff_Rt[,3,],pillar2=fit_northeast_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_northeast_ons$rt$eff_Rt[,3,],react=fit_northeast_react$rt$eff_Rt[,3,],strain=fit_northeast_strain$rt$eff_Rt[,3,],serology=fit_northeast_sero$rt$eff_Rt[,3,])
Rt_eff_northwest=list(reference=fit_northwest_original$rt$eff_Rt[,3,],hospital_deaths=fit_northwest_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_northwest_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_northwest_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_northwest_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_northwest_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_northwest_all_admission$rt$eff_Rt[,3,],pillar2=fit_northwest_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_northwest_ons$rt$eff_Rt[,3,],react=fit_northwest_react$rt$eff_Rt[,3,],strain=fit_northwest_strain$rt$eff_Rt[,3,],serology=fit_northwest_sero$rt$eff_Rt[,3,])
Rt_eff_southeast=list(reference=fit_southeast_original$rt$eff_Rt[,3,],hospital_deaths=fit_southeast_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_southeast_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_southeast_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_southeast_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_southeast_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_southeast_all_admission$rt$eff_Rt[,3,],pillar2=fit_southeast_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_southeast_ons$rt$eff_Rt[,3,],react=fit_southeast_react$rt$eff_Rt[,3,],strain=fit_southeast_strain$rt$eff_Rt[,3,],serology=fit_southeast_sero$rt$eff_Rt[,3,])
Rt_eff_southwest=list(reference=fit_southwest_original$rt$eff_Rt[,3,],hospital_deaths=fit_southwest_deaths_hosp$rt$eff_Rt[,3,],community_deaths=fit_southwest_deaths_comm$rt$eff_Rt[,3,],icu_occupancy=fit_southwest_icu$rt$eff_Rt[,3,],general_bed_occupancy=fit_southwest_general$rt$eff_Rt[,3,],hospital_bed_occupancy=fit_southwest_hosp$rt$eff_Rt[,3,],hospital_admissions=fit_southwest_all_admission$rt$eff_Rt[,3,],pillar2=fit_southwest_pillar2$rt$eff_Rt[,3,],ons_pcr_testing=fit_southwest_ons$rt$eff_Rt[,3,],react=fit_southwest_react$rt$eff_Rt[,3,],strain=fit_southwest_strain$rt$eff_Rt[,3,],serology=fit_southwest_sero$rt$eff_Rt[,3,])
Rt_eff_england=list(reference=Rt_england_original$eff_Rt[,3,],hospital_deaths=Rt_england_deaths_hosp$eff_Rt[,3,],community_deaths=Rt_england_deaths_comm$eff_Rt[,3,],icu_occupancy=Rt_england_icu$eff_Rt[,3,],general_bed_occupancy=Rt_england_general$eff_Rt[,3,],hospital_bed_occupancy=Rt_england_hosp$eff_Rt[,3,],hospital_admissions=Rt_england_all_admission$eff_Rt[,3,],pillar2=Rt_england_pillar2$eff_Rt[,3,],ons_pcr_testing=Rt_england_ons$eff_Rt[,3,],react=Rt_england_react$eff_Rt[,3,],strain=Rt_england_strain$eff_Rt[,3,],serology=Rt_england_sero$eff_Rt[,3,])
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

R0_east_original <- get_R0(fit_east_original)
R0_london_original <- get_R0(fit_london_original)
R0_midlands_original <- get_R0(fit_midlands_original)
R0_northeast_original <- get_R0(fit_northeast_original)
R0_northwest_original <- get_R0(fit_northwest_original)
R0_southeast_original <- get_R0(fit_southeast_original)
R0_southwest_original <- get_R0(fit_southwest_original)
R0_east_deaths_hosp <- get_R0(fit_east_deaths_hosp)
R0_london_deaths_hosp <- get_R0(fit_london_deaths_hosp)
R0_midlands_deaths_hosp <- get_R0(fit_midlands_deaths_hosp)
R0_northeast_deaths_hosp <- get_R0(fit_northeast_deaths_hosp)
R0_northwest_deaths_hosp <- get_R0(fit_northwest_deaths_hosp)
R0_southeast_deaths_hosp <- get_R0(fit_southeast_deaths_hosp)
R0_southwest_deaths_hosp <- get_R0(fit_southwest_deaths_hosp)
R0_east_deaths_comm <- get_R0(fit_east_deaths_comm)
R0_london_deaths_comm <- get_R0(fit_london_deaths_comm)
R0_midlands_deaths_comm <- get_R0(fit_midlands_deaths_comm)
R0_northeast_deaths_comm <- get_R0(fit_northeast_deaths_comm)
R0_northwest_deaths_comm <- get_R0(fit_northwest_deaths_comm)
R0_southeast_deaths_comm <- get_R0(fit_southeast_deaths_comm)
R0_southwest_deaths_comm <- get_R0(fit_southwest_deaths_comm)
R0_east_icu <- get_R0(fit_east_icu)
R0_london_icu <- get_R0(fit_london_icu)
R0_midlands_icu <- get_R0(fit_midlands_icu)
R0_northeast_icu <- get_R0(fit_northeast_icu)
R0_northwest_icu <- get_R0(fit_northwest_icu)
R0_southeast_icu <- get_R0(fit_southeast_icu)
R0_southwest_icu <- get_R0(fit_southwest_icu)
R0_east_general <- get_R0(fit_east_general)
R0_london_general <- get_R0(fit_london_general)
R0_midlands_general <- get_R0(fit_midlands_general)
R0_northeast_general <- get_R0(fit_northeast_general)
R0_northwest_general <- get_R0(fit_northwest_general)
R0_southeast_general <- get_R0(fit_southeast_general)
R0_southwest_general <- get_R0(fit_southwest_general)
R0_east_hosp <- get_R0(fit_east_hosp)
R0_london_hosp <- get_R0(fit_london_hosp)
R0_midlands_hosp <- get_R0(fit_midlands_hosp)
R0_northeast_hosp <- get_R0(fit_northeast_hosp)
R0_northwest_hosp <- get_R0(fit_northwest_hosp)
R0_southeast_hosp <- get_R0(fit_southeast_hosp)
R0_southwest_hosp <- get_R0(fit_southwest_hosp)
R0_east_all_admission <- get_R0(fit_east_all_admission)
R0_london_all_admission <- get_R0(fit_london_all_admission)
R0_midlands_all_admission <- get_R0(fit_midlands_all_admission)
R0_northeast_all_admission <- get_R0(fit_northeast_all_admission)
R0_northwest_all_admission <- get_R0(fit_northwest_all_admission)
R0_southeast_all_admission <- get_R0(fit_southeast_all_admission)
R0_southwest_all_admission <- get_R0(fit_southwest_all_admission)
R0_east_pillar2 <- get_R0(fit_east_pillar2)
R0_london_pillar2 <- get_R0(fit_london_pillar2)
R0_midlands_pillar2 <- get_R0(fit_midlands_pillar2)
R0_northeast_pillar2 <- get_R0(fit_northeast_pillar2)
R0_northwest_pillar2 <- get_R0(fit_northwest_pillar2)
R0_southeast_pillar2 <- get_R0(fit_southeast_pillar2)
R0_southwest_pillar2 <- get_R0(fit_southwest_pillar2)
R0_east_ons <- get_R0(fit_east_ons)
R0_london_ons <- get_R0(fit_london_ons)
R0_midlands_ons <- get_R0(fit_midlands_ons)
R0_northeast_ons <- get_R0(fit_northeast_ons)
R0_northwest_ons <- get_R0(fit_northwest_ons)
R0_southeast_ons <- get_R0(fit_southeast_ons)
R0_southwest_ons <- get_R0(fit_southwest_ons)
R0_east_react <- get_R0(fit_east_react)
R0_london_react <- get_R0(fit_london_react)
R0_midlands_react <- get_R0(fit_midlands_react)
R0_northeast_react <- get_R0(fit_northeast_react)
R0_northwest_react <- get_R0(fit_northwest_react)
R0_southeast_react <- get_R0(fit_southeast_react)
R0_southwest_react <- get_R0(fit_southwest_react)
R0_east_strain <- get_R0(fit_east_strain)
R0_london_strain <- get_R0(fit_london_strain)
R0_midlands_strain <- get_R0(fit_midlands_strain)
R0_northeast_strain <- get_R0(fit_northeast_strain)
R0_northwest_strain <- get_R0(fit_northwest_strain)
R0_southeast_strain <- get_R0(fit_southeast_strain)
R0_southwest_strain <- get_R0(fit_southwest_strain)
R0_east_sero <- get_R0(fit_east_sero)
R0_london_sero <- get_R0(fit_london_sero)
R0_midlands_sero <- get_R0(fit_midlands_sero)
R0_northeast_sero <- get_R0(fit_northeast_sero)
R0_northwest_sero <- get_R0(fit_northwest_sero)
R0_southeast_sero <- get_R0(fit_southeast_sero)
R0_southwest_sero <- get_R0(fit_southwest_sero)

R0_east_of_england=list(reference=R0_east_original,hospital_deaths=R0_east_deaths_hosp,community_deaths=R0_east_deaths_comm,icu_occupancy=R0_east_icu,general_bed_occupancy=R0_east_general,hospital_bed_occupancy=R0_east_hosp,hospital_admissions=R0_east_all_admission,pillar2=R0_east_pillar2,ons_pcr_testing=R0_east_ons,react=R0_east_react,strain=R0_east_strain,serology=R0_east_sero)
R0_london=list(reference=R0_london_original,hospital_deaths=R0_london_deaths_hosp,community_deaths=R0_london_deaths_comm,icu_occupancy=R0_london_icu,general_bed_occupancy=R0_london_general,hospital_bed_occupancy=R0_london_hosp,hospital_admissions=R0_london_all_admission,pillar2=R0_london_pillar2,ons_pcr_testing=R0_london_ons,react=R0_london_react,strain=R0_london_strain,serology=R0_london_sero)
R0_midlands=list(reference=R0_midlands_original,hospital_deaths=R0_midlands_deaths_hosp,community_deaths=R0_midlands_deaths_comm,icu_occupancy=R0_midlands_icu,general_bed_occupancy=R0_midlands_general,hospital_bed_occupancy=R0_midlands_hosp,hospital_admissions=R0_midlands_all_admission,pillar2=R0_midlands_pillar2,ons_pcr_testing=R0_midlands_ons,react=R0_midlands_react,strain=R0_midlands_strain,serology=R0_midlands_sero)
R0_north_east_and_yorksire=list(reference=R0_northeast_original,hospital_deaths=R0_northeast_deaths_hosp,community_deaths=R0_northeast_deaths_comm,icu_occupancy=R0_northeast_icu,general_bed_occupancy=R0_northeast_general,hospital_bed_occupancy=R0_northeast_hosp,hospital_admissions=R0_northeast_all_admission,pillar2=R0_northeast_pillar2,ons_pcr_testing=R0_northeast_ons,react=R0_northeast_react,strain=R0_northeast_strain,serology=R0_northeast_sero)
R0_north_west=list(reference=R0_northwest_original,hospital_deaths=R0_northwest_deaths_hosp,community_deaths=R0_northwest_deaths_comm,icu_occupancy=R0_northwest_icu,general_bed_occupancy=R0_northwest_general,hospital_bed_occupancy=R0_northwest_hosp,hospital_admissions=R0_northwest_all_admission,pillar2=R0_northwest_pillar2,ons_pcr_testing=R0_northwest_ons,react=R0_northwest_react,strain=R0_northwest_strain,serology=R0_northwest_sero)
R0_south_east=list(reference=R0_southeast_original,hospital_deaths=R0_southeast_deaths_hosp,community_deaths=R0_southeast_deaths_comm,icu_occupancy=R0_southeast_icu,general_bed_occupancy=R0_southeast_general,hospital_bed_occupancy=R0_southeast_hosp,hospital_admissions=R0_southeast_all_admission,pillar2=R0_southeast_pillar2,ons_pcr_testing=R0_southeast_ons,react=R0_southeast_react,strain=R0_southeast_strain,serology=R0_southeast_sero)
R0_south_west=list(reference=R0_southwest_original,hospital_deaths=R0_southwest_deaths_hosp,community_deaths=R0_southwest_deaths_comm,icu_occupancy=R0_southwest_icu,general_bed_occupancy=R0_southwest_general,hospital_bed_occupancy=R0_southwest_hosp,hospital_admissions=R0_southwest_all_admission,pillar2=R0_southwest_pillar2,ons_pcr_testing=R0_southwest_ons,react=R0_southwest_react,strain=R0_southwest_strain,serology=R0_southwest_sero)
R0_england=list(reference=england_R0_original,hospital_deaths=england_R0_deaths_hosp,community_deaths=england_R0_deaths_comm,icu_occupancy=england_R0_icu,general_bed_occupancy=england_R0_general,hospital_bed_occupancy=england_R0_hosp,hospital_admissions=england_R0_all_admission,pillar2=england_R0_pillar2,ons_pcr_testing=england_R0_ons,react=england_R0_react,strain=england_R0_strain,serology=england_R0_sero)

R0=list(east_of_england=R0_east_of_england,london=R0_london,midlands=R0_midlands,north_east_and_yorkshire=R0_north_east_and_yorksire,north_west=R0_north_west,south_east=R0_south_east,south_west=R0_south_west,england=R0_england)

#parameter data====

pars_east<-list(reference=fit_east_original$samples$pars,hospital_deaths=fit_east_deaths_hosp$samples$pars,community_deaths=fit_east_deaths_comm$samples$pars,icu_occupancy=fit_east_icu$samples$pars)
pars_london<-list(reference=fit_london_original$samples$pars,hospital_deaths=fit_london_deaths_hosp$samples$pars,community_deaths=fit_london_deaths_comm$samples$pars,icu_occupancy=fit_london_icu$samples$pars)
pars_midlands<-list(reference=fit_midlands_original$samples$pars,hospital_deaths=fit_midlands_deaths_hosp$samples$pars,community_deaths=fit_midlands_deaths_comm$samples$pars,icu_occupancy=fit_midlands_icu$samples$pars)
pars_northeast<-list(reference=fit_northeast_original$samples$pars,hospital_deaths=fit_northeast_deaths_hosp$samples$pars,community_deaths=fit_northeast_deaths_comm$samples$pars,icu_occupancy=fit_northeast_icu$samples$pars)
pars_northwest<-list(reference=fit_northwest_original$samples$pars,hospital_deaths=fit_northwest_deaths_hosp$samples$pars,community_deaths=fit_northwest_deaths_comm$samples$pars,icu_occupancy=fit_northwest_icu$samples$pars)
pars_southeast<-list(reference=fit_southeast_original$samples$pars,hospital_deaths=fit_southeast_deaths_hosp$samples$pars,community_deaths=fit_southeast_deaths_comm$samples$pars,icu_occupancy=fit_southeast_icu$samples$pars)
pars_southwest<-list(reference=fit_southwest_original$samples$pars,hospital_deaths=fit_southwest_deaths_hosp$samples$pars,community_deaths=fit_southwest_deaths_comm$samples$pars,icu_occupancy=fit_southwest_icu$samples$pars)
pars_east2<-list(general_bed_occupancy=fit_east_general$samples$pars,hospital_bed_occupancy=fit_east_hosp$samples$pars)
pars_london2<-list(general_bed_occupancy=fit_london_general$samples$pars,hospital_bed_occupancy=fit_london_hosp$samples$pars)
pars_midlands2<-list(general_bed_occupancy=fit_midlands_general$samples$pars,hospital_bed_occupancy=fit_midlands_hosp$samples$pars)
pars_northeast2<-list(general_bed_occupancy=fit_northeast_general$samples$pars,hospital_bed_occupancy=fit_northeast_hosp$samples$pars)
pars_northwest2<-list(general_bed_occupancy=fit_northwest_general$samples$pars,hospital_bed_occupancy=fit_northwest_hosp$samples$pars)
pars_southeast2<-list(general_bed_occupancy=fit_southeast_general$samples$pars,hospital_bed_occupancy=fit_southeast_hosp$samples$pars)
pars_southwest2<-list(general_bed_occupancy=fit_southwest_general$samples$pars,hospital_bed_occupancy=fit_southwest_hosp$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)
pars_east2<-list(hospital_admissions=fit_east_all_admission$samples$pars,pillar2=fit_east_pillar2$samples$pars,ons_pcr_testing=fit_east_ons$samples$pars)
pars_london2<-list(hospital_admissions=fit_london_all_admission$samples$pars,pillar2=fit_london_pillar2$samples$pars,ons_pcr_testing=fit_london_ons$samples$pars)
pars_midlands2<-list(hospital_admissions=fit_midlands_all_admission$samples$pars,pillar2=fit_midlands_pillar2$samples$pars,ons_pcr_testing=fit_midlands_ons$samples$pars)
pars_northeast2<-list(hospital_admissions=fit_northeast_all_admission$samples$pars,pillar2=fit_northeast_pillar2$samples$pars,ons_pcr_testing=fit_northeast_ons$samples$pars)
pars_northwest2<-list(hospital_admissions=fit_northwest_all_admission$samples$pars,pillar2=fit_northwest_pillar2$samples$pars,ons_pcr_testing=fit_northwest_ons$samples$pars)
pars_southeast2<-list(hospital_admissions=fit_southeast_all_admission$samples$pars,pillar2=fit_southeast_pillar2$samples$pars,ons_pcr_testing=fit_southeast_ons$samples$pars)
pars_southwest2<-list(hospital_admissions=fit_southwest_all_admission$samples$pars,pillar2=fit_southwest_pillar2$samples$pars,ons_pcr_testing=fit_southwest_ons$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)
pars_east2<-list(react=fit_east_react$samples$pars,strain=fit_east_strain$samples$pars,serology=fit_east_sero$samples$pars)
pars_london2<-list(react=fit_london_react$samples$pars,strain=fit_london_strain$samples$pars,serology=fit_london_sero$samples$pars)
pars_midlands2<-list(react=fit_midlands_react$samples$pars,strain=fit_midlands_strain$samples$pars,serology=fit_midlands_sero$samples$pars)
pars_northeast2<-list(react=fit_northeast_react$samples$pars,strain=fit_northeast_strain$samples$pars,serology=fit_northeast_sero$samples$pars)
pars_northwest2<-list(react=fit_northwest_react$samples$pars,strain=fit_northwest_strain$samples$pars,serology=fit_northwest_sero$samples$pars)
pars_southeast2<-list(react=fit_southeast_react$samples$pars,strain=fit_southeast_strain$samples$pars,serology=fit_southeast_sero$samples$pars)
pars_southwest2<-list(react=fit_southwest_react$samples$pars,strain=fit_southwest_strain$samples$pars,serology=fit_southwest_sero$samples$pars)
pars_east=append(pars_east,pars_east2)
pars_london=append(pars_london,pars_london2)
pars_midlands=append(pars_midlands,pars_midlands2)
pars_northeast=append(pars_northeast,pars_northeast2)
pars_northwest=append(pars_northwest,pars_northwest2)
pars_southeast=append(pars_southeast,pars_southeast2)
pars_southwest=append(pars_southwest,pars_southwest2)

pars<-list(east_of_england=pars_east,london=pars_london,midlands=pars_midlands,north_east_and_yorkshire=pars_northeast,north_west=pars_northwest,south_east=pars_southeast,south_west=pars_southwest)

#diagnostics data====

diagnostics_original <- readRDS("inputs/original/diagnostics.rds")
diagnostics_deaths_hosp <- readRDS("inputs/deaths_hosp/diagnostics.rds")
diagnostics_deaths_comm <- readRDS("inputs/deaths_comm/diagnostics.rds")
diagnostics_icu <- readRDS("inputs/icu/diagnostics.rds")
diagnostics_general <- readRDS("inputs/general/diagnostics.rds")
diagnostics_hosp <- readRDS("inputs/hosp/diagnostics.rds")
diagnostics_all_admission <- readRDS("inputs/all_admission/diagnostics.rds")
diagnostics_pillar2 <- readRDS("inputs/pillar2/diagnostics.rds")
diagnostics_ons <- readRDS("inputs/ons/diagnostics.rds")
diagnostics_react <- readRDS("inputs/react/diagnostics.rds")
diagnostics_strain <- readRDS("inputs/strain/diagnostics.rds")
diagnostics_sero <- readRDS("inputs/sero/diagnostics.rds")

diagnostics<-list(reference=diagnostics_original,hospital_deaths=diagnostics_deaths_hosp,community_deaths=diagnostics_deaths_comm,icu_occupancy=diagnostics_icu,general_bed_occupancy=diagnostics_general,hospital_bed_occupancy=diagnostics_hosp,hospital_admissions=diagnostics_all_admission,pillar2=diagnostics_pillar2,ons_pcr_testing=diagnostics_ons,react=diagnostics_react,strain=diagnostics_strain,serology=diagnostics_sero)

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
  names(temp)=c("data_stream","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,data_stream:=factor(data_stream,rownames(KL_ifr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=data_stream,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective IFR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))
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
  names(temp)=c("data_stream","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,data_stream:=factor(data_stream,rownames(KL_ihr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=data_stream,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective IHR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))
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
  names(temp)=c("data_stream","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,data_stream:=factor(data_stream,rownames(KL_hfr_continuous[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=data_stream,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective HFR in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))
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
  names(temp)=c("data_stream","date","value")
  setDT(temp)
  temp[,dateI:=as.integer(str_extract(date,'\\d+'))]
  temp[,data_stream:=factor(data_stream,rownames(KL_Rt_eff[[r]]))]
  ds<-sapply(setNames(,622:76),function(x) as.character(as_date('2021-09-13')-(622-as.integer(x))))
  temp[,DATE:=ds[as.character(dateI)]]
  temp[dateI==0,DATE:='2020-03-16']
  temp[,DATE:=as.Date(DATE)]
  p<-ggplot(temp,aes(x=DATE,y=data_stream,fill=value))+
    geom_tile(height=0.95,colour=NA)+
    theme_bw()+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
    labs(x='Date',y='',fill='')+
    theme(legend.key.height=unit(1.7,'cm'),panel.grid=element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of effective Rt in ",R))+
    scale_y_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))
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
  tmp=paste0("KL divergence of parameters in ",R)
  ggp <- ggplot(data_melt, aes(Var1, Var2)) +                         
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(tmp)+
    theme(axis.text.x = element_text(angle = 50, vjust = 0.5))+
    scale_x_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))
  # agg_png(paste0("outputs/KL divergence heatmap of parameters in ",r,".png"),res=600,height = 8000,width = 4000)
  print(ggp)
  # dev.off()
}
dev.off()

pdf(paste0("outputs/KL divergence heatmap of IHR,IFR,HFR.pdf"),width =10, height =7)
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
      ticks=c(3,6.5,3.85,4.75,5.65)
    }else{
      ticks=c(5,10,6.25,7.5,8.5)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
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
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf("%.3f",mean),` `=paste(rep(' ',100),collapse=''),
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
                     # ref_line = 7.15,
                     # xlim=c(6.8,7.6),
                     # ticks_at=c(7,7.2,7.4),
                     # ref_line = 4.4,
                     # xlim=c(4.0,4.8),
                     # ticks_at=c(4.2,4.4,4.6),
                     # ref_line = 2.6,
                     # xlim=c(2.2,3.0),
                     # ticks_at=c(2.4,2.6,2.8),
                     theme = tm,
                     xlab = paste("R0 of",variant),
                     title = paste("R0 of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/R0 of ",variant, " in ",r,"2.png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
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
      ticks=c(0.004,0.014,0.006,0.009,0.012)
    }else if(i==2){
      ticks=c(0.0075,0.0425,0.0125,0.025,0.0375)
    }else{
      ticks=c(0.005,0.04,0.014,0.023,0.032)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
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
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
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
                     # ref_line = 0.02,
                     # xlim=c(0.0075,0.031),
                     # ticks_at=c(0.014,0.02,0.026),
                     theme = tm,
                     xlab = paste("IFR of",variant),
                     title = paste("Intrinsic IFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
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
      ticks=c(0.0115,0.033,0.017,0.022,0.027)
    }else if(i==2){
      ticks=c(0.02,0.0525,0.028,0.036,0.045)
    }else{
      ticks=c(0.01,0.09,0.030,0.050,0.070)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
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
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
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
                     # ref_line = 0.039,
                     # xlim=c(0.019,0.059),
                     # ticks_at=c(0.029,0.039,0.049),
                     theme = tm,
                     xlab = paste("IHR of",variant),
                     title = paste("Intrinsic IHR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
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
      ticks=c(0.1,0.375,0.170,0.240,0.310)
    }else if(i==2){
      ticks=c(0.15,0.75,0.300,0.450,0.600)
    }else{
      ticks=c(0,0.7,0.175,0.350,0.525)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
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
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
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
                     # ref_line = 0.335,
                     # xlim=c(0.15,0.52),
                     # ticks_at=c(0.240,0.335,0.430),
                     theme = tm,
                     xlab = paste("HFR of",variant),
                     title = paste("Intrinsic HFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
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
