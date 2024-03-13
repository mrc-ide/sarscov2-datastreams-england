orderly2::orderly_parameters(short_run = FALSE, deterministic = FALSE)

orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "central" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_central.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "fixed_si_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_fixed_si_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "fixed_si_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_fixed_si_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "booster_ve_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_booster_ve_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "booster_ve_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_booster_ve_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "alpha_ve_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_alpha_ve_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "alpha_ve_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_alpha_ve_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "delta_ve_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_delta_ve_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "delta_ve_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_delta_ve_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_death_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_death_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_death_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_death_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_hospi_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_hospi_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_hospi_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_hospi_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_infect_high" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_infect_high.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "crim_infect_low" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_crim_infect_low.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "mu_d_winter" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_mu_d_winter.rds" = "outputs/national_severity.rds"))
orderly2::orderly_dependency("severity_fits_combined", 'latest(parameter:assumptions == "mu_d_summer" && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic)', c("input/national_sev_mu_d_summer.rds" = "outputs/national_severity.rds"))

orderly2::orderly_artefact(
  "Files for external reviews",
  c())
orderly2::orderly_artefact(
  "regional fitting plots and projections for comparison",
  c("outputs/national_ve_scenarios.png", "outputs/national_crim_scenarios.png", "outputs/national_other_scenarios.png"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(purrr)

orderly2::orderly_resource("support.R")
orderly2::orderly_resource("util.R")
source("support.R")
source("util.R")

version_check("sircovid", "0.14.11")
version_check("spimalot", "0.8.23")

dir.create("outputs", FALSE, TRUE)

# Read scenario inputs
national <- load_national_estimates("input/")

# Plot sensitivity analyses
png("outputs/national_ve_scenarios.png", units = "in", width = 14, height = 10, res = 300)
plot_national(national, which = "ve_scenarios")
dev.off()

png("outputs/national_other_scenarios.png", units = "in", width = 14, height = 10, res = 300)
plot_national(national, which = "other_scenarios")
dev.off()

png("outputs/national_crim_scenarios.png", units = "in", width = 14, height = 10, res = 300)
plot_national(national, which = "crim_scenarios")
dev.off()
