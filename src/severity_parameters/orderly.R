orderly2::orderly_parameters(data_changed = "original", deterministic = TRUE, percent_removed = 100)

orderly2::orderly_shared_resource(global_util.R = "rtm_inference/util_new.R")

orderly2::orderly_resource(c("pars",
                             "data/vaccine_efficacy_alpha.csv",
                             "data/vaccine_efficacy_delta.csv",
                             "data/vaccine_efficacy_omicron.csv",
                             "data/vaccine_uptake.csv",
                             "data/support_severity.csv"))

orderly2::orderly_dependency(
  "severity_parsed_data",
  "latest",
  c(data_vaccination.csv = "outputs/data_vaccination.csv",
    weighted_prior_ranges.csv = "outputs/weighted_prior_ranges.csv"))

orderly2::orderly_artefact(
  "fitted hyperparameters for priors",
  c("parameters_base.rds", "parameters_info.csv", "parameters_prior.csv", "parameters_proposal.csv", "parameters_transform.R"))
orderly2::orderly_artefact("supplementary figures", "fig_sup_vacc_age.png")

library(sircovid)
library(spimalot)
library(tidyr)
library(dplyr)
library(forcats)
library(magrittr)
library(ggplot2)
library(scales)

orderly2::orderly_resource("R/support.R")
orderly2::orderly_resource("R/priors.R")
orderly2::orderly_resource("R/baseline.R")
orderly2::orderly_resource("R/transform.R")
orderly2::orderly_resource("R/vaccine.R")
source("R/support.R")
source("R/priors.R")
source("R/baseline.R")
source("R/transform.R")
source("R/vaccine.R")

source("global_util.R")

version_check("sircovid", "0.15.0")
version_check("spimalot", "0.8.25")

## Define date at which the data is capped for analysis
date <- "2021-09-13"
assumptions <- "central"

## Five epochs after starting with a single strain model (without vaccination)
## * mid August 2020: Alpha appears, expand strains
## * early December 2020: vaccination starts, expand vaccine classes 
##                        but without boosters
## * early March 2021: delta appears, expand strains
## * mid September 2021: booster programme starts, expand vaccine classes
## * early November 2021: omicron appears, rotate strains
epoch_dates <- c("2020-09-17", "2020-12-07", "2021-03-08", "2021-09-14", "2021-11-01")

## Load all parameters from the last run; creates priors, and updates
## new entries into the proposal matrix as needed.
pars <- load_mcmc_parameters(data_changed, deterministic, percent_removed)

## The baselines are always region-specific
regions <- sircovid::regions("england")

baseline <- lapply(regions, create_baseline,
                   date, NULL, # setting restart_date to NULL
                   epoch_dates, pars$info, assumptions)
names(baseline) <- regions

message("Writing parameters_info.csv")
write_csv(pars$info, "parameters_info.csv")
message("Writing parameters_proposal.csv")
write_csv(pars$proposal, "parameters_proposal.csv")
message("Writing parameters_prior.csv")
write_csv(pars$prior, "parameters_prior.csv")

message("Writing parameters_base.rds")
saveRDS(baseline, "parameters_base.rds")

message("Writing parameters_transform.R")
fs::file_copy("R/transform.R",
              "parameters_transform.R", overwrite = TRUE)

message("Printing supplementary figures")
png("fig_sup_vacc_age.png", units = "in", width = 6, height = 6, res = 300)
supl_fig_vac_age()
dev.off()

