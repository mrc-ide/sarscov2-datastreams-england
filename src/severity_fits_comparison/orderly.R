orderly2::orderly_parameters(short_run = TRUE, deterministic = TRUE)

regions <- c("north_west")
variants <- c("Wildtype", "Alpha", "Delta", "Omicron")

data_streams <- list(deaths_hosp = c(TRUE, FALSE),
                     deaths_comm = c(TRUE, FALSE),
                     icu = c(TRUE, FALSE),
                     general = c(TRUE, FALSE),
                     hosp = c(TRUE, FALSE),
                     admissions = c(TRUE, FALSE),
                     pillar2 = c(TRUE, FALSE),
                     ons = c(TRUE, FALSE),
                     react = c(TRUE, FALSE),
                     strain = c(TRUE, FALSE),
                     sero = c(TRUE, FALSE))
fit_grid <- expand.grid(data_streams, stringsAsFactors = FALSE)
fit_grid <- fit_grid[rowSums(!fit_grid) <= 3, ]
fit_grid <- fit_grid[order(rowSums(!fit_grid)), ]

for (i in seq_len(nrow(fit_grid))) {
  folder_name <- paste(1 * fit_grid[i, ], collapse = "")
  
  deaths_hosp <- fit_grid$deaths_hosp[i] 
  deaths_comm <- fit_grid$deaths_comm[i] 
  icu <- fit_grid$icu[i] 
  general <- fit_grid$general[i]
  hosp <- fit_grid$hosp[i]
  admissions <- fit_grid$admissions[i]
  pillar2 <- fit_grid$pillar2[i]
  ons <- fit_grid$ons[i]
  react <- fit_grid$react[i]
  strain <- fit_grid$strain[i]
  sero <- fit_grid$sero[i]
  
  orderly2::orderly_dependency(
    "severity_fits",
    quote(latest(parameter:deterministic == this:deterministic && 
                   parameter:short_run == this:short_run &&
                   parameter:region == "north_west" &&
                   parameter:deaths_hosp == environment:deaths_hosp && 
                   parameter:deaths_comm == environment:deaths_comm && 
                   parameter:icu == environment:icu && 
                   parameter:general == environment:general &&
                   parameter:hosp == environment:hosp &&
                   parameter:admissions == environment:admissions &&
                   parameter:pillar2 == environment:pillar2 &&
                   parameter:ons == environment:ons &&
                   parameter:react == environment:react &&
                   parameter:strain == environment:strain &&
                   parameter:sero == environment:sero)),
    c("inputs/${folder_name}/convergence_diagnostics.rds" = "outputs/convergence_diagnostics.rds",
      "inputs/${folder_name}/metrics.rds" = "outputs/metrics.rds"))
  
}



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
library(patchwork)


# source ====
orderly2::orderly_shared_resource(global_util.R = "rtm_inference/util_new.R")
orderly2::orderly_resource("convergence_diagnostics.Rmd")
orderly2::orderly_resource("KL_divergence.Rmd")
orderly2::orderly_resource("support.R")
orderly2::orderly_resource("plot.R")
source("global_util.R")
source("plot.R")
source("support.R")

orderly2::orderly_artefact("Convergence diagnostics",
                           "convergence_diagnostics.html")

orderly2::orderly_artefact("KL divergence",
                           "KL_divergence.html")

dat <- load_combined("inputs", fit_grid)

KL <- get_KL_divergence(dat)

rmarkdown::render("KL_divergence.Rmd")
rmarkdown::render("convergence_diagnostics.Rmd")
