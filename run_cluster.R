## 1. severity_parsed_data
orderly2::orderly_run("severity_parsed_data")

# data_changed can also be any of:
# deaths_hosp, deaths_comm
# icu, general, hosp
# all_admission, pillar2
# react, ons
# strain, sero
## 2. severity_parameters 
orderly2::orderly_run("severity_parameters", 
                      parameters = list(deterministic = TRUE,
                                        data_changed = "deaths_comm",
                                        change_rate=0.9))

## ---------------------------
## Run in the cluster
## ---------------------------

## 1. Context ----
setwd(orderly2:::orderly_src_root(NULL, TRUE))
packages <- c("sircovid", "lubridate", "coda", "tidyr", "ggplot2",
              "viridisLite", "orderly2", 'vaultr', 'readxl', "ggtext",
              'abind', 'here', "mcstate", "dust", "spimalot", "purrr",
              "stringr", "ggrepel", "naniar", "desplot", "rmarkdown",
              "jtools", "DescTools", "car", "data.table", "reshape2",
              "gridExtra", "ggpubr", "gdata", "philentropy","png",
              "gtable","grid","scales","forestploter","ragg")
src <- conan::conan_sources(NULL,
                            repos = c("https://ncov-ic.r-universe.dev",
                                      "https://mrc-ide.r-universe.dev"))
ctx <- context::context_save("contexts",
                             packages = packages,
                             package_sources = src)
cfg <- didehpc::didehpc_config(cluster = "wpia-hn",
                               template = 'AllNodes',
                               cores = 8)
obj <- didehpc::queue_didehpc(ctx, config = cfg)


regions <- sircovid::regions("england")

#----

## 2. Short runs ----
fits <- 
  obj$lapply(X = regions,
             FUN = function(x) {
               orderly2::orderly_run('severity_fits',
                                     parameters = list(region = x,
                                                       short_run = TRUE,
                                                       deterministic = TRUE,
                                                       data_changed = "icu",
                                                       change_rate = 1))})
batch <- fits$name

## Collect results
res <- obj$task_bundle_get(batch)$results()

# Combined
combined <- obj$enqueue(orderly2::orderly_run('severity_fits_combined',
                                              parameters = list(short_run = TRUE,
                                                                deterministic = TRUE,
                                                                data_changed = "original",
                                                                change_rate = 1)))
combined_result <- combined$result()

#----

## 3. Long runs ----
fits <- 
  obj$lapply(X = c("north_east_and_yorksire"),
             FUN = function(x) {
               orderly2::orderly_run('severity_fits',
                                     parameters = list(region = x,
                                                       short_run = FALSE,
                                                       deterministic = TRUE,
                                                       data_changed = "deaths_comm",
                                                       change_rate = 0.9))})
batch <- fits$name

## Collect results
res <- obj$task_bundle_get(batch)$results()

# Combined
combined <- obj$enqueue(orderly2::orderly_run('severity_fits_combined',
                                              parameters = list(short_run = FALSE,
                                                                deterministic = TRUE,
                                                                data_changed = "deaths_comm",
                                                                change_rate = 0.9)))
combined_result <- combined$result()

#comparison
comparison <- obj$enqueue(orderly2::orderly_run('severity_fits_comparison',
                                                parameters = list(short_run = FALSE,
                                                                  deterministic = TRUE)))

comparison_result <- comparison$result()

comparison2 <- obj$enqueue(orderly2::orderly_run('severity_fits_comparison2',
                                                parameters = list(short_run = FALSE,
                                                                  deterministic = TRUE)))
