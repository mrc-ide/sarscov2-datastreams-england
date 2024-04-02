## 1. severity_parsed_data
orderly2::orderly_run("severity_parsed_data")

# data_changed can also be any of:
# deaths_hosp, deaths_comm
# icu, general, hosp
# all_admission, pillar2
# react, ons
# strain, sero

regions <- sircovid::regions("england")
data_changed <- c("original", "deaths_hosp", "deaths_comm", "icu", "general",
                  "hosp", "all_admission", "pillar2", "ons", "react", "strain",
                  "sero")

## 2. severity_parameters 
for (d in data_changed) {
  orderly2::orderly_run("severity_parameters", 
                        parameters = list(deterministic = TRUE,
                                          data_changed = d,
                                          percent_removed = 100))
}
  

## ---------------------------
## Run in the cluster
## ---------------------------

## 1. Basic cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision(method = "pkgdepends",
                             refs = "github::mrc-ide/mcstate@adaptive-v2")

`#----

fit_grid <- expand.grid(regions, data_changed, stringsAsFactors = FALSE)
names(fit_grid) <- c("r", "d")

## 2. Short runs ----
fits <- 
  hipercow::task_create_bulk_expr(
      orderly2::orderly_run('severity_fits',
                            parameters = list(region = r,
                                              short_run = TRUE,
                                              deterministic = TRUE,
                                              data_changed = d,
                                              percent_removed = 100)),
    fit_grid,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8)
  )

## Collect results
res <- hipercow::hipercow_bundle_result(fits$name)

# Combined
combined <- 
  hipercow::task_create_bulk_call(
    function(x) {
      orderly2::orderly_run('severity_fits_combined',
                            parameters = list(short_run = TRUE,
                                              deterministic = TRUE,
                                              data_changed = x,
                                              percent_removed = 100))},
    data_changed,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8))
combined_result <- hipercow::hipercow_bundle_result(combined$name)

# Comparison
comparison <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison',
                        parameters = list(short_run = TRUE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison)


#----

## 3. Long runs ----
fits <- 
  hipercow::task_create_bulk_expr(
    orderly2::orderly_run('severity_fits',
                          parameters = list(region = r,
                                            short_run = FALSE,
                                            deterministic = TRUE,
                                            data_changed = d,
                                            percent_removed = 100)),
    fit_grid,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8)
  )

## Collect results
res <- hipercow::hipercow_bundle_result(fits$name)

# Combined
combined <- 
  hipercow::task_create_bulk_call(
    function(x) {
      orderly2::orderly_run('severity_fits_combined',
                            parameters = list(short_run = FALSE,
                                              deterministic = TRUE,
                                              data_changed = x,
                                              percent_removed = 100))},
    data_changed,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8))
combined_result <- hipercow::hipercow_bundle_result(combined$name)

#comparison
comparison <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison',
                        parameters = list(short_run = FALSE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison)
