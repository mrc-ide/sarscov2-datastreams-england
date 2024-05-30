## 1. severity_parsed_data
orderly2::orderly_run("severity_parsed_data")

## orderly parameter setup
regions <- sircovid::regions("england")
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
  

## ---------------------------
## Run in the cluster
## ---------------------------

## 1. Basic cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision(method = "pkgdepends",
                             refs = "github::mrc-ide/mcstate@adaptive-v2")

`#----

## 2. severity_parameters 
parameters <- hipercow::task_create_bulk_expr(
  orderly2::orderly_run("severity_parameters", 
                        parameters = list(deterministic = TRUE,
                                          deaths_hosp = deaths_hosp,
                                          deaths_comm = deaths_comm,
                                          icu = icu,
                                          general = general,
                                          hosp = hosp,
                                          admissions = admissions,
                                          pillar2 = pillar2,
                                          ons = ons,
                                          react = react,
                                          strain = strain,
                                          sero = sero)),
  fit_grid,
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 1))

## 2. Short runs ----
fits <- 
  hipercow::task_create_bulk_expr(
      orderly2::orderly_run('severity_fits',
                            parameters = list(region = r,
                                              short_run = TRUE,
                                              deterministic = TRUE,
                                              deaths_hosp = deaths_hosp,
                                              deaths_comm = deaths_comm,
                                              icu = icu,
                                              general = general,
                                              hosp = hosp,
                                              admissions = admissions,
                                              pillar2 = pillar2,
                                              ons = ons,
                                              react = react,
                                              strain = strain,
                                              sero = sero)),
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
