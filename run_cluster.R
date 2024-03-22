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
                                        data_changed = "icu",
                                        percent_removed = 100))

## ---------------------------
## Run in the cluster
## ---------------------------

## 1. Basic cluster setup
hipercow::hipercow_init(driver = "windows")
hipercow::hipercow_provision()

regions <- sircovid::regions("england")

#----

## 2. Short runs ----
fits <- 
  hipercow::task_create_bulk_call(
    function(x) {
      orderly2::orderly_run('severity_fits',
                            parameters = list(region = x,
                                              short_run = TRUE,
                                              deterministic = TRUE,
                                              data_changed = "icu",
                                              percent_removed = 100))},
    regions,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8))
batch <- fits$name

## Collect results
res <- hipercow::hipercow_bundle_result(batch)

# Combined
combined <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_combined',
                        parameters = list(short_run = TRUE,
                                          deterministic = TRUE,
                                          data_changed = "icu",
                                          percent_removed = 100)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
combined_result <- hipercow::task_result(combined)

# Comparison
comparison <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison',
                        parameters = list(short_run = TRUE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison)

# Comparison2
comparison2 <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison2',
                        parameters = list(short_run = TRUE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison2)

#----

## 3. Long runs ----
fits <- 
  hipercow::task_create_bulk_call(
    function(x) {
      orderly2::orderly_run('severity_fits',
                            parameters = list(region = x,
                                              short_run = FALSE,
                                              deterministic = TRUE,
                                              data_changed = "icu",
                                              percent_removed = 100))},
    regions,
    resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                             cores = 8))
batch <- fits$name

## Collect results
res <- hipercow::hipercow_bundle_result(batch)

# Combined
combined <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_combined',
                        parameters = list(short_run = FALSE,
                                          deterministic = TRUE,
                                          data_changed = "icu",
                                          percent_removed = 100)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
combined_result <- hipercow::task_result(combined)

#comparison
comparison <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison',
                        parameters = list(short_run = FALSE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison)

#comparison2
comparison2 <- hipercow::task_create_expr(
  orderly2::orderly_run('severity_fits_comparison2',
                        parameters = list(short_run = FALSE,
                                          deterministic = TRUE)),
  resources = hipercow::hipercow_resources(queue = 'AllNodes',
                                           cores = 8)
)
comparison_result <- hipercow::task_result(comparison2)
