deterministic <- TRUE
short_run <- TRUE

# data_changed can also be any of:
# deaths_hosp, deaths_comm
# icu, general, hosp
# all_admission, pillar2
# react, ons
# strain, sero
data_changed <- "original"
change_rate <- 1

## 1. severity_parsed_data
orderly2::orderly_run("severity_parsed_data")

## 2. severity_parameters 
orderly2::orderly_run(
  "severity_parameters",
  parameters = list(deterministic = deterministic,
                    data_changed = data_changed,
                    change_rate = change_rate))

## 3. severity_fits
for (r in sircovid::regions("england")) {
  orderly2::orderly_run(
    "severity_fits",
    parameters = list(region = r,
                      short_run = short_run,
                      deterministic = deterministic,
                      data_changed = data_changed,
                      change_rate = change_rate))
}


## 4. severity_fits_combined
orderly2::orderly_run(
  "severity_fits_combined",
  parameters = list(short_run = short_run,
                    deterministic = deterministic,
                    data_changed = data_changed,
                    change_rate = change_rate))

## 5. severity_fits_comparison
orderly2::orderly_run(
  "severity_fits_comparison",
  parameters = list(short_run = short_run))

orderly2::orderly_run(
  "severity_fits_comparison2",
  parameters = list(short_run = short_run))
