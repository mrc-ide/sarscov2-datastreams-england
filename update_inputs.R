update_inputs <- function(region, data_streams) {
  
  ## Set working directory to project directory
  setwd(orderly2:::orderly_src_root(NULL, TRUE))
  
  ## Find the latest combined report for the given data_changed
  latest <- 
    orderly2::orderly_search(quote(latest(parameter:region == this:region &&
                                            parameter:deterministic == this:deterministic && 
                                            parameter:deaths_hosp == this:deaths_hosp && 
                                            parameter:deaths_comm == this:deaths_comm && 
                                            parameter:icu == this:icu && 
                                            parameter:general == this:general && 
                                            parameter:hosp == this:hosp &&
                                            parameter:admissions == this:admissions &&
                                            parameter:pillar2 == this:pillar2 &&
                                            parameter:ons == this:ons &&
                                            parameter:react == this:react &&
                                            parameter:strain == this:strain &&
                                            parameter:sero == this:sero)),
                             name = "severity_fits",
                             parameters = c(region = region,
                                            deterministic = TRUE,
                                            c(data_streams)))
  
  ## Folder location in severity_parameters
  pars_folder <- paste0("src/severity_parameters/pars/", paste(1 * data_streams, collapse = ""), "/deterministic")
  
  info <- read.csv(paste0("archive/severity_fits/", latest, "/outputs/info.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  proposal <- read.csv(paste0("archive/severity_fits/", latest, "/outputs/proposal.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  
  if (!file.exists(pars_folder)) {
    dir.create(pars_folder, FALSE, TRUE)
  } else {
    info_old <- read.csv(file.path(pars_folder, "info.csv"), stringsAsFactors = FALSE, check.names = FALSE)
    prop_old <- read.csv(file.path(pars_folder, "proposal.csv"), stringsAsFactors = FALSE, check.names = FALSE)
    info <- rbind(info_old[info_old$region != region, ], info)
    proposal <- rbind(prop_old[prop_old$region != region, ], proposal)
  }
  write.csv(info, file.path(pars_folder, "info.csv"), row.names = FALSE)
  write.csv(proposal, file.path(pars_folder, "proposal.csv"), row.names = FALSE)
}

