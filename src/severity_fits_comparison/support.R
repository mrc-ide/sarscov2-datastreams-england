
load_combined <- function(path, fit_grid) {
  
  load1 <- function(i) {
    data_streams <- fit_grid[i, ]
    
    folder_name <- file.path(path, paste(1 * data_streams, collapse = ""))
    
    convergence_diagnostics <- readRDS(paste0(folder_name, "/convergence_diagnostics.rds"))
    metrics <- readRDS(paste0(folder_name, "/metrics.rds"))
    
    list(data_streams = data_streams,
         convergence_diagnostics = convergence_diagnostics,
         metrics = metrics)
  }
  
  lapply(seq_len(nrow(fit_grid)), load1)
}

calc_KL_from_sample <- function(reference, changed) {
  density_reference <- density(reference)
  density_changed <- density(changed)
  
  common_support <- sort(union(density_reference$x, density_changed$x))
  
  interp <- function(dens) {
    z <- approx(dens$x, dens$y, xout = common_support, method = 'linear')$y
    z[is.na(z)] <- 0
    z / sum(z)
  }
  
  interp_reference <- interp(density_reference)
  interp_changed <- interp(density_changed)
  
  X <- rbind(interp_reference, interp_changed)
  
  suppressMessages(unname(philentropy::KL(X)))
}

get_KL_divergence <- function(dat) {
  
  reference <- dat[[1L]]
  if (!all(reference$data_streams)) {
    stop("Reference should be first in list")
  }
  
  get_KL1 <- function(x) {
    out <- Map(function (a, b) Map(calc_KL_from_sample, a, b),
               reference$metrics, x$metrics)
    out$data_streams <- x$data_streams
    out
  }
  
  lapply(dat, get_KL1)
}