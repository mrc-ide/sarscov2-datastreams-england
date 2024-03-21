load_combined <- function(path, data_changed, custom_names) {
  files <- file.path(path, data_changed, "combined.rds")
  msg <- !file.exists(files)
  if (any(msg)) {
    msg <- sprintf("  - %s", file.path(data_changed[msg], "combined.rds"))
    stop(sprintf("Missing files at '%s':\n%s",
                 path, paste(msg, collapse = "\n")),
         call. = FALSE)
  }
  
  read_rds <- function(filename, d) {
    message(sprintf("Reading combined for data_changed = %s", d))
    readRDS(filename)
  }
  dat <- Map(read_rds, files, data_changed)
  names(data_changed) <- data_changed
  data_changed[names(custom_names)] <- custom_names
  names(dat) <- data_changed
  
  dat <- spimalot:::list_transpose(dat)
  
  nms <- setdiff(names(dat), "diagnostics")
  dat[nms] <- lapply(dat[nms], spimalot:::list_transpose)
  
  dat
}
