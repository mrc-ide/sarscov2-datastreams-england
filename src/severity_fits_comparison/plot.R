plot_time_series_heatmap <- function(dat, region, metric) {
  
  dat <- dat[[region]]
  
  calc_KL_time_series <- function(changed) {
    vapply(seq_len(nrow(changed))[-1L], 
           function(i) calc_KL_from_sample(dat$reference[i, ], changed[i, ]),
           numeric(1))
  }
  
  KL <- lapply(dat, calc_KL_time_series)
  KL <- do.call(rbind, KL)
  colnames(KL) <- as.character(sircovid::sircovid_date_as_date(c(76:622)))
  
  temp <- reshape2::melt(KL)
  names(temp) <- c("data_stream", "date", "value")
  temp$date <- as.Date(temp$date)
  
  ggplot(temp, aes(x = date, y = data_stream, fill = value)) +
    geom_tile(height = 0.95, colour = NA) +
    theme_bw() +
    scale_fill_gradientn(colours = c("#122B42", "#73B9F8", "#98C9FB"),
                         values = scales::rescale(c(0, 8.5 / 12, 1)),
                         limits = c(0, 12)) +
    labs(x = 'Date', y = '', fill = '') +
    theme(legend.key.height = unit(1.7, 'cm'), panel.grid = element_blank()) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y")+
    ggtitle(paste0("KL divergence heatmap of ", metric, " in ", 
                   region_to_title(region))) +
    scale_y_discrete(labels = c("reference" = "reference", 
                                "hospital_deaths" = "hospital deaths",
                                "community_deaths" = "community deaths",
                                "icu_occupancy" = "ICU occupancy",
                                "general_bed_occupancy" = "general bed occupancy",
                                "hospital_bed_occupancy" = "hospital bed occupancy",
                                "hospital_admission" = "hospital admission",
                                "pillar2" = "pillar 2",
                                "ons_pcr_testing" = "ONS PCR testing",
                                "react" = "REACT",
                                "strain" = "strain",
                                "serology"="serology"))
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
  
  suppressMessages(philentropy::KL(X))
}

region_to_title <- function(region) {
  titles <- list(east_of_england = "East of England",
                 london = "London",
                 midlands = "Midlands",
                 north_east_and_yorkshire = "North East and Yorkshire",
                 north_west = "North West",
                 south_east = "South East",
                 south_west = "South West",
                 england = "England")
  
  titles[[region]]
} 
