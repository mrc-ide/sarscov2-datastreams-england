plot_time_series_heatmap <- function(dat, region, metric) {
  
  date <- dat$date[[1]][-1L]
  dat <- dat[[region]]
  
  calc_KL_time_series <- function(changed) {
    vapply(seq_len(nrow(changed))[-1L], 
           function(i) calc_KL_from_sample(dat$reference[i, ], changed[i, ]),
           numeric(1))
  }
  
  KL <- lapply(dat, calc_KL_time_series)
  KL <- do.call(rbind, KL)
  colnames(KL) <- as.character(sircovid::sircovid_date_as_date(date))
  
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
    scale_y_discrete(labels = get_labels(names(dat)))
}


plot_forest <- function(dat, region, variant, metric) {
  dat <- dat[[region]]
  
  get_summary <- function(changed) {
    x <- changed[[variant]]
    mean <- mean(x)
    qs <- quantile(x, c(0.025, 0.975))
    KL <- calc_KL_from_sample(dat$reference[[variant]], x)
    
    data.frame(mean = mean, lb = qs[1], ub = qs[2], KL = KL)
  }
  
  tab <- do.call(rbind, lapply(dat, get_summary))
  tab$name <- factor(rownames(tab), levels = rownames(tab))
  if ((!metric == "R0")) {
    tab[, c("mean", "lb", "ub")] <- tab[, c("mean", "lb", "ub")] * 100
  }
    
  if (metric == "R0") {
    if (variant == "Wildtype") {
      xlim <- c(1.5, 3.5)
    } else if (variant == "Alpha") {
      xlim <- c(3, 6.5)
    } else if (variant == "Delta") {
      xlim <- c(5, 10)
    }
  } else if (metric == "Intrinsic IFR") {
    if (variant == "Wildtype") {
      xlim <- c(0.4, 1.4)
    } else if (variant == "Alpha") {
      xlim <- c(0.75, 4.25)
    } else if (variant == "Delta") {
      xlim <- c(0.5, 4)
    }
  } else if (metric == "Intrinsic IHR") {
    if (variant == "Wildtype") {
      xlim <- c(1.15, 3.3)
    } else if (variant == "Alpha") {
      xlim <- c(2, 5.25)
    } else if (variant == "Delta") {
      xlim <- c(1, 9)
    }
  } else if (metric == "Intrinsic HFR") {
    if (variant == "Wildtype") {
      xlim <- c(10, 37.5)
    } else if (variant == "Alpha") {
      xlim <- c(15, 75)
    } else if (variant == "Delta") {
      xlim <- c(0, 70)
    }
  }

  tab <- setDT(tab)
  heatmap <- ggplot(tab, aes(x = 'x', y = name , fill = KL)) +
    geom_tile(colour = 'gray60') +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0)) +
    scale_fill_gradientn(colours = c("#122B42", "#73B9F8", "#98C9FB"),
                         values = scales::rescale(c(0, 8.5 / 12, 1)),
                         limits = c(0, 12)) +
    theme_void() +
    labs(fill = '') +
    theme(legend.key.width = unit(0.4, 'cm'),
          legend.key.height = unit(0.5, 'cm'))
  p <- ggplotGrob(heatmap)
  pop <- p$grobs[[5]]
  leg <- p$grobs[[14]]
  tm <- forestploter::forest_theme(base_size = 8,
                                   ci_pch = 15,
                                   ci_col = "#2F4F96",
                                   ci_lty = 1,
                                   ci_lwd = 1.5,
                                   ci_Theight = 0.2,
                                   vertline_lwd = 1.5,
                                   vertline_lty= "dashed",
                                   vertline_col= "red",
                                   refline_lwd = 0,
                                   refline_col = "transparent",
                                   footnote_cex = 1.1,
                                   footnote_fontface = "italic",
                                   footnote_col = "blue")
  
  if (metric == "R0") {
    dt <- tab[,.(`Excluded data stream` = get_labels(name),
                 Mean = sprintf("%.4g", mean),
                 ` ` = paste(rep(' ', 100), collapse = ''),
                 `95%CrI` = paste(sprintf("%.4g", lb), sprintf("%.4g", ub), sep=" ~ "),
                 `KL divergence` = paste(rep(' ', 2)))]
  } else {
    dt <- tab[,.(`Excluded data stream` = get_labels(name),
                 Mean = sprintf("%.4g%%", mean),
                 ` ` = paste(rep(' ', 100), collapse = ''),
                 `95%CrI` = paste(sprintf("%.4g%%", lb), sprintf("%.4g%%", ub), sep=" ~ "),
                 `KL divergence` = paste(rep(' ', 2)))]
  }
  
  forest <- forestploter::forest(dt,
                                 est = tab$mean,
                                 lower = tab$lb,
                                 upper = tab$ub,
                                 sizes = 0.6,
                                 ci_column = 3,
                                 vert_line = tab[name == 'reference', mean],
                                 xlim = xlim,
                                 theme = tm,
                                 ref_line = mean(xlim),
                                 xlab = paste(metric, "of", variant),
                                 title = paste(metric, "of", variant, "in", region_to_title(region)))
  tp <- gtable_add_grob(forest, grobs = pop, t = 4, r = 6, b = 15, l = 6)
  tp$widths[6] <- unit(5, 'mm')
  figure <- gtable_add_cols(tp, width = unit(0.8, 'cm'))
  figure <- gtable_add_grob(figure, grobs = leg, t = 4, r = 8, b = 15, l = 8)
  
  print(figure)

}

plot_metrics_heatmap <- function(dat, region) {
  IFR <- spimalot:::list_transpose(dat$intrinsic_ifr[[region]])
  IHR <- spimalot:::list_transpose(dat$intrinsic_ihr[[region]])
  HFR <- spimalot:::list_transpose(dat$intrinsic_hfr[[region]])
  R0 <- spimalot:::list_transpose(dat$R0[[region]])
  
  calc_KL <- function(x) {
    calc_KL_variant <- function(y) {
      out <- lapply(y, function (z) calc_KL_from_sample(y$reference, z))
      do.call(rbind, out)
    }
    
    ret <- lapply(x, calc_KL_variant)
    ret <- do.call(cbind, ret)
    colnames(ret) <- names(x)
    ret
  }
  
  KL <- list(
    IFR = calc_KL(IFR),
    IHR = calc_KL(IHR),
    HFR = calc_KL(HFR),
    R0 = calc_KL(R0))
  
  ggp1 <- function(nm) {
    if (nm == "IHR") {
      y_label <- element_text()
    } else {
      y_label <- element_blank()
    }
    data_melt <- reshape2::melt(KL[[nm]])
    p <- ggplot(data_melt, aes(Var2, Var1)) +
      geom_tile(aes(fill = value)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = y_label) +
      ggtitle(nm) +
      coord_fixed()
    
    if (nm != "R0") {
      p <- p + guides(fill = "none") 
    }
    p
  }
  
  ggp_IHR <- ggp1("IHR")
  ggp_IFR <- ggp1("IFR")
  ggp_HFR <- ggp1("HFR")
  ggp_R0 <- ggp1("R0")
  
  (ggp_IHR | ggp_IFR | ggp_HFR | ggp_R0) + 
    plot_annotation(title = region_to_title(r))
}

plot_parameters_heatmap <- function(dat, regions) {
  for (r in regions) {
    pars <- dat$pars[[r]]
    
    calc_KL <- function(x) {
      par_nms <- colnames(x)
      KL <- vapply(par_nms,
                   function (nm) {
                     calc_KL_from_sample(pars$reference[, nm], x[, nm])
                     }, numeric(1))
    }
    
    KL <- lapply(pars, calc_KL)
    KL <- do.call(rbind, KL)
    
    data_melt <- reshape2::melt(KL)
    tmp <- 
    ggp <- ggplot(data_melt, aes(Var1, Var2)) +
      geom_tile(aes(fill = value)) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
      ggtitle(paste0("KL divergence of parameters in ", region_to_title(r))) +
      theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
      scale_x_discrete(labels = get_labels(names(pars))) +
      scale_fill_gradientn(colours = c("#122B42", "#73B9F8", "#98C9FB"),
                           values = scales::rescale(c(0, 8.5 / 12, 1)),
                           limits = c(0, 12))
    print(ggp)
  }
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

get_labels <- function(names) {
  labels <- c("reference" = "reference", 
             "deaths_hosp" = "hospital deaths",
             "deaths_comm" = "community deaths",
             "icu" = "ICU occupancy",
             "general" = "general bed occupancy",
             "hosp" = "hospital bed occupancy",
             "all_admission" = "hospital admission",
             "pillar2" = "pillar 2",
             "ons" = "ONS PCR testing",
             "react" = "REACT",
             "strain" = "strain",
             "sero" = "serology")
  
  labels[names]
}
