orderly2::orderly_parameters(short_run = TRUE, deterministic = TRUE)

regions <- sircovid::regions("england")

#library====

library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(data.table)
library(stringr)
library(RColorBrewer)
library(lubridate)
library(png)
library(gtable)
library(grid)
library(scales)
library(forestploter)
library(ragg)


# source ====
orderly2::orderly_shared_resource(global_util.R = "rtm_inference/util_new.R")
source("global_util.R")
source("plot.R")
source("support.R")


#dependency====

data_changed <- c("original", "deaths_hosp", "deaths_comm", "icu", "general",
                  "hosp", "all_admission", "pillar2", "ons", "react", "strain",
                  "sero")

for (d in data_changed) {
  orderly2::orderly_dependency(
    "severity_fits_combined",
    quote(latest(parameter:data_changed == environment:d && parameter:short_run == this:short_run && parameter:deterministic == this:deterministic && parameter:change_rate == 1)),
    c("inputs/${d}/combined.rds" = "outputs/combined.rds"))
}

#artefact====

#orderly2::orderly_artefact("csv files for storing KL divergence", c("outputs/KL_divergence/pars east_of_england.csv","outputs/KL_divergence/pars london.csv","outputs/KL_divergence/pars midlands.csv","outputs/KL_divergence/pars north_east_and_yorkshire.csv","outputs/KL_divergence/pars north_west.csv","outputs/KL_divergence/pars south_east.csv","outputs/KL_divergence/pars south_west.csv"))
#orderly2::orderly_artefact("heatmap of KL divergence",c("outputs/KL_divergence/KL divergence of parameters in east_of_england.pdf","outputs/KL_divergence/KL divergence of parameters in london.pdf","outputs/KL_divergence/KL divergence of parameters in midlands.pdf","outputs/KL_divergence/KL divergence of parameters in north_east_and_yorkshire.pdf","outputs/KL_divergence/KL divergence of parameters in north_west.pdf","outputs/KL_divergence/KL divergence of parameters in south_east.pdf","outputs/KL_divergence/KL divergence of parameters in south_west.pdf"))
#orderly2::orderly_artefact("ifr",c('outputs/east_of_england ifr distribution.pdf','outputs/london ifr distribution.pdf','outputs/midlands ifr distribution.pdf','outputs/north_east_and_yorkshire ifr distribution.pdf','outputs/north_west ifr distribution.pdf','outputs/south_east ifr distribution.pdf','outputs/south_west ifr distribution.pdf',"outputs/ifr_density_plots.rds","outputs/KL_ifr.rds","outputs/KL_divergence/ifr east_of_england.csv","outputs/KL_divergence/ifr london.csv","outputs/KL_divergence/ifr midlands.csv","outputs/KL_divergence/ifr north_east_and_yorkshire.csv","outputs/KL_divergence/ifr north_west.csv","outputs/KL_divergence/ifr south_east.csv","outputs/KL_divergence/ifr south_west.csv","outputs/ifr.rds"))
orderly2::orderly_artefact("R0",c("outputs/R0.rds",'outputs/KL_R0.rds',"outputs/KL divergence heatmap of R0.png",'outputs/R0_density_plots.rds','outputs/east_of_england R0 distribution.pdf','outputs/london R0 distribution.pdf','outputs/midlands R0 distribution.pdf','outputs/north_east_and_yorkshire R0 distribution.pdf','outputs/north_west R0 distribution.pdf','outputs/south_east R0 distribution.pdf','outputs/south_west R0 distribution.pdf'))
#orderly2::orderly_artefact("Rt",c('outputs/Rt distribution.pdf','outputs/Rt_density_plots.rds','outputs/KL_Rt.rds','outputs/Rt.rds'))
orderly2::orderly_artefact("HFR",c("outputs/HFR.rds",'outputs/KL_HFR.rds','outputs/HFR_density_plots.rds','outputs/east_of_england HFR distribution.pdf','outputs/london HFR distribution.pdf','outputs/midlands HFR distribution.pdf','outputs/north_east_and_yorkshire HFR distribution.pdf','outputs/north_west HFR distribution.pdf','outputs/south_east HFR distribution.pdf','outputs/south_west HFR distribution.pdf'))
orderly2::orderly_artefact("IFR",c("outputs/IFR.rds",'outputs/KL_IFR.rds','outputs/IFR_density_plots.rds','outputs/east_of_england IFR distribution.pdf','outputs/london IFR distribution.pdf','outputs/midlands IFR distribution.pdf','outputs/north_east_and_yorkshire IFR distribution.pdf','outputs/north_west IFR distribution.pdf','outputs/south_east IFR distribution.pdf','outputs/south_west IFR distribution.pdf'))
orderly2::orderly_artefact("IHR",c("outputs/IHR.rds",'outputs/KL_IHR.rds',"outputs/KL divergence heatmap of IHR,IFR,HFR.pdf",'outputs/IHR_density_plots.rds','outputs/east_of_england IHR distribution.pdf','outputs/london IHR distribution.pdf','outputs/midlands IHR distribution.pdf','outputs/north_east_and_yorkshire IHR distribution.pdf','outputs/north_west IHR distribution.pdf','outputs/south_east IHR distribution.pdf','outputs/south_west IHR distribution.pdf'))
orderly2::orderly_artefact("parameters",c("outputs/KL_pars.rds","outputs/pars_density_plots.rds","outputs/pars.rds","outputs/KL divergence heatmap of parameters.pdf",'outputs/east_of_england parameter distribution.pdf','outputs/london parameter distribution.pdf','outputs/midlands parameter distribution.pdf','outputs/north_east_and_yorkshire parameter distribution.pdf','outputs/north_west parameter distribution.pdf','outputs/south_east parameter distribution.pdf','outputs/south_west parameter distribution.pdf'))
orderly2::orderly_artefact("diagnostics","outputs/diagnostics.rds")

orderly2::orderly_artefact("Time series heatmaps",
                           c(paste0("figs/heatmap_HFR_", c(regions, "england"), ".png"),
                             paste0("figs/heatmap_IFR_", c(regions, "england"), ".png"),
                             paste0("figs/heatmap_IHR_", c(regions, "england"), ".png"),
                             paste0("figs/heatmap_Rt_", c(regions, "england"), ".png")))

#load data====

dat <- load_combined("inputs", data_changed)

#====

dir.create('outputs', FALSE, TRUE)
dir.create("figs", FALSE, TRUE)

# Time series heatmaps ====

for (r in c(regions, "england")) {
  write_png(paste0("figs/heatmap_ifr_", r, ".png"), 
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ifr, r, "effective IFR"))

  write_png(paste0("figs/heatmap_ihr_", r, ".png"), 
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$ihr, r, "effective IHR"))
  
  write_png(paste0("figs/heatmap_hfr_", r, ".png"), 
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$hfr, r, "effective HFR"))
  
  write_png(paste0("figs/heatmap_Rt_", r, ".png"), 
            width = 9000, height = 2700, res = 600,
            plot_time_series_heatmap(dat$Rt_eff, r, "effective Rt"))
}


#ifr emergency3 KLD&density====

IFR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(IFR_density_plots)=c(regions,"england")
KL_IFR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_IFR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_IFR[[r]]= matrix(nrow = length(names(IFR[[r]])),ncol=3)
  colnames(KL_IFR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_IFR[[r]]) = names(IFR[[r]])
  IFR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(IFR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  IFR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(IFR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")

  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    IFR_density_plots[[r]][[tempname]] <- as.list(matrix(nrow=length(names(IFR[[r]])),ncol=1))
    names(IFR_density_plots[[r]][[tempname]]) = names(IFR[[r]])
    for (dataOff in names(IFR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(IFR[[r]]$reference[i,])
        colnames(temp)=tempname
        IFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_IFR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(IFR[[r]]$reference[i,],IFR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)

        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        #X<-rbind(reference$y/sum(reference$y),changed$y/sum(changed$y))
        KL<-suppressMessages(philentropy::KL(X))

        KL_IFR[[r]][dataOff,i] <- KL

        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        IFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=IFR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    IFR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' IFR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(IFR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(IFR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(IFR_density_plots,file = 'outputs/IFR_density_plots.rds')
graphics.off()
saveRDS(KL_IFR,file = 'outputs/KL_IFR.rds')

#ihr emergency3 KLD&density====

IHR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(IHR_density_plots)=c(regions,"england")
KL_IHR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_IHR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_IHR[[r]]= matrix(nrow = length(names(IHR[[r]])),ncol=3)
  colnames(KL_IHR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_IHR[[r]]) = names(IHR[[r]])
  IHR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(IHR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  IHR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(IHR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")

  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    IHR_density_plots[[r]][[tempname]] = as.list(matrix(nrow=length(names(IHR[[r]])),ncol=1))
    names(IHR_density_plots[[r]][[tempname]]) = names(IHR[[r]])
    for (dataOff in names(IHR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(IHR[[r]]$reference[i,])
        colnames(temp)=tempname
        IHR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_IHR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(IHR[[r]]$reference[i,],IHR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)

        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-suppressMessages(philentropy::KL(X))

        KL_IHR[[r]][dataOff,i] <- KL

        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        IHR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=IHR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    IHR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' IHR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(IHR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(IHR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(IHR_density_plots,file = 'outputs/IHR_density_plots.rds')
graphics.off()
saveRDS(KL_IHR,file = 'outputs/KL_IHR.rds')

#hfr emergency3 KLD&density====

HFR_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(HFR_density_plots)=c(regions,"england")
KL_HFR=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_HFR)=c(regions,"england")
for (r in c(regions,"england")){
  KL_HFR[[r]]= matrix(nrow = length(names(HFR[[r]])),ncol=3)
  colnames(KL_HFR[[r]]) = c("Wildtype","Alpha","Delta")
  rownames(KL_HFR[[r]]) = names(HFR[[r]])
  HFR_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(HFR_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  HFR_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(HFR_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")

  for (i in 1:3){
    temp=c("Wildtype","Alpha","Delta")
    tempname=temp[i]
    HFR_density_plots[[r]][[tempname]] = as.list(matrix(nrow=length(names(HFR[[r]])),ncol=1))
    names(HFR_density_plots[[r]][[tempname]]) = names(HFR[[r]])
    for (dataOff in names(HFR[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(HFR[[r]]$reference[i,])
        colnames(temp)=tempname
        HFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(get(tempname)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(tempname)
        KL_HFR[[r]][dataOff,i] <- 0
      }else{
        temp=data.frame(HFR[[r]]$reference[i,],HFR[[r]][[dataOff]][i,])
        colnames(temp)=c('reference',dataOff)

        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-suppressMessages(philentropy::KL(X))

        KL_HFR[[r]][dataOff,i] <- KL

        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        HFR_density_plots[[r]][[tempname]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(tempname)
      }
    }
    title=paste(tempname,'in', r)
    grided<-grid.arrange(grobs=HFR_density_plots[[r]][[tempname]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    HFR_density_plots[[r]]$grided_plots[[tempname]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' HFR distribution.pdf'),width =11.69, height =8.27)
  for (j in names(HFR_density_plots[[r]]$grided_plots)){
    print(grid.arrange(HFR_density_plots[[r]]$grided_plots[[j]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(HFR_density_plots,file = 'outputs/HFR_density_plots.rds')
graphics.off()
saveRDS(KL_HFR,file = 'outputs/KL_HFR.rds')

#R0 KLD&density====

R0_density_plots=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(R0_density_plots)=c(regions,"england")
KL_R0=as.list(matrix(nrow = length(regions)+1, ncol=1))
names(KL_R0)=c(regions,"england")
for (r in c(regions,"england")){
  KL_R0[[r]]= matrix(nrow = length(names(R0[[r]])),ncol=length(names(R0[[r]]$reference)))
  colnames(KL_R0[[r]]) = names(R0[[r]]$reference)
  rownames(KL_R0[[r]]) = names(R0[[r]])

  R0_density_plots[[r]]= as.list(matrix(nrow = 4,ncol=1))
  names(R0_density_plots[[r]]) = c('grided_plots',"Wildtype","Alpha","Delta")
  R0_density_plots[[r]]$grided_plots = as.list(matrix(nrow=3,ncol=1))
  names(R0_density_plots[[r]]$grided_plots) = c("Wildtype","Alpha","Delta")

  for (variant in names(R0[[r]]$reference)){
    R0_density_plots[[r]][[variant]] = as.list(matrix(nrow=length(names(R0[[r]])),ncol=1))
    names(R0_density_plots[[r]][[variant]]) = names(R0[[r]])

    for (dataOff in names(R0[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(R0[[r]]$reference)
        temp=as.data.frame(temp[,colnames(temp) %in% variant])
        colnames(temp)=variant
        R0_density_plots[[r]][[variant]][[dataOff]] <-
          ggplot(temp, aes(get(variant)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(variant)
        KL_R0[[r]][dataOff,variant] <- 0
      }else{
        temp=data.frame(R0[[r]]$reference[[variant]],R0[[r]][[dataOff]][[variant]])
        colnames(temp)=c('reference',dataOff)

        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-suppressMessages(philentropy::KL(X))

        KL_R0[[r]][dataOff,variant] <- KL

        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        R0_density_plots[[r]][[variant]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(variant)
      }
    }
    title=paste(variant,'in', r)
    grided<-grid.arrange(grobs=R0_density_plots[[r]][[variant]],newpage = TRUE)
    R0_density_plots[[r]]$grided_plots[[variant]]=grided
    graphics.off()
  }
  pdf(paste0('outputs/',r,' R0 distribution.pdf'),width =11.69, height =8.27)
  for (i in names(R0_density_plots[[r]]$grided_plots)){
    print(grid.arrange(R0_density_plots[[r]]$grided_plots[[i]]))
  }
  dev.off()
  graphics.off()
}
saveRDS(R0_density_plots,file = 'outputs/R0_density_plots.rds')
graphics.off()
saveRDS(KL_R0,file = 'outputs/KL_R0.rds')

#parameter KLD&density====

pars_density_plots=as.list(matrix(nrow = length(regions), ncol=1))
names(pars_density_plots)=regions
KL_pars=as.list(matrix(nrow = length(regions), ncol=1))
names(KL_pars)=regions
for (r in regions){
  pars_density_plots[[r]]= as.list(matrix(nrow = length(colnames(pars[[r]]$reference))+1,ncol=1))
  names(pars_density_plots[[r]]) = c('grided_plots',colnames(pars[[r]]$reference))
  pars_density_plots[[r]]$grided_plots = as.list(matrix(nrow=length(colnames(pars[[r]]$reference)),ncol=1))
  names(pars_density_plots[[r]]$grided_plots) = colnames(pars[[r]]$reference)
  KL_pars[[r]]= matrix(nrow = length(names(pars[[r]])),ncol=length(colnames(pars[[r]]$reference)))
  colnames(KL_pars[[r]]) = colnames(pars[[r]]$reference)
  rownames(KL_pars[[r]]) = names(pars[[r]])

  for (parameter in colnames(pars[[r]]$reference)){
    pars_density_plots[[r]][[parameter]] = as.list(matrix(nrow=length(names(pars[[r]])),ncol=1))
    names(pars_density_plots[[r]][[parameter]]) = names(pars[[r]])

    for (dataOff in names(pars[[r]])){
      if (dataOff=='reference'){
        temp=as.data.frame(pars[[r]]$reference)
        temp=as.data.frame(temp[,colnames(temp) %in% parameter])
        colnames(temp)=parameter
        pars_density_plots[[r]][[parameter]][[dataOff]] <-
          ggplot(temp, aes(get(parameter)))+
          geom_density(color='#f8766d',fill='#f8766d',alpha=0.6)+
          ggtitle('reference')+
          xlab(parameter)
        KL_pars[[r]][dataOff,parameter] <- 0
      }else{
        temp=data.frame(pars[[r]]$reference[,colnames(pars[[r]]$reference) %in% parameter],pars[[r]][[dataOff]][,colnames(pars[[r]][[dataOff]]) %in% parameter])
        colnames(temp)=c('reference',dataOff)

        reference <- density(temp[,1])
        changed <- density(temp[,2])
        common_support <- sort(union(reference$x,changed$x))
        reference_interp <- approx(reference$x,reference$y,xout=common_support,method = 'linear')$y
        changed_interp <- approx(changed$x,changed$y,xout=common_support, method = 'linear')$y
        reference_interp[is.na(reference_interp)] <- 0
        changed_interp[is.na(changed_interp)] <- 0

        X<-rbind(reference_interp/sum(reference_interp),changed_interp/sum(changed_interp))
        KL<-suppressMessages(philentropy::KL(X))

        temp=melt(temp,variable.name='data_off')
        subtitle=paste("origianl &",dataOff, 'KL_div =', round(KL,4))
        pars_density_plots[[r]][[parameter]][[dataOff]] <-
          ggplot(temp, aes(value,fill=data_off,color=data_off))+
          geom_density(alpha=0.6)+
          ggtitle(subtitle)+
          xlab(parameter)
        KL_pars[[r]][dataOff,parameter] <- KL
      }
    }
    title=paste(parameter,'in', r)
    grided<-grid.arrange(grobs=pars_density_plots[[r]][[parameter]],newpage = TRUE)
    #    annotate_figure(grided,top=ggpubr::text_grob(title,face='bold',size=18))
    pars_density_plots[[r]]$grided_plots[[parameter]]=grided
    #    title=paste0(parameter, ' in ', r, '.pdf')
    #    ggsave(title,plot = grided, width =11.69, height =8.27)
    graphics.off()
  }
  pdf(paste0('outputs/',r,' parameter distribution.pdf'),width =11.69, height =8.27)
  for (i in names(pars_density_plots[[r]]$grided_plots)){
    print(grid.arrange(pars_density_plots[[r]]$grided_plots[[i]]))
  }
  dev.off()
  graphics.off()
}

saveRDS(pars_density_plots,file = 'outputs/pars_density_plots.rds')
graphics.off()
saveRDS(KL_pars,file = 'outputs/KL_pars.rds')

#heatmap of parameter; IHR IFR HFR emergency3; R0====

pdf(paste0("outputs/KL divergence heatmap of parameters.pdf"),width =11.69, height =8.27)
for (j in 1:7){
  r=c(regions)[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West")[j]
  data_melt <- melt(KL_pars[[r]])
  tmp=paste0("KL divergence of parameters in ",R)
  ggp <- ggplot(data_melt, aes(Var1, Var2)) +
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(tmp)+
    theme(axis.text.x = element_text(angle = 50, vjust = 0.5))+
    scale_x_discrete(labels = c("reference" = "reference", "hospital_deaths" = "hospital deaths", "community_deaths" = "community deaths", "icu_occupancy" = "ICU occupancy", "general_bed_occupancy" = "general bed occupancy", "hospital_bed_occupancy" = "hospital bed occupancy", "hospital_admission" = "hospital admission", "pillar2" = "pillar 2", "ons_pcr_testing" = "ONS_PCR_testing", "react" = "REACT", "strain" = "strain","serology"="serology"))+
    scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))
  # agg_png(paste0("outputs/KL divergence heatmap of parameters in ",r,".png"),res=600,height = 8000,width = 4000)
  print(ggp)
  # dev.off()
}
dev.off()

pdf(paste0("outputs/KL divergence heatmap of IHR,IFR,HFR.pdf"),width =10, height =7)
for (r in names(KL_IFR)){
  data_melt <- melt(KL_IFR[[r]])
  tmp=paste0("IFR in ",r)
  ggp_IFR <- ggplot(data_melt, aes(Var2, Var1)) +
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
    ggtitle(tmp)+
    guides(fill = FALSE)+
    coord_fixed()
  data_melt <- melt(KL_IHR[[r]])
  tmp=paste0("IHR in ",r)
  ggp_IHR <- ggplot(data_melt, aes(Var2, Var1)) +
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
    ggtitle(tmp)+
    guides(fill = FALSE)+
    coord_fixed()
  data_melt <- melt(KL_HFR[[r]])
  tmp=paste0("HFR in ",r)
  ggp_HFR <- ggplot(data_melt, aes(Var2, Var1)) +
    geom_tile(aes(fill = value))+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
    ggtitle(tmp)+
    coord_fixed()
  grid.arrange(ggp_IHR, ggp_IFR, ggp_HFR, ncol= 3,top=NULL)
}
dev.off()

ggp=as.list(matrix(nrow = length(names(KL_R0)),ncol=1))
names(ggp)=names(KL_R0)
for (r in names(KL_R0)){
  if (r==names(KL_R0)[1]){
    data_melt <- melt(KL_R0[[r]])
    tmp=paste0("R0 in ",r)
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
      ggtitle(r)+
      coord_fixed()+
      guides(fill = FALSE)
  }else if(r==names(KL_R0)[8]){
    data_melt <- melt(KL_R0[[r]])
    tmp=paste0("R0 in ",r)
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
      ggtitle(r)+
      coord_fixed()
  }else{
    data_melt <- melt(KL_R0[[r]])
    ggp[[r]] <- ggplot(data_melt, aes(Var2, Var1)) +
      geom_tile(aes(fill = value))+
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank())+
      ggtitle(r)+
      coord_fixed()+
      guides(fill = FALSE)
  }
}
p<-grid.arrange(grobs=ggp,ncol=length(names(ggp)),top=NULL)
ggsave(paste0("outputs/KL divergence heatmap of R0.png"),p,dpi=600,width=21,height=4.5)
dev.off()

#R0 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(1.5,3.5,2.0,2.5,3.0)
    }else if(i==2){
      ticks=c(3,6.5,3.85,4.75,5.65)
    }else{
      ticks=c(5,10,6.25,7.5,8.5)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(R0[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(R0[[r]]), levels = names(R0[[r]]))
    for (dataOff in names(R0$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(R0[[r]][[dataOff]][[variant]])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_R0[[r]][dataOff,variant]
      temp=quantile(R0[[r]][[dataOff]][[variant]],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }

    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8,
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf("%.3f",mean),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf("%.3f",lower_bound),sprintf("%.3f",upper_bound),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 7.15,
                     # xlim=c(6.8,7.6),
                     # ticks_at=c(7,7.2,7.4),
                     # ref_line = 4.4,
                     # xlim=c(4.0,4.8),
                     # ticks_at=c(4.2,4.4,4.6),
                     # ref_line = 2.6,
                     # xlim=c(2.2,3.0),
                     # ticks_at=c(2.4,2.6,2.8),
                     theme = tm,
                     xlab = paste("R0 of",variant),
                     title = paste("R0 of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/R0 of ",variant, " in ",r,"2.png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/R0 forest plots.rds")



#IFR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.004,0.014,0.006,0.009,0.012)
    }else if(i==2){
      ticks=c(0.0075,0.0425,0.0125,0.025,0.0375)
    }else{
      ticks=c(0.005,0.04,0.014,0.023,0.032)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(IFR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(IFR[[r]]), levels = names(IFR[[r]]))
    for (dataOff in names(IFR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(IFR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_IFR[[r]][dataOff,variant]
      temp=quantile(IFR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }

    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8,
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.02,
                     # xlim=c(0.0075,0.031),
                     # ticks_at=c(0.014,0.02,0.026),
                     theme = tm,
                     xlab = paste("IFR of",variant),
                     title = paste("Intrinsic IFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/IFR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/IFR forest plots.rds")


#IHR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.0115,0.033,0.017,0.022,0.027)
    }else if(i==2){
      ticks=c(0.02,0.0525,0.028,0.036,0.045)
    }else{
      ticks=c(0.01,0.09,0.030,0.050,0.070)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(IHR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(IHR[[r]]), levels = names(IHR[[r]]))
    for (dataOff in names(IHR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(IHR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_IHR[[r]][dataOff,variant]
      temp=quantile(IHR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }

    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8,
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.039,
                     # xlim=c(0.019,0.059),
                     # ticks_at=c(0.029,0.039,0.049),
                     theme = tm,
                     xlab = paste("IHR of",variant),
                     title = paste("Intrinsic IHR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/IHR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/IHR forest plots.rds")


#HFR emergency3 forest plots====

data <- vector("list", length = 8)
names(data)=c(regions,"england")
for (j in 1:8){
  r=c(regions,"england")[j]
  R=c("East of England","London","Midlands","North East and Yorkshire","North West","South East","South West","England")[j]
  data[[r]]<- list(Wildtype=NULL, Alpha=NULL, Delta=NULL)
  for (i in 1:3){
    tmp=c("Wildtype","Alpha","Delta")
    variant=tmp[i]
    if (i==1){
      ticks=c(0.1,0.375,0.170,0.240,0.310)
    }else if(i==2){
      ticks=c(0.15,0.75,0.300,0.450,0.600)
    }else{
      ticks=c(0,0.7,0.175,0.350,0.525)
    }
    data[[r]][[variant]]=data.frame(matrix(nrow = 12, ncol = 5))
    colnames(data[[r]][[variant]])=c("data_stream","mean","lower_bound","upper_bound","KL_divergence")
    rownames(data[[r]][[variant]])=names(HFR[[r]])
    data[[r]][[variant]]$data_stream <- factor(names(HFR[[r]]), levels = names(HFR[[r]]))
    for (dataOff in names(HFR$east_of_england)){
      data[[r]][[variant]][dataOff,"mean"]=mean(HFR[[r]][[dataOff]][i,])
      data[[r]][[variant]][dataOff,"KL_divergence"]=KL_HFR[[r]][dataOff,variant]
      temp=quantile(HFR[[r]][[dataOff]][i,],
                    probs = seq(0.025, 0.975,by=0.005),
                    na.rm = TRUE)
      data[[r]][[variant]][dataOff,"lower_bound"]=unname(temp[1])
      data[[r]][[variant]][dataOff,"upper_bound"]=unname(temp[length(temp)])
    }

    setDT(data[[r]][[variant]])
    heatmap<-ggplot(data[[r]][[variant]],aes(x='x',y=factor(data_stream, levels = levels(data_stream)[length(levels(data_stream)):1]),fill=KL_divergence))+
      geom_tile(colour='gray60')+
      scale_x_discrete(expand=expansion(0))+
      scale_y_discrete(expand=expansion(0))+
      scale_fill_gradientn(colours=c("#122B42","#73B9F8","#98C9FB"),values = scales::rescale(c(0, 8.5 / 12, 1)),limits = c(0, 12))+
      theme_void()+
      labs(fill='')+
      theme(legend.key.width=unit(0.4,'cm'),
            legend.key.height=unit(0.5,'cm'))
    p <- ggplotGrob(heatmap)
    pop <- p$grobs[[5]]
    leg <- p$grobs[[14]]
    tm <- forest_theme(base_size = 8,
                       ci_pch = 15,
                       ci_col = "#2F4F96",
                       ci_lty = 1,
                       ci_lwd = 1.5,
                       ci_Theight = 0.2,
                       vertline_lwd = 1.5,
                       vertline_lty="dashed",
                       vertline_col="red",
                       refline_lwd = 0,
                       refline_col = "transparent",
                       footnote_cex = 1.1,
                       footnote_fontface = "italic",
                       footnote_col = "blue")
    dt<-data[[r]][[variant]][,.(`Excluded data stream`=c("reference", "hospital deaths", "community deaths", "ICU occupancy", "general bed occupancy", "hospital bed occupancy", "hospital admission", "pillar 2", "ONS PCR testing","REACT", "strain" ,"serology"),Mean=sprintf('%.3f%%',mean*100),` `=paste(rep(' ',100),collapse=''),
                                `95%CrI`=paste(sprintf('%.3f%%',lower_bound*100),sprintf('%.3f%%',upper_bound*100),sep=" ~ "),
                                `KL divergence`=paste(rep(' ',2)))]
    forest <- forest(dt,
                     est = data[[r]][[variant]]$mean,
                     lower = data[[r]][[variant]]$lower_bound,
                     upper = data[[r]][[variant]]$upper_bound,
                     sizes = 0.6,
                     ci_column = 3,
                     vert_line = data[[r]][[variant]][data_stream=='reference',mean],
                     ref_line = ticks[4],
                     xlim=ticks[1:2],
                     ticks_at=ticks[3:5],
                     # ref_line = 0.335,
                     # xlim=c(0.15,0.52),
                     # ticks_at=c(0.240,0.335,0.430),
                     theme = tm,
                     xlab = paste("HFR of",variant),
                     title = paste("Intrinsic HFR of", variant, "in", R))
    tp<-gtable_add_grob(forest,grobs=pop,t=4,r=6,b=15,l=6)
    tp$widths[6]<-unit(5,'mm')
    figure<-gtable_add_cols(tp,width=unit(0.8,'cm'))
    figure<-gtable_add_grob(figure,grobs=leg,t=4,r=8,b=15,l=8)
    wh<-get_wh(figure)
    agg_png(paste0("outputs/HFR of ",variant, " in ",r,".png"),res=600,width=wh['width']*1.1,height=wh['height'],unit='in')
    print(figure)
    dev.off()
  }
  dev.off()
}
saveRDS(data,"outputs/HFR forest plots.rds")

#====
