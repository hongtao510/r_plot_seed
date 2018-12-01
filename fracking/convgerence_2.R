library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
library(extrafont)

loadfonts()

theme_Publication <- function(base_size=14, legend_position="bottom") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(),
               axis.text.x = element_text(angle = 90, hjust = 1),
               axis.line.x = element_line(color="black", size = 1),
               axis.line.y = element_line(color="black", size = 1),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = legend_position,
               legend.direction = "horizontal",
               legend.key.size= unit(1, "cm"),
               legend.margin = unit(0.2, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_blank(), #element_rect(colour="#f0f0f0", fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}






iter_pool_station_RIFLE <- readRDS("I:/Fracking/RIFLE/Convergence_check/CONV_RIFLE_out_station_R16_2000FT_10k.rds")
iter_pool_station_RIFLE$Location <- "RIFLE"

iter_pool_station_PD <- readRDS("I:/Fracking/PD/Convergence_check/CONV_PD_out_station_R16_2000FT_10k.rds")
iter_pool_station_PD$Location <- "AB_Vrain"

iter_pool_station_BarD <- readRDS("I:/Fracking/BarD/Convergence_check/CONV_BarD_out_station_R16_2000FT_10k.rds")
iter_pool_station_BarD$Location <- "BarD"




iter_pool_station_all <- rbind(iter_pool_station_RIFLE, iter_pool_station_PD, iter_pool_station_BarD)


names(iter_pool_station_all) <- make.names(names(iter_pool_station_all))


iter_pool_station_all <- ungroup(iter_pool_station_all)

iter_pool_station_sub <- group_by(iter_pool_station_all, Iter, Station_ID, Activity, Location) %>% arrange(Iter, Station_ID)

# calculate max
iter_pool_station_sub_max <- summarise_if(iter_pool_station_sub, is.numeric, max, na.rm = TRUE) %>% group_by(Station_ID, Activity)




# calculate cummulative mean values

cum_cv <- function(x){
    n <- 1:length(x)
    sqrt((cumsum(x^2)-n*cummean(x)^2)/(n-1))/cummean(x)
}


cum_sd <- function(x){
    n <- 1:length(x)
    sqrt((cumsum(x^2)-n*cummean(x)^2)/(n-1))
}


# calculate cumulative
iter_pool_station_sub_max_cum <- mutate(iter_pool_station_sub_max, 
                                   cumu_benzene = cummean(benzene),
                                   cumu_toluene = cummean(toluene),
                                   cumu_t.2.butene = cummean(t.2.butene),
                                   cumu_n.butane = cummean(n.butane),
                                   cumsd_benzene = cum_sd(benzene),
                                   cumsd_toluene = cum_sd(toluene),
                                   cumsd_t.2.butene = cum_sd(t.2.butene),
                                   cumsd_n.butane = cum_sd(n.butane)
                                   )

iter_pool_station_sub_avg_for_plot <- select(iter_pool_station_sub_max_cum, Iter, Station_ID, Activity, cumu_benzene, cumu_toluene, cumu_t.2.butene, cumu_n.butane, Location)
iter_pool_station_sub_avg_for_plot_melt <- melt(iter_pool_station_sub_avg_for_plot, id.vars=c("Iter", "Station_ID", "Activity", "Location"))
iter_pool_station_sub_avg_for_plot_melt$logvalue <- log10(iter_pool_station_sub_avg_for_plot_melt$value)


iter_pool_station_sub_sd_for_plot <- select(iter_pool_station_sub_max_cum, Iter, Station_ID, Activity, cumsd_benzene, cumsd_toluene, cumsd_t.2.butene, cumsd_n.butane, Location)
iter_pool_station_sub_sd_for_plot_melt <- melt(iter_pool_station_sub_sd_for_plot, id.vars=c("Iter", "Station_ID", "Activity", "Location"))
iter_pool_station_sub_sd_for_plot_melt$logvalue <- log10(iter_pool_station_sub_sd_for_plot_melt$value)

# Cummulative Mean for Drilling Activity (R16_2000FT, Max Value)
P_Drilling_mean <- ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Activity=="Drilling", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")

# Cummulative SD for Drilling Activity (R16_2000FT, Max Value)
P_Drilling_sd <- ggplot(filter(iter_pool_station_sub_sd_for_plot_melt, Activity=="Drilling", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")

# grid.arrange(P_Drilling_mean, P_Drilling_sd, ncol=1)

# Cummulative Mean for Flowback Activity (R16_2000FT, Max Value)
P_Flowback_mean <- ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Activity=="Flowback", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")

# Cummulative SD for Flowback Activity (R16_2000FT, Max Value)
P_Flowback_sd <- ggplot(filter(iter_pool_station_sub_sd_for_plot_melt, Activity=="Flowback", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")

# grid.arrange(P_Flowback_mean, P_Flowback_sd, ncol=1)

# Cummulative Mean for Fracking Activity (R16_2000FT, Max Value)
P_Fracking_mean <- ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Activity=="Fracking", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")

# Cummulative SD for Fracking Activity (R16_2000FT, Max Value)
P_Fracking_sd <- ggplot(filter(iter_pool_station_sub_sd_for_plot_melt, Activity=="Fracking", Station_ID==80), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Location, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(legend_position="none")





grid.arrange(P_Drilling_mean, P_Flowback_mean, P_Fracking_mean, ncol=1)

P_Drilling_sd
P_Flowback_sd
P_Fracking_sd

