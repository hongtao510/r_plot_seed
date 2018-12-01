library(dplyr)
library(openxlsx)
library(reshape2)
library(ggplot2)
library(gridExtra) 
library(scales)  
library(extrafont)

loadfonts()

theme_Publication <- function(base_size=14) {
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
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(1, "cm"),
               legend.margin = unit(0.2, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_blank(),#element_rect(colour="#f0f0f0", fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}


pth <- "I:/Fracking/RIFLE/Convergence_check/"
setwd(pth)



###################################
## combine station iter files 
###################################
library(foreach)
library(doParallel)

all_ouput <- list.files("./R16_2000FT", pattern="^R.*rds$")

# register number of cores
cl <- makeCluster(8)
registerDoParallel(cl, cores = 8)


out_station <- foreach(i = 1:length(all_ouput), .combine='rbind', .packages=c('dplyr')) %dopar% {
  iter_temp <- data.frame()
  tryCatch(
     iter_temp <- readRDS(paste0("./R16_2000FT", "/", all_ouput[i])), 
     error=function(e) NULL
     )
     if (nrow(iter_temp)>0){
       names(iter_temp) <- make.names(names(iter_temp))
       iter_temp_station <- iter_temp %>% filter(Station_ID %in% c(1, 20, 40, 60, 80, 100)) %>% 
                        select(Activity, Station_ID, Iter, benzene, toluene, t.2.butene, n.butane)
       rm(iter_temp)
       gc()
       return(iter_temp_station)
      }
}


saveRDS(out_station, "CONV_RIFLE_out_station_R16_2000FT_10k.rds", compress = "xz")

###################################
## end combine station iter files 
###################################

# Drilling: benzene (4+ orders of magnitude)
# Fracking: t-2 butane (5+ orders of magnitude) 
# Flowback: n-butane (5+ orders of magnitude) 


iter_pool_station_raw <- out_station

# out_station_R6_500FT  out_station_R11_1000FT  out_station_R16_2000FT

iter_pool_station_raw <- readRDS("CONV_RIFLE_out_station_R16_2000FT_10k.rds")
# clean up names
names(iter_pool_station_raw) <- make.names(names(iter_pool_station_raw))


iter_pool_station_raw <- ungroup(iter_pool_station_raw)

iter_pool_station <- select(iter_pool_station_raw, Activity, Station_ID, Iter, benzene, toluene, t.2.butene, n.butane)

iter_pool_station_sub <- group_by(iter_pool_station, Iter, Station_ID, Activity) %>% arrange(Iter, Station_ID)

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




iter_pool_station_sub_avg_for_plot <- select(iter_pool_station_sub_max_cum, Iter, Station_ID, Activity, cumu_benzene, cumu_toluene, cumu_t.2.butene, cumu_n.butane)
iter_pool_station_sub_avg_for_plot_melt <- melt(iter_pool_station_sub_avg_for_plot, id.vars=c("Iter", "Station_ID", "Activity"))
iter_pool_station_sub_avg_for_plot_melt$logvalue <- log10(iter_pool_station_sub_avg_for_plot_melt$value)


iter_pool_station_sub_sd_for_plot <- select(iter_pool_station_sub_max_cum, Iter, Station_ID, Activity, cumsd_benzene, cumsd_toluene, cumsd_t.2.butene, cumsd_n.butane)
iter_pool_station_sub_sd_for_plot_melt <- melt(iter_pool_station_sub_sd_for_plot, id.vars=c("Iter", "Station_ID", "Activity"))
iter_pool_station_sub_sd_for_plot_melt$logvalue <- log10(iter_pool_station_sub_sd_for_plot_melt$value)


########### drilling ########
P1_Drilling_mean <- ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Activity=="Drilling"), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Station_ID, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "Iteration", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^3*')'), title="Cummulative Mean for Drilling Activity (R16_2000FT, Max Value)") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication()




# Cumulative Mean of Max Hourly Conc. for Drilling Activity (2000FT)
ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Activity=="Drilling"), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Station_ID, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "Iteration", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(15)



# Cumulative SD of Max Hourly Conc. for Drilling Activity (2000FT)
ggplot(filter(iter_pool_station_sub_sd_for_plot_melt, Activity=="Drilling"), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Station_ID, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-2,4), breaks=seq(-2,4,0.5))+
      labs(x = "Iteration", y = bquote('SD (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(15)




ggplot(filter(iter_pool_station_sub_avg_for_plot_melt, Station_ID==40), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Activity, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "Iteration", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(15)


ggplot(filter(iter_pool_station_sub_sd_for_plot_melt, Station_ID==40), aes(y = logvalue, x = Iter, col=as.factor(variable)))+
      geom_line()+
      facet_wrap(~Activity, scales="free")+
      scale_x_continuous(trans=log10_trans(), breaks=c(10, 50, 100, 500, 1000, 2000, 5000, 10000))+
      scale_y_continuous(limits=c(-4,4), breaks=seq(-4,4,0.5))+
      labs(x = "Iteration", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_colour_discrete(name = "", labels=c("benzene", "toluene", "t.2.butene", "n.butane"))+
      theme_Publication(15)