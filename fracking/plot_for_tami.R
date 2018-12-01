library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
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

theme_Publication_no_legend <- function(base_size=14) {
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
               axis.title.y = element_text(angle=90, vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               # axis.text = element_text(),
               axis.text.x = element_text(angle = 90, hjust = 1),
               axis.line.x = element_line(color="black", size = 1),
               axis.line.y = element_line(color="black", size = 1),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "none",
               legend.direction = "horizontal",
               legend.key.size= unit(1, "cm"),
               legend.margin = unit(0.2, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_blank(),#element_rect(colour="#f0f0f0", fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}


ring_list <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", "R6_500FT", 
               "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
               "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
               "R15_1800FT","R16_2000FT")


ring_label <- c("150FT", "250FT", "300FT", "350FT", "400FT", "500FT", 
               "600FT", "700FT", "800FT", "900FT", 
               "1000FT", "1200FT", "1400FT", "1600FT", 
               "1800FT","2000FT")

data_all_BarD <- readRDS("I:/Fracking/BarD/BarD_selected_per_station_per_ring.rds") %>% select(Activity, Station_ID, Iter, Data_type, RING, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)
data_all_BarD$Location <- "BarD"

data_all_PD <- readRDS("I:/Fracking/PD/PD_selected_per_station_per_ring.rds") %>% select(Activity, Station_ID, Iter, Data_type, RING, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)
data_all_PD$Location <- "AB_ST"

data_all_RIFLE <- readRDS("I:/Fracking/RIFLE/RIFLE_selected_per_station_per_ring.rds") %>% select(Activity, Station_ID, Iter, Data_type, RING, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)
data_all_RIFLE$Location <- "RIFLE"

# combine data
all_data <- rbind(data_all_BarD, data_all_PD, data_all_RIFLE)

# create xylene
all_data$xylene <- all_data$o.xylene + all_data$m.p.xylene
all_data <- select(all_data, -o.xylene, -m.p.xylene)

Summary_sta_all_sub_melt <- melt(all_data, id.vars=c("Activity", "Station_ID", "RING", "Data_type", "Iter", "Location"))
Summary_sta_all_sub_melt$logvalue <- log10(Summary_sta_all_sub_melt$value)


Summary_sta_all_sub_melt$RING <- factor(Summary_sta_all_sub_melt$RING, 
                                   levels=ring_list[1:16],
                                   ordered=TRUE)

Summary_sta_all_sub_melt$Data_type <- factor(Summary_sta_all_sub_melt$Data_type, 
                                   levels=c("Max", "999%", "99%", "95%", "Mean", "Median"),
                                   ordered=TRUE)


# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


###### Fig 1, Distribution of Max Hourly benzene Conc. by Distance
ggplot(filter(Summary_sta_all_sub_melt, Data_type=="Max", variable=="benzene"), aes(y = logvalue, x =Location, fill = RING))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-4.5, 5.5), breaks = seq(-4.5, 5.5, by = 1))+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      theme_Publication(15)


###### Fig 2, Distribution of Max Hourly VOC Conc. by Distance
ggplot(filter(Summary_sta_all_sub_melt, Data_type=="Max"), aes(y = logvalue, x =Location, fill = RING))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-4.5, 5.5), breaks = seq(-4.5, 5.5, by = 1))+
      facet_wrap(~variable, scales="free")+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      theme_Publication(15)



###### Fig 3, VOC concentration by activities

ggplot(filter(Summary_sta_all_sub_melt, Data_type=="Max"), aes(y=logvalue, x=Location, fill = variable))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="Distribution of Max VOC by Activities") +
      scale_y_continuous(limits = c(-4.5, 5.5), breaks = seq(-4.5, 5.5, by = 1))+
      facet_wrap(~Activity, scales="free", ncol=1)+
      scale_fill_discrete(name = "VOCs")+
      theme_Publication()


###### Fig 4, VOC concentration by metric

ggplot(filter(Summary_sta_all_sub_melt), aes(y = logvalue, x =Location , fill = Data_type))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="Distribution of VOC by Metrics") +
      scale_y_continuous(limits = c(-7.0, 5.5), breaks = seq(-7.0, 5.5, by = 1))+
      facet_wrap(~variable, scales="free")+
      scale_fill_discrete(name = "Metric")+
      theme_Publication(15)











