library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
library(extrafont)
library(gtools)

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


load_data <- function(work_pth) {
  setwd(work_pth)
  ring_val <- "R3_300FT"
  data_all <- rbind()
  for (qq in 1:36){
    cat(qq, "\n")
      Drilling_station_id <- qq
      Flowback_station_id <- qq
      Fracking_station_id <- qq

      data_temp <- read.csv(paste0("./", ring_val, "/1hr_All_data_station_", ring_val, "_", Drilling_station_id, ".csv"), stringsAsFactors=FALSE) %>% 
                             filter(Data_type=="Max") %>% select(Activity, Station_ID, Iter, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)
      data_temp$RING <- ring_val
      data_all <- rbind(data_all, data_temp)
  }
  return (data_all)
}


PB_data <- load_data("I:/Fracking/PD/")
PB_data$Location <- "AB_ST"
BarD_data <- load_data("I:/Fracking/BarD/")
BarD_data$Location <- "BarD"
RIFLE_data <- load_data("I:/Fracking/RIFLE/")
RIFLE_data$Location <- "RIFLE"

all_data <- rbind(PB_data, BarD_data, RIFLE_data)

# create xylene
all_data$xylene <- all_data$o.xylene + all_data$m.p.xylene
all_data <- select(all_data, -o.xylene, -m.p.xylene)

Summary_sta_all_sub_melt <- melt(all_data, id.vars=c("Activity", "Station_ID", "RING",  "Iter", "Location"))
Summary_sta_all_sub_melt$logvalue <- log10(Summary_sta_all_sub_melt$value)


# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}




ggplot(filter(Summary_sta_all_sub_melt, variable=="benzene"), aes(y = logvalue, x =Station_ID))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Receptor", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-1.5, 3.5), breaks = seq(-1.5, 3.5, by = 0.5))+
      facet_wrap(~Location, scales="free")+
      # scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      theme_Publication(15)







