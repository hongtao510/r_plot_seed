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


PD_1Acre_summary_sta_all <- readRDS("I:/Fracking/PD/PD_Summary_1hr_sta_all.rds")
PD_1Acre_summary_sta_all$Lot_size <- "1-acre"
PD_1Acre_summary_sta_all$Location <- "AB_ST"

PD_3Acre_summary_sta_all <- readRDS("I:/Fracking/PD_3acre/PD_Summary_1hr_sta_all.rds")
PD_3Acre_summary_sta_all$Lot_size <- "3-acre"
PD_3Acre_summary_sta_all$Location <- "AB_ST"

PD_5Acre_summary_sta_all <- readRDS("I:/Fracking/PD_5acre/PD_Summary_1hr_sta_all.rds")
PD_5Acre_summary_sta_all$Lot_size <- "5-acre"
PD_5Acre_summary_sta_all$Location <- "AB_ST"


BarD_1Acre_summary_sta_all <- readRDS("I:/Fracking/BarD/BarD_Summary_1hr_sta_all.rds")
BarD_1Acre_summary_sta_all$Lot_size <- "1-acre"
BarD_1Acre_summary_sta_all$Location <- "BarD"

BarD_3Acre_summary_sta_all <- readRDS("I:/Fracking/BarD_3acre/BarD_Summary_1hr_sta_all.rds")
BarD_3Acre_summary_sta_all$Lot_size <- "3-acre"
BarD_3Acre_summary_sta_all$Location <- "BarD"

BarD_5Acre_summary_sta_all <- readRDS("I:/Fracking/BarD_5acre/BarD_Summary_1hr_sta_all.rds")
BarD_5Acre_summary_sta_all$Lot_size <- "5-acre"
BarD_5Acre_summary_sta_all$Location <- "BarD"

Rifle_1Acre_summary_sta_all <- readRDS("I:/Fracking/Rifle/RIFLE_Summary_1hr_sta_all.rds")
Rifle_1Acre_summary_sta_all$Lot_size <- "1-acre"
Rifle_1Acre_summary_sta_all$Location <- "RIFLE"

Rifle_3Acre_summary_sta_all <- readRDS("I:/Fracking/Rifle_3acre/RIFLE_Summary_1hr_sta_all.rds")
Rifle_3Acre_summary_sta_all$Lot_size <- "3-acre"
Rifle_3Acre_summary_sta_all$Location <- "RIFLE"

Rifle_5Acre_summary_sta_all <- readRDS("I:/Fracking/Rifle_5acre/RIFLE_Summary_1hr_sta_all.rds")
Rifle_5Acre_summary_sta_all$Lot_size <- "5-acre"
Rifle_5Acre_summary_sta_all$Location <- "RIFLE"



all_summary_data <- rbind(PD_1Acre_summary_sta_all, PD_3Acre_summary_sta_all, PD_5Acre_summary_sta_all, BarD_1Acre_summary_sta_all, BarD_3Acre_summary_sta_all, BarD_5Acre_summary_sta_all, Rifle_1Acre_summary_sta_all, Rifle_3Acre_summary_sta_all, Rifle_5Acre_summary_sta_all)

all_summary_data_sub <- select(all_summary_data, Activity, Station_ID, Data_type, Lot_size, Location, RING, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)


# create xylene
all_summary_data_sub$xylene <- all_summary_data_sub$o.xylene + all_summary_data_sub$m.p.xylene
all_summary_data_sub <- select(all_summary_data_sub, -o.xylene, -m.p.xylene)



# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}





sta_summary <- all_summary_data_sub %>% group_by(Activity, Data_type, Lot_size, Location, RING) %>% summarise_if(is.numeric, mean, na.rm = TRUE)


sta_summary_melt <- melt(sta_summary, id.vars=c("Activity", "Station_ID", "RING", "Data_type", "Location", "Lot_size"))
# sta_summary_melt$logvalue <- log10(sta_summary_melt$value)


sta_summary_melt$RING <- factor(sta_summary_melt$RING, 
                                   levels=ring_list[1:16],
                                   ordered=TRUE)
ggplot(filter(sta_summary_melt, Data_type=="Max-Max", variable=="benzene"), aes(y = value, x =Location, fill = RING))+
      geom_bar(stat="identity", position="dodge")+
      facet_wrap(~Lot_size, scales="free") +
      labs(x = "", y = bquote('VOC Concentrations ('*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(0, 8000))+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      facet_wrap(~Lot_size, scales="free")+
      theme_Publication(15)




# write.csv(sta_summary, "D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/Development_Runs/report_plots/sta_summary.csv")

