library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
library(extrafont)
library(gtools)
library(openxlsx)


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




ring_label <- c("150FT", "250FT", "300FT", "350FT", "400FT", "500FT", 
               "600FT", "700FT", "800FT", "900FT", 
               "1000FT", "1200FT", "1400FT", "1600FT", 
               "1800FT","2000FT")


ring_list <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", "R6_500FT", 
               "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
               "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
               "R15_1800FT","R16_2000FT")



pth <- "I:/Fracking/"
setwd(pth)


ring_list <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", "R6_500FT", 
               "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
               "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
               "R15_1800FT","R16_2000FT")


load_station_results <- function(folder_str, location, size) {
    Summary_sta_all <- data.frame()

    all_ouput <- list.files(paste0(folder_str), pattern = paste0("^1hr_Development_", size, "_", location))
    all_ouput <- mixedsort(all_ouput)

    cat(size, "\n")
    # register number of cores
    for (qq in 1:length(all_ouput)){
        cat(qq, "\n")
        All_station_t <- read.xlsx(paste0(folder_str, all_ouput[qq]))
        All_station_t <- All_station_t %>% select(Iter, Activity, Data_type, RING, benzene, toluene, ethylbenzene, o.xylene, m.p.xylene, isoprene)
        Summary_sta_all <- rbind(Summary_sta_all, All_station_t)
    }

    return (Summary_sta_all)
}



BarD_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "BarD", "1Acre")
BarD_1acre$Location <- "BarD"
BarD_1acre$Lot_size <- "1-acre"

PD_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "AB_ST", "1Acre")
PD_1acre$Location <- "AB_ST"
PD_1acre$Lot_size <- "1-acre"

RIFLE_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "RIFLE", "1Acre")
RIFLE_1acre$Location <- "RIFLE"
RIFLE_1acre$Lot_size <- "1-acre"


BarD_3acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "BarD", "3Acre")
BarD_3acre$Location <- "BarD"
BarD_3acre$Lot_size <- "3-acre"

PD_3acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "AB_ST", "3Acre")
PD_3acre$Location <- "AB_ST"
PD_3acre$Lot_size <- "3-acre"

RIFLE_3acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "RIFLE", "3Acre")
RIFLE_3acre$Location <- "RIFLE"
RIFLE_3acre$Lot_size <- "3-acre"


BarD_5acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "BarD", "5Acre")
BarD_5acre$Location <- "BarD"
BarD_5acre$Lot_size <- "5-acre"

PD_5acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "AB_ST", "5Acre")
PD_5acre$Location <- "AB_ST"
PD_5acre$Lot_size <- "5-acre"

RIFLE_5acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "RIFLE", "5Acre")
RIFLE_5acre$Location <- "RIFLE"
RIFLE_5acre$Lot_size <- "5-acre"



all_summary_data <- rbind(BarD_1acre, BarD_3acre, BarD_5acre, PD_1acre, PD_3acre, PD_5acre, RIFLE_1acre, RIFLE_3acre, RIFLE_5acre)

all_summary_data <- all_summary_data %>% filter(!RING %in% c("R1_150FT", "R2_250FT"))

all_summary_data$logvalue <- log10(all_summary_data$benzene)
all_summary_data$RING <- ordered(all_summary_data$RING, levels =ring_list[3:16])

# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(filter(all_summary_data, Data_type=="Max"), aes(y = logvalue, x =Lot_size , fill = RING))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')')) +
      scale_y_continuous(limits = c(-2.5, 3.5), breaks = seq(-2.5, 3.5, by = 0.5))+
      facet_wrap(~Location, scales="free")+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label[3:16])+
      theme_Publication(15) + 
      guides(fill=guide_legend(nrow=2, byrow=TRUE))




