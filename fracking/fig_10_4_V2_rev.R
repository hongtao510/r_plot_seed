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



pth <- "I:/Fracking/"
setwd(pth)


ring_list <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", "R6_500FT", 
               "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
               "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
               "R15_1800FT","R16_2000FT")


load_station_results <- function(folder_str, location) {
    Summary_sta_all <- data.frame()

    all_ouput <- list.files(paste0(folder_str), pattern = paste0("^1hr_Development_1Acre_", location))
    all_ouput <- mixedsort(all_ouput)
    # register number of cores
    for (qq in 1:length(all_ouput)){
        cat(qq, "\n")
        All_station_t <- read.xlsx(paste0(folder_str, all_ouput[qq]))
        All_station_t <- All_station_t %>% select(Iter, Activity, Data_type, RING, benzene, toluene, ethylbenzene, o.xylene, m.p.xylene, isoprene)
        Summary_sta_all <- rbind(Summary_sta_all, All_station_t)
    }

    return (Summary_sta_all)
}


BarD_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "BarD")
BarD_1acre$Location <- "BarD"

PD_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "AB_ST")
PD_1acre$Location <- "AB_ST"

RIFLE_1acre <- load_station_results("I:/Fracking/1hr_output_development_rev/", "RIFLE")
RIFLE_1acre$Location <- "RIFLE"

Summary_1acre_all <- rbind(BarD_1acre, PD_1acre, RIFLE_1acre)

Summary_1acre_all$xylene <- Summary_1acre_all$o.xylene + Summary_1acre_all$m.p.xylene
Summary_1acre_all <- select(Summary_1acre_all, -o.xylene, -m.p.xylene)

Summary_1acre_all <- Summary_1acre_all %>% filter(!RING %in% c("R1_150FT", "R2_250FT"))

Summary_sta_all_sub_melt <- melt(Summary_1acre_all, id.vars=c("Activity", "RING",  "Data_type", "Location", "Iter"))
Summary_sta_all_sub_melt$logvalue <- log10(Summary_sta_all_sub_melt$value)
Summary_sta_all_sub_melt$RING <- ordered(Summary_sta_all_sub_melt$RING, levels =ring_list[3:16])


Summary_sta_all_sub_melt$Data_type <- factor(Summary_sta_all_sub_melt$Data_type, 
                                   levels=c("Max", "999%", "99%", "95%", "Median", "Mean"),
                                   ordered=TRUE)


# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


legend_label_text <- c("300FT", "350FT", "400FT", "500FT", 
                       "600FT", "700FT", "800FT", "900FT", 
                       "1000FT", "1200FT", "1400FT", "1600FT", 
                       "1800FT", "2000FT")

ggplot(filter(Summary_sta_all_sub_melt), aes(y = logvalue, x =Location , fill = Data_type))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "", y = bquote('VOC Concentrations (log10 scale, '*ug~ m^-3*')')) +
      scale_y_continuous(limits = c(-6.0, 5.0), breaks = seq(-6.0, 5.0, by = 1))+
      facet_wrap(~variable, scales="free")+
      scale_fill_discrete(name = "Metric", labels = c("Max", "99.9th", "99th", "95th", "Median", "Mean"))+
      geom_vline(xintercept=c(1.5,2.5), linetype="dashed") + 
      theme_Publication(15)+
      guides(fill=guide_legend(nrow=1, byrow=TRUE))











