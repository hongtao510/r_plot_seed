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


BarD_1Acre_summary_sta_all_new <- readRDS("I:/Fracking/BarD/BarD_Summary_1hr_sta_all.rds")
BarD_1Acre_summary_sta_all_new$Lot_size <- "1-acre"
BarD_1Acre_summary_sta_all_new$Location <- "BarD"
BarD_1Acre_summary_sta_all_new$emission_type <- "1hr"

BarD_1Acre_summary_sta_all_old <- readRDS("I:/Fracking/BarD/BarD_Summary_sta_all.rds")
BarD_1Acre_summary_sta_all_old$Lot_size <- "1-acre"
BarD_1Acre_summary_sta_all_old$Location <- "BarD"
BarD_1Acre_summary_sta_all_old$emission_type <- "3min"


all_summary_data <- rbind(BarD_1Acre_summary_sta_all_new, BarD_1Acre_summary_sta_all_old)

all_summary_data_sub <- select(all_summary_data, Activity, Station_ID, Data_type, Lot_size, Location, RING, emission_type, benzene, toluene, ethylbenzene, m.p.xylene, o.xylene, isoprene)


# create xylene
all_summary_data_sub$xylene <- all_summary_data_sub$o.xylene + all_summary_data_sub$m.p.xylene
all_summary_data_sub <- select(all_summary_data_sub, -o.xylene, -m.p.xylene)



# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}





# # sta_summary <- all_summary_data_sub %>% group_by(Activity, Data_type, Lot_size, Location, RING) %>% summarise_if(is.numeric, mean, na.rm = TRUE)


sta_summary_melt <- melt(all_summary_data_sub, id.vars=c("Activity", "Station_ID", "RING", "Data_type", "Location", "Lot_size", "emission_type"))
# # sta_summary_melt$logvalue <- log10(sta_summary_melt$value)


sta_summary_melt$RING <- factor(sta_summary_melt$RING, 
                                   levels=ring_list[1:16],
                                   ordered=TRUE)
p1 <- ggplot(filter(sta_summary_melt, Data_type=="Median-Max", variable=="xylene", Activity=="Flowback"), aes(y = value, x =Location, fill = RING))+
      geom_bar(stat="identity", position="dodge")+
      labs(x = "", y = bquote('VOC Concentrations ('*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(0, 5000))+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      facet_wrap(~emission_type, scales="free")+
      ggtitle("Flowback, xylene, Median-Max") +
      theme_Publication(15)


p2 <- ggplot(filter(sta_summary_melt, Data_type=="Median-Max", variable=="xylene", Activity=="Fracking"), aes(y = value, x =Location, fill = RING))+
      geom_bar(stat="identity", position="dodge")+
      labs(x = "", y = bquote('VOC Concentrations ('*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(0, 5000))+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      facet_wrap(~emission_type, scales="free")+
      ggtitle("Fracking, xylene, Median-Max") +
      theme_Publication(15)



p3 <- ggplot(filter(sta_summary_melt, Data_type=="Median-Max", variable=="xylene", Activity=="Drilling"), aes(y = value, x =Location, fill = RING))+
      geom_bar(stat="identity", position="dodge")+
      labs(x = "", y = bquote('VOC Concentrations ('*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(0, 300))+
      scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      facet_wrap(~emission_type, scales="free")+
      ggtitle("Drilling, xylene, Median-Max") +
      theme_Publication(15)

grid.arrange(p1, p2, p3, ncol=2)


########################################################################
library(dplyr)



BarD_1hr <- read.csv("I:/Fracking/BarD/R3_300FT/1hr_All_data_station_R3_300FT_1.csv")
BarD_1hr$type <- "1hr"
BarD_1hr_sub <- select(BarD_1hr, Activity, Station_ID, Iter, Data_type, ethane_1hr = ethane, benzene_1hr = benzene)

BarD_3min <- read.csv("I:/Fracking/BarD/R3_300FT/All_data_station_R3_300FT_1.csv")
BarD_3min$type <- "3min"
BarD_3min_sub <- select(BarD_3min, Activity, Station_ID, Iter, Data_type, ethane_3min = ethane, benzene_3min = benzene)

BarD_pool = left_join(BarD_1hr_sub, BarD_3min_sub, by = c("Iter" = "Iter", "Activity" = "Activity", "Data_type"="Data_type"))


BarD_pool$ethane_1hr_2_3min <- BarD_pool$ethane_1hr/BarD_pool$ethane_3min
BarD_pool$benzene_1hr_2_3min <- BarD_pool$benzene_1hr/BarD_pool$benzene_3min


emission_values <- readRDS("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/Development_Runs/_Correct/ef_BarD_1acre_1hrto3min_2000.rds")

emission_values_drilling <- emission_values$em_1hrto3min_sampled_drilling_fixed_ratio
emission_values_fracking <- emission_values$em_1hrto3min_sampled_fracking_fixed_ratio
emission_values_flowback <- emission_values$em_1hrto3min_sampled_flowback_fixed_ratio

emission_df <- rbind(emission_values_drilling, emission_values_drilling, emission_values_drilling, 
                     emission_values_drilling, emission_values_drilling, emission_values_drilling,
                     emission_values_fracking, emission_values_fracking, emission_values_fracking,
                     emission_values_fracking, emission_values_fracking, emission_values_fracking,
                     emission_values_flowback, emission_values_flowback, emission_values_flowback,
                     emission_values_flowback, emission_values_flowback, emission_values_flowback
                     )


emission_df_sub <- select(emission_df, Operation_Type, em_Sample_ID2 = Sample_ID2, em_ethane_1hr_2_3min = ethane, em_benzene_1hr_2_3min=benzene)

exp_df <- cbind(BarD_pool, emission_df_sub)

exp_df <- select(exp_df, 
                 Activity, 
                 Station_ID=Station_ID.x,
                 Iter = Iter,
                 em_Sample_ID2,
                 Data_type = Data_type,
                 ethane_1hr,
                 ethane_3min,
                 ethane_1hr_2_3min,
                 em_ethane_1hr_2_3min,
                 benzene_1hr,
                 benzene_3min,
                 benzene_1hr_2_3min,
                 em_benzene_1hr_2_3min
          )



write.csv(exp_df, "D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/Development_Runs/_Correct/quick_QA_R3_300FT_1.csv")


# write.csv(emission_values_fracking, "D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/Development_Runs/_Correct/emission_values_fracking.csv")
# write.csv(emission_values_flowback, "D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/Development_Runs/_Correct/emission_values_flowback.csv")


