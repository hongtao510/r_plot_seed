
library(xlsx)
library(dplyr)
library(ggplot2)
set.seed(666)

###############################
#####Load the raw data#########
###############################

pth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/Version_Feb_22_2016"

data_all_raw <- read.xlsx(paste(pth, "/4_08_Norovirus_Raw_Data_1-11-16.xlsx", sep=""), sheetIndex = 1)
names(data_all_raw) <- c("Study", "Study_ID", "Location", "Plant", "Season", "Date", "Type", "Density_gc_per_L",
                     "Density_gc_per_L_log10", "Below LOD (0/1)")

##############################################################################################
# removed studies
data_all_rm_raw <- data_all_raw[data_all_raw$Study %in% c("Sima, 2011", "Perez-Sautu, 2012"), ]

data_all_rm_raw %>% group_by(Study) %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )
##############################################################################################

theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
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


# create a boxplot
ggplot(data_all_raw, aes(x=Study, y=Density_gc_per_L_log10, fill=Study)) +
geom_boxplot(alpha = 0.4)+
geom_point(position = position_jitter(width = 0.2))+
		scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
		labs(x = "", y = "Norovirus Density (log10 gc/L)",
		    title="Boxplots of Norovirus Densities by Studies")+
		theme_Publication()






# removing Perez-Sautu and Sima 
data_all_raw <- data_all_raw[!data_all_raw$Study %in% c("Sima, 2011", "Perez-Sautu, 2012"), ]
# drop unused levels
data_all_raw$Study <- droplevels(data_all_raw$Study)





data_all <- subset(data_all_raw, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))
data_all <- data_all[which(data_all$Type=="GI" | data_all$Type=="GII"), ]

data_all_raw_1 <- data_all_raw[which(data_all_raw$Type=="GI"),]
data_all_raw_2 <- data_all_raw[which(data_all_raw$Type=="GII"),]
data_all_1 <- subset(data_all_raw_1, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))
data_all_2 <- subset(data_all_raw_2, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))
