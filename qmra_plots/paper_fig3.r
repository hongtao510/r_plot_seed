library(ggplot2)
library(gridExtra)
library(reshape2)
library(xlsx)

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

#####Load the raw data#################
set.seed(666)
pth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/Version_Feb_22_2016"
setwd(pth)

data_all_raw <- read.xlsx(paste(pth, "/4_08_Norovirus_Raw_Data_1-11-16.xlsx", sep=""), sheetIndex = 1)
names(data_all_raw) <- c("Study", "Study_ID", "Location", "Plant", "Season", "Date", "Type", "Density_gc_per_L",
                     "Density_gc_per_L_log10", "Below LOD (0/1)")

# removing Perez-Sautu and Sima 
data_all_raw <- data_all_raw[!data_all_raw$Study %in% c("Sima, 2011", "Perez-Sautu, 2012"), ]
# drop unused levels
data_all_raw$Study <- droplevels(data_all_raw$Study)

# summary(data_all_raw$Density_gc_per_L_log10)


data_all <- subset(data_all_raw, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))
data_all <- data_all[which(data_all$Type=="GI" | data_all$Type=="GII"), ]

# update season order
data_all$Season <- factor(data_all$Season,
                          levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)


# update Cont
data_all$Cont = "Euro"
data_all[which(data_all$Location=="USA" | data_all$Location=="USA & Canada"), ]$Cont <- "North America"
data_all[which(data_all$Location=="Singapore" | data_all$Location=="Japan"), ]$Cont <- "Asia"
data_all[which(data_all$Location=="New Zeeland"), ]$Cont <- "New Zeeland"
data_all$Cont <- factor(data_all$Cont,
                        levels = c("North America", "Euro", "New Zeeland", "Asia"), ordered = TRUE)


### prepare the data ##

#######Using all the data#############
all_density <- data_all$Density_gc_per_L_log10

Spring_density <- data_all[which(data_all$Season=="Spring"), ]$Density_gc_per_L_log10
Summer_density <- data_all[which(data_all$Season=="Summer"), ]$Density_gc_per_L_log10
Fall_density <- data_all[which(data_all$Season=="Fall"), ]$Density_gc_per_L_log10
Winter_density <- data_all[which(data_all$Season=="Winter"), ]$Density_gc_per_L_log10

NorthAmerica_density <- data_all[which(data_all$Cont=="North America"), ]$Density_gc_per_L_log10
Euro_density <- data_all[which(data_all$Cont=="Euro"), ]$Density_gc_per_L_log10
Oceania_density <- data_all[which(data_all$Cont=="New Zeeland"), ]$Density_gc_per_L_log10
Asia_density <- data_all[which(data_all$Cont=="Asia"), ]$Density_gc_per_L_log10


##############################
######    bootstrap  #########
##############################

library(boot)

f1 <- function(d, i){
  d2 <- d[i]
  return(mean(d2))
}

f2 <- function(d, i){
  d2 <- d[i]
  return(sd(d2))
}

##########################
###### By Location ######
##########################
### All data ###
all_sample_mean = boot(data= all_density, statistic=f1, R=10000)
all_sample_std = boot(data=all_density, statistic=f2, R=10000)
all_sample_mean_CI <- boot.ci(all_sample_mean)
all_sample_std_CI <- boot.ci(all_sample_std)

NorthAmerica_sample_mean = boot(data=NorthAmerica_density, statistic=f1, R=10000)
NorthAmerica_sample_std = boot(data=NorthAmerica_density, statistic=f2, R=10000)
NorthAmerica_sample_mean_CI <- boot.ci(NorthAmerica_sample_mean)
NorthAmerica_sample_std_CI <- boot.ci(NorthAmerica_sample_std)

Euro_sample_mean = boot(data=Euro_density, statistic=f1, R=10000)
Euro_sample_std = boot(data=Euro_density, statistic=f2, R=10000)
Euro_sample_mean_CI <- boot.ci(Euro_sample_mean)
Euro_sample_std_CI <- boot.ci(Euro_sample_std)

Oceania_sample_mean = boot(data=Oceania_density, statistic=f1, R=10000)
Oceania_sample_std = boot(data=Oceania_density, statistic=f2, R=10000)
Oceania_sample_mean_CI <- boot.ci(Oceania_sample_mean)
Oceania_sample_std_CI <- boot.ci(Oceania_sample_std)

Asia_sample_mean = boot(data=Asia_density, statistic=f1, R=10000)
Asia_sample_std = boot(data=Asia_density, statistic=f2, R=10000)
Asia_sample_mean_CI <- boot.ci(Asia_sample_mean)
Asia_sample_std_CI <- boot.ci(Asia_sample_std)


boot_out_location <- data.frame(NorthAmerica=NorthAmerica_sample_mean$t[,1],
                                Euro=Euro_sample_mean$t[,1],
                                Oceania=Oceania_sample_mean$t[,1],
                                Asia=Asia_sample_mean$t[,1])

boot_out_location_melt <- melt(boot_out_location)


color_code <- c("#000000", "#9B9B9B", "#D4D4D4", "#FFFFFF")


p1 <- ggplot(boot_out_location_melt, aes(value, fill = variable))+
      scale_fill_manual(values = color_code, name="", labels=c("North America", "Europe", "New Zeeland", "Asia"))+
      geom_density(alpha = 0.9)+
      scale_x_continuous(limits = c(2.5, 7.5), breaks=c(2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5))+
      scale_y_continuous(limits = c(0, 8.0))+
      labs(x = "Norovirus Mean Density (log10 gc/L)", y = "Frequency",
      title="Norovirus Mean Density (by Location)")+
      theme_Publication()+
      theme(text = element_text(size=18))


##########################
###### By season ######
##########################

### All data ###
Spring_sample_mean = boot(data=Spring_density, statistic=f1, R=10000)
Spring_sample_std = boot(data=Spring_density, statistic=f2, R=10000)
Spring_sample_mean_CI <- boot.ci(Spring_sample_mean)
Spring_sample_std_CI <- boot.ci(Spring_sample_std)

Summer_sample_mean = boot(data=Summer_density, statistic=f1, R=10000)
Summer_sample_std = boot(data=Summer_density, statistic=f2, R=10000)
Summer_sample_mean_CI <- boot.ci(Summer_sample_mean)
Summer_sample_std_CI <- boot.ci(Summer_sample_std)

Fall_sample_mean = boot(data=Fall_density, statistic=f1, R=10000)
Fall_sample_std = boot(data=Fall_density, statistic=f2, R=10000)
Fall_sample_mean_CI <- boot.ci(Fall_sample_mean)
Fall_sample_std_CI <- boot.ci(Fall_sample_std)

Winter_sample_mean = boot(data=Winter_density, statistic=f1, R=10000)
Winter_sample_std = boot(data=Winter_density, statistic=f2, R=10000)
Winter_sample_mean_CI <- boot.ci(Winter_sample_mean)
Winter_sample_std_CI <- boot.ci(Winter_sample_std)

boot_out_season <- data.frame(Spring_sample=Spring_sample_mean$t[,1],
                              Summer_sample=Summer_sample_mean$t[,1],
                              Fall_sample=Fall_sample_mean$t[,1],
                              Winter_sample=Winter_sample_mean$t[,1])

boot_out_season_melt <- melt(boot_out_season)

color_code <- c("#000000", "#9B9B9B", "#D4D4D4", "#FFFFFF")


p2 <- ggplot(boot_out_season_melt, aes(value, fill = variable)) +
      scale_fill_manual(values = color_code, name="", labels=c("Spring", "Summer", "Fall", "Winter"))+
      geom_density(alpha = 0.9)+
      scale_x_continuous(limits = c(2.5, 7.5), breaks=c(2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5))+
      scale_y_continuous(limits = c(0, 8))+
      labs(x = "Norovirus Mean Density (log10 gc/L)", y = "Frequency",
      title="Norovirus Mean Density (by Season)")+
      theme_Publication()+
      theme(text = element_text(size=18))


grid.arrange(p1, p2, ncol=2)


