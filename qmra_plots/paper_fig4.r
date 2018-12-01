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


###############################
#####Load the raw data#########
###############################
set.seed(666)
pth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/Version_Feb_22_2016"

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

data_all_raw_1 <- data_all_raw[which(data_all_raw$Type=="GI"),]
data_all_raw_2 <- data_all_raw[which(data_all_raw$Type=="GII"),]
data_all_1 <- subset(data_all_raw_1, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))
data_all_2 <- subset(data_all_raw_2, setect=c("Study", "Study_ID", "Location", "Season", "Density_gc_per_L_log10"))


# update season order
data_all_1$Season <- factor(data_all_1$Season,
                          levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)

data_all_2$Season <- factor(data_all_2$Season,
                          levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)

# update Cont
data_all$Cont = "Euro"
data_all[which(data_all$Location=="USA" | data_all$Location=="USA & Canada"), ]$Cont <- "North America"
data_all[which(data_all$Location=="Singapore" | data_all$Location=="Japan"), ]$Cont <- "Asia"
data_all[which(data_all$Location=="New Zeeland"), ]$Cont <- "New Zeeland"
data_all$Cont <- factor(data_all$Cont,
                        levels = c("North America", "Euro", "New Zeeland", "Asia"), ordered = TRUE)



data_all_1$Cont = "Euro"
data_all_1[which(data_all_1$Location=="USA" | data_all_1$Location=="USA & Canada"), ]$Cont <- "North America"
data_all_1[which(data_all_1$Location=="Singapore" | data_all_1$Location=="Japan"), ]$Cont <- "Asia"
data_all_1[which(data_all_1$Location=="New Zeeland"), ]$Cont <- "New Zeeland"
data_all_1$Cont <- factor(data_all_1$Cont,
                        levels = c("North America", "Euro", "New Zeeland", "Asia"), ordered = TRUE)


data_all_2$Cont = "Euro"
data_all_2[which(data_all_2$Location=="USA" | data_all_2$Location=="USA & Canada"), ]$Cont <- "North America"
data_all_2[which(data_all_2$Location=="Singapore" | data_all_2$Location=="Japan"), ]$Cont <- "Asia"
data_all_2[which(data_all_2$Location=="New Zeeland"), ]$Cont <- "New Zeeland"
data_all_2$Cont <- factor(data_all_2$Cont,
                        levels = c("North America", "Euro", "New Zeeland", "Asia"), ordered = TRUE)


##########################
### prepare the data #####
##########################
all_density_1 <- data_all_1$Density_gc_per_L_log10
Spring_density_1 <- data_all_1[which(data_all_1$Season=="Spring"), ]$Density_gc_per_L_log10
Summer_density_1 <- data_all_1[which(data_all_1$Season=="Summer"), ]$Density_gc_per_L_log10
Fall_density_1 <- data_all_1[which(data_all_1$Season=="Fall"), ]$Density_gc_per_L_log10
Winter_density_1 <- data_all_1[which(data_all_1$Season=="Winter"), ]$Density_gc_per_L_log10
NorthAmerica_density_1 <- data_all_1[which(data_all_1$Cont=="North America"), ]$Density_gc_per_L_log10
Euro_density_1 <- data_all_1[which(data_all_1$Cont=="Euro"), ]$Density_gc_per_L_log10
Oceania_density_1 <- data_all_1[which(data_all_1$Cont=="New Zeeland"), ]$Density_gc_per_L_log10
Asia_density_1 <- data_all_1[which(data_all_1$Cont=="Asia"), ]$Density_gc_per_L_log10


all_density_2 <- data_all_2$Density_gc_per_L_log10
Spring_density_2 <- data_all_2[which(data_all_2$Season=="Spring"), ]$Density_gc_per_L_log10
Summer_density_2 <- data_all_2[which(data_all_2$Season=="Summer"), ]$Density_gc_per_L_log10
Fall_density_2 <- data_all_2[which(data_all_2$Season=="Fall"), ]$Density_gc_per_L_log10
Winter_density_2 <- data_all_2[which(data_all_2$Season=="Winter"), ]$Density_gc_per_L_log10
NorthAmerica_density_2 <- data_all_2[which(data_all_2$Cont=="North America"), ]$Density_gc_per_L_log10
Euro_density_2 <- data_all_2[which(data_all_2$Cont=="Euro"), ]$Density_gc_per_L_log10
Oceania_density_2 <- data_all_2[which(data_all_2$Cont=="New Zeeland"), ]$Density_gc_per_L_log10
Asia_density_2 <- data_all_2[which(data_all_2$Cont=="Asia"), ]$Density_gc_per_L_log10


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



#######################
###### By season ######
#######################

################################
### Type 1
################################
all_sample_mean_1 = boot(data=all_density_1, statistic=f1, R=10000)
all_sample_std_1 = boot(data=all_density_1, statistic=f2, R=10000)
all_sample_mean_CI_1 <- boot.ci(all_sample_mean_1)
all_sample_std_CI_1 <- boot.ci(all_sample_std_1)
NorthAmerica_sample_mean_1 = boot(data=NorthAmerica_density_1, statistic=f1, R=10000)
NorthAmerica_sample_std_1 = boot(data=NorthAmerica_density_1, statistic=f2, R=10000)
NorthAmerica_sample_mean_CI_1 <- boot.ci(NorthAmerica_sample_mean_1)
NorthAmerica_sample_std_CI_1 <- boot.ci(NorthAmerica_sample_std_1)
Euro_sample_mean_1 = boot(data=Euro_density_1, statistic=f1, R=10000)
Euro_sample_std_1 = boot(data=Euro_density_1, statistic=f2, R=10000)
Euro_sample_mean_CI_1 <- boot.ci(Euro_sample_mean_1)
Euro_sample_std_CI_1 <- boot.ci(Euro_sample_std_1)
Oceania_sample_mean_1 = boot(data=Oceania_density_1, statistic=f1, R=10000)
Oceania_sample_std_1 = boot(data=Oceania_density_1, statistic=f2, R=10000)
Oceania_sample_mean_CI_1 <- boot.ci(Oceania_sample_mean_1)
Oceania_sample_std_CI_1 <- boot.ci(Oceania_sample_std_1)
Asia_sample_mean_1 = boot(data=Asia_density_1, statistic=f1, R=10000)
Asia_sample_std_1 = boot(data=Asia_density_1, statistic=f2, R=10000)
Asia_sample_mean_CI_1 <- boot.ci(Asia_sample_mean_1)
Asia_sample_std_CI_1 <- boot.ci(Asia_sample_std_1)

boot_out_location_1 <- data.frame(NorthAmerica=NorthAmerica_sample_mean_1$t[,1],
                                  Euro=Euro_sample_mean_1$t[,1],
                                  Oceania=Oceania_sample_mean_1$t[,1],
                                  Asia=Asia_sample_mean_1$t[,1])

boot_out_location_melt_1 <- melt(boot_out_location_1)



color_code <- c("#000000", "#9B9B9B", "#D4D4D4", "#FFFFFF")

p1 <- ggplot(boot_out_location_melt_1, aes(value, fill = variable)) +
            scale_fill_manual(values = color_code, name="", labels=c("North America", "Europe", "New Zeeland", "Asia"))+
            geom_density(alpha = 0.9)+
            scale_x_continuous(limits = c(2, 7.5), breaks=c(2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5))+
            scale_y_continuous(limits = c(0, 8.0))+
            labs(x = "Norovirus Mean Density (log10 gc/L)", y = "Frequency",
            title="Norovirus Mean Density (GI)")+
            theme_Publication()+
            theme(text = element_text(size=18))


################################
### Type 2
################################
all_sample_mean_2 = boot(data=all_density_2, statistic=f1, R=10000)
all_sample_std_2 = boot(data=all_density_2, statistic=f2, R=10000)
all_sample_mean_CI_2 <- boot.ci(all_sample_mean_2)
all_sample_std_CI_2 <- boot.ci(all_sample_std_2)
NorthAmerica_sample_mean_2 = boot(data=NorthAmerica_density_2, statistic=f1, R=10000)
NorthAmerica_sample_std_2 = boot(data=NorthAmerica_density_2, statistic=f2, R=10000)
NorthAmerica_sample_mean_CI_2 <- boot.ci(NorthAmerica_sample_mean_2)
NorthAmerica_sample_std_CI_2 <- boot.ci(NorthAmerica_sample_std_2)
Euro_sample_mean_2 = boot(data=Euro_density_2, statistic=f1, R=10000)
Euro_sample_std_2 = boot(data=Euro_density_2, statistic=f2, R=10000)
Euro_sample_mean_CI_2 <- boot.ci(Euro_sample_mean_2)
Euro_sample_std_CI_2 <- boot.ci(Euro_sample_std_2)
Oceania_sample_mean_2 = boot(data=Oceania_density_2, statistic=f1, R=10000)
Oceania_sample_std_2 = boot(data=Oceania_density_2, statistic=f2, R=10000)
Oceania_sample_mean_CI_2 <- boot.ci(Oceania_sample_mean_2)
Oceania_sample_std_CI_2 <- boot.ci(Oceania_sample_std_2)
Asia_sample_mean_2 = boot(data=Asia_density_2, statistic=f1, R=10000)
Asia_sample_std_2 = boot(data=Asia_density_2, statistic=f2, R=10000)
Asia_sample_mean_CI_2 <- boot.ci(Asia_sample_mean_2)
Asia_sample_std_CI_2 <- boot.ci(Asia_sample_std_2)

boot_out_location_2 <- data.frame(NorthAmerica=NorthAmerica_sample_mean_2$t[,1],
                                  Euro=Euro_sample_mean_2$t[,1],
                                  Oceania=Oceania_sample_mean_2$t[,1],
                                  Asia=Asia_sample_mean_2$t[,1])

boot_out_location_melt_2 <- melt(boot_out_location_2)


color_code <- c("#000000", "#9B9B9B", "#D4D4D4", "#FFFFFF")


p2 <- ggplot(boot_out_location_melt_2, aes(value, fill = variable)) +
            scale_fill_manual(values = color_code, name="", labels=c("North America", "Europe", "New Zeeland", "Asia"))+
            geom_density(alpha = 0.9)+
            scale_x_continuous(limits = c(2, 7.5), breaks=c(2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5))+
            scale_y_continuous(limits = c(0, 8.0))+
            labs(x = "Norovirus Mean Density (log10 gc/L)", y = "Frequency",
            title="Norovirus Mean Density (GII)")+
            theme_Publication()+
            theme(text = element_text(size=18))

grid.arrange(p1, p2, ncol=2)