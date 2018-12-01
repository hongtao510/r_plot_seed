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


#######################################
######## Stratified sampling ##########
#######################################
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")


data_all_strat <- data_all[which(data_all$Cont!="N/A"), ]
strat_location_100 <- c()
strat_location_500 <- c()
strat_location_1000 <- c()
strat_location_3000 <- c()


#######################
### By location all ###
#######################

for (i in 1:100){
  strat_temp <- stratified(data_all_strat, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_100 <- rbind(strat_location_100, strat_temp)
}
row.names(strat_location_100) <- NULL
mean_row_100 <- apply(strat_location_100, 1, mean)
std_row_100 <- apply(strat_location_100, 1, sd)


for (i in 1:500){
  strat_temp <- stratified(data_all_strat, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_500 <- rbind(strat_location_500, strat_temp)
}
row.names(strat_location_500) <- NULL
mean_row_500 <- apply(strat_location_500, 1, mean)
std_row_500 <- apply(strat_location_500, 1, sd)

for (i in 1:1000){
  strat_temp <- stratified(data_all_strat, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_1000 <- rbind(strat_location_1000, strat_temp)
}
row.names(strat_location_1000) <- NULL
mean_row_1000 <- apply(strat_location_1000, 1, mean)
std_row_1000 <- apply(strat_location_1000, 1, sd)

for (i in 1:3000){
  strat_temp <- stratified(data_all_strat, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000 <- rbind(strat_location_3000, strat_temp)
}
row.names(strat_location_3000) <- NULL
mean_row_3000 <- apply(strat_location_3000, 1, mean)
std_row_3000 <- apply(strat_location_3000, 1, sd)

strat_location_pool <- data.frame(Mean=c(mean_row_100, mean_row_500, mean_row_1000,
                                    mean_row_3000))


strat_location_pool$iter <- c(rep("100", 100), rep("500", 500), rep("1000", 1000),
                             rep("3000", 3000))
strat_location_pool_melt <- melt(strat_location_pool)

strat_location_pool_melt$iter <- factor(strat_location_pool_melt$iter, levels=c("100", "500", "1000", "3000"), ordered = TRUE)

color_code <- c("#FFFFFF", "#D4D4D4", "#9B9B9B", "#000000")

p1 <- ggplot(strat_location_pool_melt, aes(value, fill = iter)) +
scale_fill_manual(values = color_code, name="Iteration", labels=c("100", "500", "1000", "3000"))+
geom_density(alpha = 0.9)+
scale_x_continuous(limits = c(3, 6), breaks=c(3,3.5,4,4.5,5,5.5,6))+
scale_y_continuous(limits = c(0, 2.5))+
labs(x = "Norovirus Mean Density (log10 gc/L) \n (A)", y = "Frequency",
title="Stratified by Location (Combined types)")+
theme_Publication()+
theme(text = element_text(size=18))


##################
### Type 1 all ###
##################

data_all_strat_1 <- data_all_1[which(data_all_1$Cont!="N/A"), ]
strat_location_100_1 <- c()
strat_location_500_1 <- c()
strat_location_1000_1 <- c()
strat_location_3000_1 <- c()

for (i in 1:100){
  strat_temp_1 <- stratified(data_all_strat_1, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_100_1 <- rbind(strat_location_100_1, strat_temp_1)
}
row.names(strat_location_100_1) <- NULL
mean_row_100_1 <- apply(strat_location_100_1, 1, mean)
std_row_100_1 <- apply(strat_location_100_1, 1, sd)


for (i in 1:500){
  strat_temp_1 <- stratified(data_all_strat_1, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_500_1 <- rbind(strat_location_500_1, strat_temp_1)
}
row.names(strat_location_500_1) <- NULL
mean_row_500_1 <- apply(strat_location_500_1, 1, mean)
std_row_500_1 <- apply(strat_location_500_1, 1, sd)

for (i in 1:1000){
  strat_temp_1 <- stratified(data_all_strat_1, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_1000_1 <- rbind(strat_location_1000_1, strat_temp_1)
}
row.names(strat_location_1000_1) <- NULL
mean_row_1000_1 <- apply(strat_location_1000_1, 1, mean)
std_row_1000_1 <- apply(strat_location_1000_1, 1, sd)

for (i in 1:3000){
  strat_temp_1 <- stratified(data_all_strat_1, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000_1 <- rbind(strat_location_3000_1, strat_temp_1)
}
row.names(strat_location_3000_1) <- NULL
mean_row_3000_1 <- apply(strat_location_3000_1, 1, mean)
std_row_3000_1 <- apply(strat_location_3000_1, 1, sd)

strat_location_pool_1 <- data.frame(Mean=c(mean_row_100_1, mean_row_500_1, mean_row_1000_1,
                                           mean_row_3000_1))

strat_location_pool_1$iter <- c(rep("100", 100), rep("500", 500), rep("1000", 1000),
                                rep("3000", 3000))

strat_location_pool_melt_1 <- melt(strat_location_pool_1)

color_code <- c("#FFFFFF", "#D4D4D4", "#9B9B9B", "#000000")

p2 <- ggplot(strat_location_pool_melt_1, aes(value, fill = iter)) +
scale_fill_manual(values = color_code, name="Iteration", labels=c("100", "500", "1000", "3000"))+
geom_density(alpha = 0.9)+
scale_x_continuous(limits = c(3.0, 6.0), breaks=seq(3.0, 6.0, by=0.5))+
scale_y_continuous(limits = c(0, 2.5))+
labs(x = "Norovirus Mean Density (log10 gc/L) \n (B)", y = "Frequency",
title="Stratified by Location (GI)")+
theme_Publication()+
theme(text = element_text(size=18))


##############
### Type 2 ###
##############

data_all_strat_2 <- data_all_2
strat_location_100_2 <- c()
strat_location_500_2 <- c()
strat_location_1000_2 <- c()
strat_location_3000_2 <- c()

for (i in 1:100){
  strat_temp_2 <- stratified(data_all_strat_2, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_100_2 <- rbind(strat_location_100_2, strat_temp_2)
}
row.names(strat_location_100_2) <- NULL
mean_row_100_2 <- apply(strat_location_100_2, 1, mean)
std_row_100_2 <- apply(strat_location_100_2, 1, sd)

for (i in 1:500){
  strat_temp_2 <- stratified(data_all_strat_2, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_500_2 <- rbind(strat_location_500_2, strat_temp_2)
}
row.names(strat_location_500_2) <- NULL
mean_row_500_2 <- apply(strat_location_500_2, 1, mean)
std_row_500_2 <- apply(strat_location_500_2, 1, sd)

for (i in 1:1000){
  strat_temp_2 <- stratified(data_all_strat_2, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_1000_2 <- rbind(strat_location_1000_2, strat_temp_2)
}
row.names(strat_location_1000_2) <- NULL
mean_row_1000_2 <- apply(strat_location_1000_2, 1, mean)
std_row_1000_2 <- apply(strat_location_1000_2, 1, sd)

for (i in 1:3000){
  strat_temp_2 <- stratified(data_all_strat_2, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000_2 <- rbind(strat_location_3000_2, strat_temp_2)
}
row.names(strat_location_3000_2) <- NULL
mean_row_3000_2 <- apply(strat_location_3000_2, 1, mean)
std_row_3000_2 <- apply(strat_location_3000_2, 1, sd)

strat_location_pool_2 <- data.frame(Mean=c(mean_row_100_2, mean_row_500_2, mean_row_1000_2,
                                    mean_row_3000_2))

strat_location_pool_2$iter <- c(rep("100", 100), rep("500", 500), rep("1000", 1000),
                                rep("3000", 3000))

strat_location_pool_melt_2 <- melt(strat_location_pool_2)

color_code <- c("#FFFFFF", "#D4D4D4", "#9B9B9B", "#000000")

p3 <- ggplot(strat_location_pool_melt_2, aes(value, fill = iter)) +
scale_fill_manual(values = color_code, name="Iteration", labels=c("100", "500", "1000", "3000"))+
geom_density(alpha = 0.9)+
scale_x_continuous(limits = c(3.0, 6.0), breaks=seq(3.0, 6.0, by=0.5))+
scale_y_continuous(limits = c(0, 2.5))+
labs(x = "Norovirus Mean Density (log10 gc/L) \n (C)", y = "Frequency",
title="Stratified by Location (GII)")+
theme_Publication()+
theme(text = element_text(size=18))


grid.arrange(p1, p2, p3, ncol=3)


