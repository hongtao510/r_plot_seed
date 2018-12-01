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

# summary(data_all_raw$Density_gc_per_L_log10)

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



################## Panel A: Check location ############
p1 <- ggplot(data_all, aes(x=Cont, y=Density_gc_per_L_log10)) +
		geom_boxplot(alpha = 0.4)+
		scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
		labs(x = "Sampling Location \n (A)", y = "Norovirus Density (log10 gc/L)",
		    title="Boxplots of Norovirus Densities")+
		theme_Publication()+
		theme(text = element_text(size=18))


################## Panel B: Check season ############
p2 <- ggplot(data_all, aes(x=Season, y=Density_gc_per_L_log10)) +
		geom_boxplot(alpha = 0.4)+
		scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
		labs(x = "Sampling Season \n (B)", y = "Norovirus Density (log10 gc/L)",
		    title="Boxplots of Norovirus Densities")+
		theme_Publication()+
		theme(text = element_text(size=18))




################## Panel C: Check location ############
p3 <- ggplot(data_all, aes(x=Cont, y=Density_gc_per_L_log10, fill = Type)) +
		geom_boxplot(alpha = 0.4)+
		scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
		labs(x = "Sampling Location \n (C)", y = "Norovirus Density (log10 gc/L)",
		    title="Boxplots of Norovirus Densities", fill="")+
		theme_Publication()+
		theme(text = element_text(size=18))+
		scale_fill_grey()



################## Panel D: Check season ############

p4 <- ggplot(data_all, aes(x=Season, y=Density_gc_per_L_log10, fill = Type)) +
		geom_boxplot(alpha = 0.4)+
		scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))+
		labs(x = "Sampling Season \n (D)", y = "Norovirus Density (log10 gc/L)",
		    title="Boxplots of Norovirus Densities", fill="")+
		theme_Publication()+
		theme(text = element_text(size=18))+
		scale_fill_grey()


grid.arrange(p1, p2, p3, p4, ncol=2)


