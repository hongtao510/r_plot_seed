
library(xlsx)
library(dplyr)

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


# Summary by location
total <- data_all %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )

total_type <- data_all %>% group_by(Type) %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )



location_total <- data_all %>% group_by(Cont) %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )

location_type <- data_all %>% group_by(Cont, Type) %>% 
                 summarise(nobs = length(Density_gc_per_L_log10), 
                           mean= mean(Density_gc_per_L_log10),
                           median = median(Density_gc_per_L_log10),
                           min = min(Density_gc_per_L_log10),
                           max = max(Density_gc_per_L_log10),
                           sd = sd(Density_gc_per_L_log10)
                           )



location_all <- data.frame(nobs_total = c(total$nobs, location_total$nobs),
                           nobs_1 = c(total_type$nobs[1], location_type$nobs[seq(1,dim(location_type)[1],2)]),
                           nobs_2 = c(total_type$nobs[2], location_type$nobs[seq(2,dim(location_type)[1],2)]),
                           mean_total = c(total$mean, location_total$mean),
                           mean_1 = c(total_type$mean[1], location_type$mean[seq(1,dim(location_type)[1],2)]),
                           mean_2 = c(total_type$mean[2], location_type$mean[seq(2,dim(location_type)[1],2)]),
                           median_total = c(total$median, location_total$median),
                           median_1 = c(total_type$median[1], location_type$median[seq(1,dim(location_type)[1],2)]),
                           median_2 = c(total_type$median[2], location_type$median[seq(2,dim(location_type)[1],2)]),
                           min_total = c(total$min, location_total$min),
                           max_total = c(total$max, location_total$max),
                           min_1 = c(total_type$min[1], location_type$min[seq(1,dim(location_type)[1],2)]),
                           max_1 = c(total_type$max[1], location_type$max[seq(1,dim(location_type)[1],2)]),
                           min_2 = c(total_type$min[2], location_type$min[seq(2,dim(location_type)[1],2)]),
                           max_2 = c(total_type$max[2], location_type$max[seq(2,dim(location_type)[1],2)]),
                           sd_total = c(total$sd, location_total$sd),
                           sd_1 = c(total_type$sd[1], location_type$sd[seq(1,dim(location_type)[1],2)]),
                           sd_2 = c(total_type$sd[2], location_type$sd[seq(2,dim(location_type)[1],2)])
                          )



# Summary by season

total_Season <- data_all %>% group_by(Season) %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )



Season_total <- data_all %>% group_by(Season) %>% 
                  summarise(nobs = length(Density_gc_per_L_log10),
                            mean= mean(Density_gc_per_L_log10),
                            median = median(Density_gc_per_L_log10),
                            min = min(Density_gc_per_L_log10),
                            max = max(Density_gc_per_L_log10),
                            sd = sd(Density_gc_per_L_log10)
                            )

Season_type <- data_all %>% group_by(Season, Type) %>% 
                 summarise(nobs = length(Density_gc_per_L_log10), 
                           mean= mean(Density_gc_per_L_log10),
                           median = median(Density_gc_per_L_log10),
                           min = min(Density_gc_per_L_log10),
                           max = max(Density_gc_per_L_log10),
                           sd = sd(Density_gc_per_L_log10)
                           )



Season_all <- data.frame(nobs_total = c(total$nobs, Season_total$nobs),
                           nobs_1 = c(total_Season$nobs[1], Season_type$nobs[seq(1,dim(Season_type)[1],2)]),
                           nobs_2 = c(total_Season$nobs[2], Season_type$nobs[seq(2,dim(Season_type)[1],2)]),
                           mean_total = c(total$mean, Season_total$mean),
                           mean_1 = c(total_Season$mean[1], Season_type$mean[seq(1,dim(Season_type)[1],2)]),
                           mean_2 = c(total_Season$mean[2], Season_type$mean[seq(2,dim(Season_type)[1],2)]),
                           median_total = c(total$median, Season_total$median),
                           median_1 = c(total_Season$median[1], Season_type$median[seq(1,dim(Season_type)[1],2)]),
                           median_2 = c(total_Season$median[2], Season_type$median[seq(2,dim(Season_type)[1],2)]),
                           min_total = c(total$min, Season_total$min),
                           max_total = c(total$max, Season_total$max),
                           min_1 = c(total_Season$min[1], Season_type$min[seq(1,dim(Season_type)[1],2)]),
                           max_1 = c(total_Season$max[1], Season_type$max[seq(1,dim(Season_type)[1],2)]),
                           min_2 = c(total_Season$min[2], Season_type$min[seq(2,dim(Season_type)[1],2)]),
                           max_2 = c(total_Season$max[2], Season_type$max[seq(2,dim(Season_type)[1],2)]),
                           sd_total = c(total$sd, Season_total$sd),
                           sd_1 = c(total_Season$sd[1], Season_type$sd[seq(1,dim(Season_type)[1],2)]),
                           sd_2 = c(total_Season$sd[2], Season_type$sd[seq(2,dim(Season_type)[1],2)])
                          )

write.csv(file="Table2.csv", x=rbind(location_all, Season_all))