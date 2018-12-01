library(xlsx)
set.seed(666)


#####Load the raw data#################
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
data_all[which(data_all$Location=="New Zeeland"), ]$Cont <- "Oceania"
data_all$Cont <- factor(data_all$Cont,
                        levels = c("North America", "Euro", "Oceania", "Asia"), ordered = TRUE)


### prepare the data ##

#######Using all the data#############
all_density <- data_all$Density_gc_per_L_log10

Spring_density <- data_all[which(data_all$Season=="Spring"), ]$Density_gc_per_L_log10
Summer_density <- data_all[which(data_all$Season=="Summer"), ]$Density_gc_per_L_log10
Fall_density <- data_all[which(data_all$Season=="Fall"), ]$Density_gc_per_L_log10
Winter_density <- data_all[which(data_all$Season=="Winter"), ]$Density_gc_per_L_log10

NorthAmerica_density <- data_all[which(data_all$Cont=="North America"), ]$Density_gc_per_L_log10
Euro_density <- data_all[which(data_all$Cont=="Euro"), ]$Density_gc_per_L_log10
Oceania_density <- data_all[which(data_all$Cont=="Oceania"), ]$Density_gc_per_L_log10
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

# Part 1 of Stage 1 in Table 3
boot_out_location <- rbind(
                  c(NorthAmerica_sample_mean$t0, NorthAmerica_sample_mean_CI$normal[[2]], NorthAmerica_sample_mean_CI$normal[[3]],
                    NorthAmerica_sample_std$t0, NorthAmerica_sample_std_CI$normal[[2]], NorthAmerica_sample_std_CI$normal[[3]]),
                  c(Euro_sample_mean$t0, Euro_sample_mean_CI$normal[[2]], Euro_sample_mean_CI$normal[[3]],
                    Euro_sample_std$t0, Euro_sample_std_CI$normal[[2]], Euro_sample_std_CI$normal[[3]]),
                  c(Oceania_sample_mean$t0, Oceania_sample_mean_CI$normal[[2]], Oceania_sample_mean_CI$normal[[3]],
                    Oceania_sample_std$t0, Oceania_sample_std_CI$normal[[2]], Oceania_sample_std_CI$normal[[3]]),
                  c(Asia_sample_mean$t0, Asia_sample_mean_CI$normal[[2]], Asia_sample_mean_CI$normal[[3]],
                    Asia_sample_std$t0, Asia_sample_std_CI$normal[[2]], Asia_sample_std_CI$normal[[3]])
                  )


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

# Part 2 of Stage 1 in Table 3
boot_out_season <- rbind(boot_out_location,
                  c(Spring_sample_mean$t0, Spring_sample_mean_CI$normal[[2]], Spring_sample_mean_CI$normal[[3]],
                    Spring_sample_std$t0, Spring_sample_std_CI$normal[[2]], Spring_sample_std_CI$normal[[3]]),
                  c(Summer_sample_mean$t0, Summer_sample_mean_CI$normal[[2]], Summer_sample_mean_CI$normal[[3]],
                    Summer_sample_std$t0, Summer_sample_std_CI$normal[[2]], Summer_sample_std_CI$normal[[3]]),
                  c(Fall_sample_mean$t0, Fall_sample_mean_CI$normal[[2]], Fall_sample_mean_CI$normal[[3]],
                    Fall_sample_std$t0, Fall_sample_std_CI$normal[[2]], Fall_sample_std_CI$normal[[3]]),
                  c(Winter_sample_mean$t0, Winter_sample_mean_CI$normal[[2]], Winter_sample_mean_CI$normal[[3]],
                    Winter_sample_std$t0, Winter_sample_std_CI$normal[[2]], Winter_sample_std_CI$normal[[3]])
                  )


#################################################
######  two-sample Kolmogorov-Smirnov test.######
#################################################

###### location ########

# NorthAmerica VS Euro [E5]
ks.test(NorthAmerica_sample_mean$t[,1], Euro_sample_mean$t[,1])

# NorthAmerica VS Oceania [E6]
ks.test(NorthAmerica_sample_mean$t[,1], Oceania_sample_mean$t[,1])

# NorthAmerica VS Asia [E7]
ks.test(NorthAmerica_sample_mean$t[,1], Asia_sample_mean$t[,1])

# Euro VS Oceania [F6]
ks.test(Euro_sample_mean$t[,1], Oceania_sample_mean$t[,1])

# Euro VS Asia [F7]
ks.test(Euro_sample_mean$t[,1], Asia_sample_mean$t[,1])

# Oceania VS Asia [G7]
ks.test(Oceania_sample_mean$t[,1], Asia_sample_mean$t[,1])



###### season ########

# Spring VS Summer [E11]
ks.test(Spring_sample_mean$t[,1], Summer_sample_mean$t[,1])

# Spring VS Fall [E12]
ks.test(Spring_sample_mean$t[,1], Fall_sample_mean$t[,1])

# Spring VS Winter [E13]
ks.test(Spring_sample_mean$t[,1], Winter_sample_mean$t[,1])

# Summer VS Fall [F12]
ks.test(Summer_sample_mean$t[,1], Fall_sample_mean$t[,1])

# Summer VS Winter [F13]
ks.test(Summer_sample_mean$t[,1], Winter_sample_mean$t[,1])

# Fall VS Winter [G13]
ks.test(Fall_sample_mean$t[,1], Winter_sample_mean$t[,1])



boot_out_location <- data.frame(NorthAmerica=NorthAmerica_sample_mean$t[,1],
                                Euro=Euro_sample_mean$t[,1],
                                Oceania=Oceania_sample_mean$t[,1],
                                Asia=Asia_sample_mean$t[,1])



boot_out_season <- data.frame(Spring_sample=Spring_sample_mean$t[,1],
                              Summer_sample=Summer_sample_mean$t[,1],
                              Fall_sample=Fall_sample_mean$t[,1],
                              Winter_sample=Winter_sample_mean$t[,1])

outpth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/paper/saved_results.xlsx"
write.xlsx(x = boot_out_location, file = outpth, sheetName = "location", row.names = FALSE)

write.xlsx(x = boot_out_location, file = outpth, sheetName = "season", row.names = FALSE, append=TRUE)
