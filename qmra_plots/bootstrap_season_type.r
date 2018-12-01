library(xlsx)
set.seed(666)


###############################
#####Load the raw data#########
###############################

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
data_all[which(data_all$Location=="New Zeeland"), ]$Cont <- "Oceania"
data_all$Cont <- factor(data_all$Cont,
                        levels = c("North America", "Euro", "Oceania", "Asia"), ordered = TRUE)



data_all_1$Cont = "Euro"
data_all_1[which(data_all_1$Location=="USA" | data_all_1$Location=="USA & Canada"), ]$Cont <- "North America"
data_all_1[which(data_all_1$Location=="Singapore" | data_all_1$Location=="Japan"), ]$Cont <- "Asia"
data_all_1[which(data_all_1$Location=="New Zeeland"), ]$Cont <- "Oceania"
data_all_1$Cont <- factor(data_all_1$Cont,
                        levels = c("North America", "Euro", "Oceania", "Asia"), ordered = TRUE)


data_all_2$Cont = "Euro"
data_all_2[which(data_all_2$Location=="USA" | data_all_2$Location=="USA & Canada"), ]$Cont <- "North America"
data_all_2[which(data_all_2$Location=="Singapore" | data_all_2$Location=="Japan"), ]$Cont <- "Asia"
data_all_2[which(data_all_2$Location=="New Zeeland"), ]$Cont <- "Oceania"
data_all_2$Cont <- factor(data_all_2$Cont,
                        levels = c("North America", "Euro", "Oceania", "Asia"), ordered = TRUE)


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
Oceania_density_1 <- data_all_1[which(data_all_1$Cont=="Oceania"), ]$Density_gc_per_L_log10
Asia_density_1 <- data_all_1[which(data_all_1$Cont=="Asia"), ]$Density_gc_per_L_log10


all_density_2 <- data_all_2$Density_gc_per_L_log10
Spring_density_2 <- data_all_2[which(data_all_2$Season=="Spring"), ]$Density_gc_per_L_log10
Summer_density_2 <- data_all_2[which(data_all_2$Season=="Summer"), ]$Density_gc_per_L_log10
Fall_density_2 <- data_all_2[which(data_all_2$Season=="Fall"), ]$Density_gc_per_L_log10
Winter_density_2 <- data_all_2[which(data_all_2$Season=="Winter"), ]$Density_gc_per_L_log10
NorthAmerica_density_2 <- data_all_2[which(data_all_2$Cont=="North America"), ]$Density_gc_per_L_log10
Euro_density_2 <- data_all_2[which(data_all_2$Cont=="Euro"), ]$Density_gc_per_L_log10
Oceania_density_2 <- data_all_2[which(data_all_2$Cont=="Oceania"), ]$Density_gc_per_L_log10
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

Spring_sample_mean_1 = boot(data=Spring_density_1, statistic=f1, R=10000)
Spring_sample_std_1 = boot(data=Spring_density_1, statistic=f2, R=10000)
Spring_sample_mean_CI_1 <- boot.ci(Spring_sample_mean_1)
Spring_sample_std_CI_1 <- boot.ci(Spring_sample_std_1)
Summer_sample_mean_1 = boot(data=Summer_density_1, statistic=f1, R=10000)
Summer_sample_std_1 = boot(data=Summer_density_1, statistic=f2, R=10000)
Summer_sample_mean_CI_1 <- boot.ci(Summer_sample_mean_1)
Summer_sample_std_CI_1 <- boot.ci(Summer_sample_std_1)
Fall_sample_mean_1 = boot(data=Fall_density_1, statistic=f1, R=10000)
Fall_sample_std_1 = boot(data=Fall_density_1, statistic=f2, R=10000)
Fall_sample_mean_CI_1 <- boot.ci(Fall_sample_mean_1)
Fall_sample_std_CI_1 <- boot.ci(Fall_sample_std_1)
Winter_sample_mean_1 = boot(data=Winter_density_1, statistic=f1, R=10000)
Winter_sample_std_1 = boot(data=Winter_density_1, statistic=f2, R=10000)
Winter_sample_mean_CI_1 <- boot.ci(Winter_sample_mean_1)
Winter_sample_std_CI_1 <- boot.ci(Winter_sample_std_1)

boot_out_season_1 <- data.frame(Spring_sample=Spring_sample_mean_1$t[,1],
                              Summer_sample=Summer_sample_mean_1$t[,1],
                              Fall_sample=Fall_sample_mean_1$t[,1],
                              Winter_sample=Winter_sample_mean_1$t[,1])

# Part 3 of Stage 1 in Table 4
boot_out_1 <- rbind(
                  c(Spring_sample_mean_1$t0, Spring_sample_mean_CI_1$normal[[2]], Spring_sample_mean_CI_1$normal[[3]],
                    Spring_sample_std_1$t0, Spring_sample_std_CI_1$normal[[2]], Spring_sample_std_CI_1$normal[[3]]),
                  c(Summer_sample_mean_1$t0, Summer_sample_mean_CI_1$normal[[2]], Summer_sample_mean_CI_1$normal[[3]],
                    Summer_sample_std_1$t0, Summer_sample_std_CI_1$normal[[2]], Summer_sample_std_CI_1$normal[[3]]),
                  c(Fall_sample_mean_1$t0, Fall_sample_mean_CI_1$normal[[2]], Fall_sample_mean_CI_1$normal[[3]],
                    Fall_sample_std_1$t0, Fall_sample_std_CI_1$normal[[2]], Fall_sample_std_CI_1$normal[[3]]),
                  c(Winter_sample_mean_1$t0, Winter_sample_mean_CI_1$normal[[2]], Winter_sample_mean_CI_1$normal[[3]],
                    Winter_sample_std_1$t0, Winter_sample_std_CI_1$normal[[2]], Winter_sample_std_CI_1$normal[[3]])
                  )



################################
### Type 2 All ###
################################

Spring_sample_mean_2 = boot(data=Spring_density_2, statistic=f1, R=10000)
Spring_sample_std_2 = boot(data=Spring_density_2, statistic=f2, R=10000)
Spring_sample_mean_CI_2 <- boot.ci(Spring_sample_mean_2)
Spring_sample_std_CI_2 <- boot.ci(Spring_sample_std_2)
Summer_sample_mean_2 = boot(data=Summer_density_2, statistic=f1, R=10000)
Summer_sample_std_2 = boot(data=Summer_density_2, statistic=f2, R=10000)
Summer_sample_mean_CI_2 <- boot.ci(Summer_sample_mean_2)
Summer_sample_std_CI_2 <- boot.ci(Summer_sample_std_2)
Fall_sample_mean_2 = boot(data=Fall_density_2, statistic=f1, R=10000)
Fall_sample_std_2 = boot(data=Fall_density_2, statistic=f2, R=10000)
Fall_sample_mean_CI_2 <- boot.ci(Fall_sample_mean_2)
Fall_sample_std_CI_2 <- boot.ci(Fall_sample_std_2)
Winter_sample_mean_2 = boot(data=Winter_density_2, statistic=f1, R=10000)
Winter_sample_std_2 = boot(data=Winter_density_2, statistic=f2, R=10000)
Winter_sample_mean_CI_2 <- boot.ci(Winter_sample_mean_2)
Winter_sample_std_CI_2 <- boot.ci(Winter_sample_std_2)

boot_out_season_2 <- data.frame(Spring_sample=Spring_sample_mean_2$t[,1],
                                Summer_sample=Summer_sample_mean_2$t[,1],
                                Fall_sample=Fall_sample_mean_2$t[,1],
                                Winter_sample=Winter_sample_mean_2$t[,1])

# Part 4 of Stage 1 in Table 4
boot_out_2 <- rbind(boot_out_1,
                  c(Spring_sample_mean_2$t0, Spring_sample_mean_CI_2$normal[[2]], Spring_sample_mean_CI_2$normal[[3]],
                    Spring_sample_std_2$t0, Spring_sample_std_CI_2$normal[[2]], Spring_sample_std_CI_2$normal[[3]]),
                  c(Summer_sample_mean_2$t0, Summer_sample_mean_CI_2$normal[[2]], Summer_sample_mean_CI_2$normal[[3]],
                    Summer_sample_std_2$t0, Summer_sample_std_CI_2$normal[[2]], Summer_sample_std_CI_2$normal[[3]]),
                  c(Fall_sample_mean_2$t0, Fall_sample_mean_CI_2$normal[[2]], Fall_sample_mean_CI_2$normal[[3]],
                    Fall_sample_std_2$t0, Fall_sample_std_CI_2$normal[[2]], Fall_sample_std_CI_2$normal[[3]]),
                  c(Winter_sample_mean_2$t0, Winter_sample_mean_CI_2$normal[[2]], Winter_sample_mean_CI_2$normal[[3]],
                    Winter_sample_std_2$t0, Winter_sample_std_CI_2$normal[[2]], Winter_sample_std_CI_2$normal[[3]])
                  )



#################################################
######  two-sample Kolmogorov-Smirnov test.######
#################################################

###### type I ########

# Spring I VS Summer I [E25]
ks.test(Spring_sample_mean_1$t[,1], Summer_sample_mean_1$t[,1])

# Spring I VS Fall I [E26]
ks.test(Spring_sample_mean_1$t[,1], Fall_sample_mean_1$t[,1])

# Spring I VS Winter I [E27]
ks.test(Spring_sample_mean_1$t[,1], Winter_sample_mean_1$t[,1])

# Summer I VS Fall I [F26]
ks.test(Summer_sample_mean_1$t[,1], Fall_sample_mean_1$t[,1])

# Summer I VS Winter I [F27]
ks.test(Summer_sample_mean_1$t[,1], Winter_sample_mean_1$t[,1])

# Fall I VS Winter I [G27]
ks.test(Fall_sample_mean_1$t[,1], Winter_sample_mean_1$t[,1])


###### type II ########

# Spring II VS Summer II [I39]
ks.test(Spring_sample_mean_2$t[,1], Summer_sample_mean_2$t[,1])

# Spring II VS Fall II [I40]
ks.test(Spring_sample_mean_2$t[,1], Fall_sample_mean_2$t[,1])

# Spring II VS Winter II [I41]
ks.test(Spring_sample_mean_2$t[,1], Winter_sample_mean_2$t[,1])

# Summer II VS Fall II [J40]
ks.test(Summer_sample_mean_2$t[,1], Fall_sample_mean_2$t[,1])

# Summer II VS Winter II [J41]
ks.test(Summer_sample_mean_2$t[,1], Winter_sample_mean_2$t[,1])

# Fall II VS Winter II [K41]
ks.test(Fall_sample_mean_2$t[,1], Winter_sample_mean_2$t[,1])


###### type I VS type II########

#####  Spring I ######

# Spring I VS Spring II [E38]
ks.test(Spring_sample_mean_1$t[,1], Spring_sample_mean_2$t[,1])

# Spring I VS Summer II [E39]
ks.test(Spring_sample_mean_1$t[,1], Summer_sample_mean_2$t[,1])

# Spring I VS Fall II [E40]
ks.test(Spring_sample_mean_1$t[,1], Fall_sample_mean_2$t[,1])

# Spring I VS Winter II [E41]
ks.test(Spring_sample_mean_1$t[,1], Winter_sample_mean_2$t[,1])


#####  Summer I ######

# Summer I VS Spring II [F38]
ks.test(Summer_sample_mean_1$t[,1], Spring_sample_mean_2$t[,1])

# Summer I VS Summer II [F39]
ks.test(Summer_sample_mean_1$t[,1], Summer_sample_mean_2$t[,1])

# Summer I VS Fall II [F40]
ks.test(Summer_sample_mean_1$t[,1], Fall_sample_mean_2$t[,1])

# Summer I VS Winter II [F41]
ks.test(Summer_sample_mean_1$t[,1], Winter_sample_mean_2$t[,1])



#####  Fall I ######

# Fall I VS Spring II [G38]
ks.test(Fall_sample_mean_1$t[,1], Spring_sample_mean_2$t[,1])

# Fall I VS Summer II [G39]
ks.test(Fall_sample_mean_1$t[,1], Summer_sample_mean_2$t[,1])

# Fall I VS Fall II [G40]
ks.test(Fall_sample_mean_1$t[,1], Fall_sample_mean_2$t[,1])

# Fall I VS Winter II [G41]
ks.test(Fall_sample_mean_1$t[,1], Winter_sample_mean_2$t[,1])


#####  Winter I ######

# Winter I VS Spring II [H38]
ks.test(Winter_sample_mean_1$t[,1], Spring_sample_mean_2$t[,1])

# Winter I VS Summer II [H39]
ks.test(Winter_sample_mean_1$t[,1], Summer_sample_mean_2$t[,1])

# Winter I VS Fall II [H40]
ks.test(Winter_sample_mean_1$t[,1], Fall_sample_mean_2$t[,1])

# Winter I VS Winter II [H41]
ks.test(Winter_sample_mean_1$t[,1], Winter_sample_mean_2$t[,1])


outpth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/paper/saved_results.xlsx"
write.xlsx(x = boot_out_season_1, file = outpth, sheetName = "boot_out_season_1", row.names = FALSE, append=TRUE)

write.xlsx(x = boot_out_season_2, file = outpth, sheetName = "boot_out_season_2", row.names = FALSE, append=TRUE)

