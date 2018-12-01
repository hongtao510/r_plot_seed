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


# Part 1 of Stage 1 in Table 4
boot_out_1 <- rbind(
                  c(NorthAmerica_sample_mean_1$t0, NorthAmerica_sample_mean_CI_1$normal[[2]], NorthAmerica_sample_mean_CI_1$normal[[3]],
                    NorthAmerica_sample_std_1$t0, NorthAmerica_sample_std_CI_1$normal[[2]], NorthAmerica_sample_std_CI_1$normal[[3]]),
                  c(Euro_sample_mean_1$t0, Euro_sample_mean_CI_1$normal[[2]], Euro_sample_mean_CI_1$normal[[3]],
                    Euro_sample_std_1$t0, Euro_sample_std_CI_1$normal[[2]], Euro_sample_std_CI_1$normal[[3]]),
                  c(Oceania_sample_mean_1$t0, Oceania_sample_mean_CI_1$normal[[2]], Oceania_sample_mean_CI_1$normal[[3]],
                    Oceania_sample_std_1$t0, Oceania_sample_std_CI_1$normal[[2]], Oceania_sample_std_CI_1$normal[[3]]),
                  c(Asia_sample_mean_1$t0, Asia_sample_mean_CI_1$normal[[2]], Asia_sample_mean_CI_1$normal[[3]],
                    Asia_sample_std_1$t0, Asia_sample_std_CI_1$normal[[2]], Asia_sample_std_CI_1$normal[[3]])
                  )

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

# Part 2 of Stage 1 in Table 4
boot_out_2 <- rbind(boot_out_1,
                  c(NorthAmerica_sample_mean_2$t0, NorthAmerica_sample_mean_CI_2$normal[[2]], NorthAmerica_sample_mean_CI_2$normal[[3]],
                    NorthAmerica_sample_std_2$t0, NorthAmerica_sample_std_CI_2$normal[[2]], NorthAmerica_sample_std_CI_2$normal[[3]]),
                  c(Euro_sample_mean_2$t0, Euro_sample_mean_CI_2$normal[[2]], Euro_sample_mean_CI_2$normal[[3]],
                    Euro_sample_std_2$t0, Euro_sample_std_CI_2$normal[[2]], Euro_sample_std_CI_2$normal[[3]]),
                  c(Oceania_sample_mean_2$t0, Oceania_sample_mean_CI_2$normal[[2]], Oceania_sample_mean_CI_2$normal[[3]],
                    Oceania_sample_std_2$t0, Oceania_sample_std_CI_2$normal[[2]], Oceania_sample_std_CI_2$normal[[3]]),
                  c(Asia_sample_mean_2$t0, Asia_sample_mean_CI_2$normal[[2]], Asia_sample_mean_CI_2$normal[[3]],
                    Asia_sample_std_2$t0, Asia_sample_std_CI_2$normal[[2]], Asia_sample_std_CI_2$normal[[3]])
                  )

#################################################
######  two-sample Kolmogorov-Smirnov test.######
#################################################

###### type I ########

# NorthAmerica VS Euro [E18]
ks.test(NorthAmerica_sample_mean_1$t[,1], Euro_sample_mean_1$t[,1])

# NorthAmerica VS Oceania [E19]
ks.test(NorthAmerica_sample_mean_1$t[,1], Oceania_sample_mean_1$t[,1])

# NorthAmerica VS Asia [E20]
ks.test(NorthAmerica_sample_mean_1$t[,1], Asia_sample_mean_1$t[,1])

# Euro VS Oceania [F19]
ks.test(Euro_sample_mean_1$t[,1], Oceania_sample_mean_1$t[,1])

# Euro VS Asia [F20]
ks.test(Euro_sample_mean_1$t[,1], Asia_sample_mean_1$t[,1])

# Oceania VS Asia [G20]
ks.test(Oceania_sample_mean_1$t[,1], Asia_sample_mean_1$t[,1])


###### type II ########
# NorthAmerica VS Euro [I32]
ks.test(NorthAmerica_sample_mean_2$t[,1], Euro_sample_mean_2$t[,1])

# NorthAmerica VS Oceania [I33]
ks.test(NorthAmerica_sample_mean_2$t[,1], Oceania_sample_mean_2$t[,1])

# NorthAmerica VS Asia [I34]
ks.test(NorthAmerica_sample_mean_2$t[,1], Asia_sample_mean_2$t[,1])

# Euro VS Oceania [J33]
ks.test(Euro_sample_mean_2$t[,1], Oceania_sample_mean_2$t[,1])

# Euro VS Asia [J34]
ks.test(Euro_sample_mean_2$t[,1], Asia_sample_mean_2$t[,1])

# Oceania VS Asia [K34]
ks.test(Oceania_sample_mean_2$t[,1], Asia_sample_mean_2$t[,1])


###### type I VS type II########

##### NorthAmerica I #####

# NorthAmerica I VS NorthAmerica II [E31]
ks.test(NorthAmerica_sample_mean_1$t[,1], NorthAmerica_sample_mean_2$t[,1])

# NorthAmerica I VS Euro II [E32]
ks.test(NorthAmerica_sample_mean_1$t[,1], Euro_sample_mean_2$t[,1])

# NorthAmerica I VS Oceania II [E33]
ks.test(NorthAmerica_sample_mean_1$t[,1], Oceania_sample_mean_2$t[,1])

# NorthAmerica I VS Asia II [E35]
ks.test(NorthAmerica_sample_mean_1$t[,1], Asia_sample_mean_2$t[,1])


##### Euro I  #####

# Euro I VS NorthAmerica II [F31]
ks.test(Euro_sample_mean_1$t[,1], NorthAmerica_sample_mean_2$t[,1])

# Euro I VS Euro II [F32]
ks.test(Euro_sample_mean_1$t[,1], Euro_sample_mean_2$t[,1])

# Euro I VS Oceania II [F33]
ks.test(Euro_sample_mean_1$t[,1], Oceania_sample_mean_2$t[,1])

# Euro I VS Asia II [F34]
ks.test(Euro_sample_mean_1$t[,1], Asia_sample_mean_2$t[,1])


##### Euro I  #####

# Oceania I VS NorthAmerica II [G31]
ks.test(Oceania_sample_mean_1$t[,1], NorthAmerica_sample_mean_2$t[,1])

# Oceania I VS Euro II [G32]
ks.test(Oceania_sample_mean_1$t[,1], Euro_sample_mean_2$t[,1])

# Oceania I VS Oceania II [G33]
ks.test(Oceania_sample_mean_1$t[,1], Oceania_sample_mean_2$t[,1])

# Oceania I VS Asia II [G34]
ks.test(Oceania_sample_mean_1$t[,1], Asia_sample_mean_2$t[,1])



##### Asia I  #####

# Asia I VS NorthAmerica II [H31]
ks.test(Asia_sample_mean_1$t[,1], NorthAmerica_sample_mean_2$t[,1])

# Asia I VS Euro II [H32]
ks.test(Asia_sample_mean_1$t[,1], Euro_sample_mean_2$t[,1])

# Asia I VS Oceania II [H33]
ks.test(Asia_sample_mean_1$t[,1], Oceania_sample_mean_2$t[,1])

# Asia I VS Asia II [H34]
ks.test(Asia_sample_mean_1$t[,1], Asia_sample_mean_2$t[,1])

outpth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/paper/saved_results.xlsx"
write.xlsx(x = boot_out_location_1, file = outpth, sheetName = "boot_out_location_1", row.names = FALSE, append=TRUE)
write.xlsx(x = boot_out_location_2, file = outpth, sheetName = "boot_out_location_2", row.names = FALSE, append=TRUE)



# a_50=rnorm(50)
# b_50=rnorm(50)

# ks.test(a_50, b_50)


# a_500=rnorm(500)
# b_500=rnorm(500)

# ks.test(a_500, b_500)


# a_5000=rnorm(5000)
# b_5000=rnorm(5000)

# ks.test(a_5000, b_5000)


# a_50000=rnorm(50000)
# b_50000=rnorm(50000)

# ks.test(a_50000, b_50000)