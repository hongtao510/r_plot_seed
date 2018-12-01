
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



#######################################
######## Stratified sampling ##########
#######################################
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")



##########################
##### By season all ######
##########################

data_all_strat <- data_all[which(data_all$Cont!="N/A"), ]
strat_season_3000 <- c()


for (i in 1:3000){
  strat_temp <- stratified(data_all_strat, "Season", size = c(10, 10, 10, 10), replace=TRUE)$Density_gc_per_L_log10
  strat_season_3000 <- rbind(strat_season_3000, strat_temp)
}
row.names(strat_season_3000) <- NULL
mean_row_season_3000 <- apply(strat_season_3000, 1, mean)
std_row_season_3000 <- apply(strat_season_3000, 1, sd)



### Type 1  ###

data_all_strat_1 <- data_all_1[which(data_all_1$Cont!="N/A"), ]
strat_season_3000_1 <- c()

for (i in 1:3000){
  strat_temp_1 <- stratified(data_all_strat_1, "Season", size = c(10,10,10,10), replace=TRUE)$Density_gc_per_L_log10
  strat_season_3000_1 <- rbind(strat_season_3000_1, strat_temp_1)
}
row.names(strat_season_3000_1) <- NULL
mean_row_season_3000_1 <- apply(strat_season_3000_1, 1, mean)
std_row_season_3000_1 <- apply(strat_season_3000_1, 1, sd)



### Type 2     ###

data_all_strat_2 <- data_all_2
strat_season_3000_2 <- c()

for (i in 1:3000){
  strat_temp_2 <- stratified(data_all_strat_2, "Season", size = c(10,10,10,10), replace=TRUE)$Density_gc_per_L_log10
  strat_season_3000_2 <- rbind(strat_season_3000_2, strat_temp_2)
}
row.names(strat_season_3000_2) <- NULL
mean_row_season_3000_2 <- apply(strat_season_3000_2, 1, mean)
std_row_season_3000_2 <- apply(strat_season_3000_2, 1, sd)

strat_season_pool <- data.frame(Combined=mean_row_season_3000,
                                  Type_1=mean_row_season_3000_1,
                                  Type_2=mean_row_season_3000_2)


#  Stage 2 in Tables 3 and 4 [season]
out_sum_season <- data.frame(rbind(
  c(mean(mean_row_season_3000),       quantile(mean_row_season_3000,c(0.05))[[1]],        quantile(mean_row_season_3000,c(0.95))[[1]],
    mean(std_row_season_3000), quantile(std_row_season_3000,c(0.05))[[1]],  quantile(std_row_season_3000,c(0.95))[[1]]),
  c(mean(mean_row_season_3000_1),     quantile(mean_row_season_3000_1,c(0.05))[[1]],      quantile(mean_row_season_3000_1,c(0.95))[[1]],
    mean(std_row_season_3000_1),      quantile(std_row_season_3000_1,c(0.05))[[1]],       quantile(std_row_season_3000_1,c(0.95))[[1]]),
  c(mean(mean_row_season_3000_2),     quantile(mean_row_season_3000_2,c(0.05))[[1]],      quantile(mean_row_season_3000_2,c(0.95))[[1]],
    mean(std_row_season_3000_2),      quantile(std_row_season_3000_2,c(0.05))[[1]],       quantile(std_row_season_3000_2,c(0.95))[[1]])
  )
)




###########################################
### Stratified sampling by location all ###
###########################################

data_all_strat <- data_all
strat_location_3000 <- c()

for (i in 1:3000){
  strat_temp <- stratified(data_all_strat, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000 <- rbind(strat_location_3000, strat_temp)
}
row.names(strat_location_3000) <- NULL
mean_row_3000 <- apply(strat_location_3000, 1, mean)
std_row_3000 <- apply(strat_location_3000, 1, sd)



### Type 1  ###

data_all_strat_1 <- data_all_1
strat_location_3000_1 <- c()

for (i in 1:3000){
  strat_temp_1 <- stratified(data_all_strat_1, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000_1 <- rbind(strat_location_3000_1, strat_temp_1)
}
row.names(strat_location_3000_1) <- NULL
mean_row_3000_1 <- apply(strat_location_3000_1, 1, mean)
std_row_3000_1 <- apply(strat_location_3000_1, 1, sd)



### Type 2 ###

data_all_strat_2 <- data_all_2
strat_location_3000_2 <- c()

for (i in 1:3000){
  strat_temp_2 <- stratified(data_all_strat_2, "Cont", size = c(10, 10, 5, 5), replace=TRUE)$Density_gc_per_L_log10
  strat_location_3000_2 <- rbind(strat_location_3000_2, strat_temp_2)
}
row.names(strat_location_3000_2) <- NULL
mean_row_3000_2 <- apply(strat_location_3000_2, 1, mean)
std_row_3000_2 <- apply(strat_location_3000_2, 1, sd)


strat_location_pool <- data.frame(Combined=mean_row_3000,
                                   Type_1=mean_row_3000_1,
                                   Type_2=mean_row_3000_2)

#  Stage 2 in Tables 3 and 4 [location]
out_sum <- data.frame(rbind(
  c(mean(mean_row_3000),       quantile(mean_row_3000,c(0.05))[[1]],        quantile(mean_row_3000,c(0.95))[[1]],
    mean(std_row_3000), quantile(std_row_3000,c(0.05))[[1]],  quantile(std_row_3000,c(0.95))[[1]]),
  c(mean(mean_row_3000_1),     quantile(mean_row_3000_1,c(0.05))[[1]],      quantile(mean_row_3000_1,c(0.95))[[1]],
    mean(std_row_3000_1),      quantile(std_row_3000_1,c(0.05))[[1]],       quantile(std_row_3000_1,c(0.95))[[1]]),
  c(mean(mean_row_3000_2),     quantile(mean_row_3000_2,c(0.05))[[1]],      quantile(mean_row_3000_2,c(0.95))[[1]],
    mean(std_row_3000_2),      quantile(std_row_3000_2,c(0.05))[[1]],       quantile(std_row_3000_2,c(0.95))[[1]])
  )
)






#################################################
######  two-sample Kolmogorov-Smirnov test.######
#################################################

# combined_location VS combined_season [E46]
ks.test(mean_row_3000, mean_row_season_3000)

# combined_location VS GI_location [E47]
ks.test(mean_row_3000, mean_row_3000_1)

# combined_location VS GI_season [E48]
ks.test(mean_row_3000, mean_row_season_3000_1)

# combined_location VS GII_location [E49]
ks.test(mean_row_3000, mean_row_3000_2)

# combined_location VS GII_season [E50]
ks.test(mean_row_3000, mean_row_season_3000_2)

# combined_season VS GI_location [F47]
ks.test(mean_row_season_3000, mean_row_3000_1)

# combined_season VS GI_season [F48]
ks.test(mean_row_season_3000, mean_row_season_3000_1)


# combined_season VS GII_location [F49]
ks.test(mean_row_season_3000, mean_row_3000_2)

# combined_season VS GII_season [F50]
ks.test(mean_row_season_3000, mean_row_season_3000_2)


# GI_location VS GI_season [G48]
ks.test(mean_row_3000_1, mean_row_season_3000_1)

# GI_location VS GII_location [G49]
ks.test(mean_row_3000_1, mean_row_3000_2)

# GI_location VS GII_season [G50]
ks.test(mean_row_3000_1, mean_row_season_3000_2)


# GI_season VS GII_location [H49]
ks.test(mean_row_season_3000_1, mean_row_3000_2)

# GI_season VS GII_season [H50]
ks.test(mean_row_season_3000_1, mean_row_season_3000_2)

# GII_location VS GII_season [I50]
ks.test(mean_row_3000_2, mean_row_season_3000_2)



outpth <- "D:/Dropbox/_ICF_project/QMRA/QMRA_Dist/QMRA_NOV/paper/saved_results.xlsx"
write.xlsx(x = strat_season_pool, file = outpth, sheetName = "strat_season_pool", row.names = FALSE, append=TRUE)
write.xlsx(x = strat_location_pool, file = outpth, sheetName = "strat_location_pool", row.names = FALSE, append=TRUE)