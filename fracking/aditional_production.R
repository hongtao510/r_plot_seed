library(dplyr)
library(stringr)
library(openxlsx)




load_data <- function(pth) {

	############################################################
	ring_val_vec <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", 
		              "R6_500FT", "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
		              "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
		              "R15_1800FT","R16_2000FT")
	n_stations_vec <- c(rep(36, 6), rep(72, 5), rep(120, 5))


	all_data <- data.frame()
	for (jj in c(1:16)){
		ring_val <- ring_val_vec[jj]
		cat(ring_val, "\n")

		# load AERMOD unsampled value
		AERMOD_raw <- readRDS(paste0(pth, ring_val, ".rds"))
		all_data <- rbind(all_data, AERMOD_raw)
	}

	all_datasub <- select(all_data, AVERAGE_CONC, DATE, RING, Station_ID)
	all_datasub['year'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 1, 2))
	# all_datasub['month'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 3, 4))
	# all_datasub['day'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 5, 6))
	# all_datasub['hr'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 7, 8))

	# all_datasub_dail_max <- group_by(all_datasub, year, month, day) %>% summarise(max_daily=max(AVERAGE_CONC))
	return (all_datasub)
}


RIFLE_max_daily_hr <- load_data("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/AERMOD_data/RIFLE_AERMOD/AERMOD_")

RIFLE_max_daily_hr_exclude_zero <- filter(RIFLE_max_daily_hr, AVERAGE_CONC!=0)



all_datasub_dail_max <- group_by(RIFLE_max_daily_hr_exclude_zero, year, RING, Station_ID) %>% summarise(average=mean(AVERAGE_CONC))

wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "data", gridLines = TRUE)
writeDataTable(wb = wb, sheet = "data", x = filter(all_datasub_dail_max, year=='08'))
saveWorkbook(wb, "C:/Users/TH/Desktop/RIFLE_production_08_avg.xlsx", overwrite = TRUE)


