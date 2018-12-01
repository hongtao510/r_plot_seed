library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
library(extrafont)
library(stringr)

loadfonts()

theme_Publication <- function(base_size=14) {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size)
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

theme_Publication_no_legend <- function(base_size=14) {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90, vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               # axis.text = element_text(),
               axis.text.x = element_text(angle = 90, hjust = 1),
               axis.line.x = element_line(color="black", size = 1),
               axis.line.y = element_line(color="black", size = 1),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "none",
               legend.direction = "horizontal",
               legend.key.size= unit(1, "cm"),
               legend.margin = unit(0.2, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_blank(),#element_rect(colour="#f0f0f0", fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}


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

	all_datasub <- select(all_data, AVERAGE_CONC, DATE)
	all_datasub['year'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 1, 2))
	all_datasub['month'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 3, 4))
	all_datasub['day'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 5, 6))
	all_datasub['hr'] <- lapply(all_datasub['DATE'], function(x) str_sub(x, 7, 8))

	all_datasub_dail_max <- group_by(all_datasub, year, month, day) %>% summarise(max_daily=max(AVERAGE_CONC))
	return (all_datasub_dail_max)
}


RIFLE_max_daily_hr <- load_data("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/AERMOD_data/RIFLE_AERMOD/AERMOD_")
RIFLE_max_daily_hr$location <- "Rifle"


BarD_max_daily_hr <- load_data("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/AERMOD_data/BarD_AERMOD/PD_BARD_")
BarD_max_daily_hr$location <- "BarD"


PD_AB_max_daily_hr <- load_data("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/AERMOD_data/PD_AERMOD/PD_ANHEUSERBUSCH_")
PD_AB_max_daily_hr$location <- "Anheuser-Busch"


PD_ST_max_daily_hr <- load_data("D:/Dropbox/_ICF_project/Fracking/Monte_Carlo/AERMOD_data/PD_AERMOD/PD_FTST_VRAIN_")
PD_ST_max_daily_hr$location <- "St_Vrain"

all_max_daily_hr <- rbind(RIFLE_max_daily_hr, BarD_max_daily_hr, PD_AB_max_daily_hr, PD_ST_max_daily_hr)


# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


all_max_daily_hr$logvalue <- log10(all_max_daily_hr$max_daily)

ggplot(all_max_daily_hr, aes(y = logvalue, x =month))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Month", y = bquote('Chi/Q log10 scale'), title="") +
      scale_y_continuous(limits = c(2.5, 4.0), breaks = seq(2.5, 4.0, by = 0.5))+
      facet_wrap(~location, scales="free")+
      # scale_fill_discrete(name = "AERMOD Rings", labels=ring_label)+
      theme_Publication(15)


# check groupby metrics
all_max_yearly_sum <- all_max_daily_hr %>% group_by(year, location) %>% summarise(yearly_avg=mean(max_daily))

  year       location yearly_avg
  <chr>          <chr>      <dbl>
1    02           BarD   3534.659
2    04           BarD   3674.542
3    05          Rifle   4415.233
4    06          Rifle   4606.997
5    07          Rifle   4611.970
6    08          Rifle   4702.605
7    09          Rifle   4539.301
8    09       St_Vrain   4801.684
9    88 Anheuser-Busch   3868.295