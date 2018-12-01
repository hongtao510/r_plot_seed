library(dplyr)
library(openxlsx)
library(foreach)
library(doParallel)
library(gtools)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(scales)
library(gtable)
library(extrafont)

loadfonts()

pth <- "I:/Fracking/BarD_3acre/"
setwd(pth)


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



ring_val_vec <- c("R1_150FT", "R2_250FT", "R3_300FT", "R4_350FT", "R5_400FT", 
	              "R6_500FT", "R7_600FT", "R8_700FT", "R9_800FT", "R10_900FT", 
	              "R11_1000FT", "R12_1200FT", "R13_1400FT", "R14_1600FT", 
	              "R15_1800FT","R16_2000FT")




## Step 1: extract the data (4 stations per ring)
for (ring_index in c(3:16)){
	ring_val <- ring_val_vec[ring_index]

	cat(ring_val, "\n")

	# list all the output files
	all_ouput <- list.files(paste0("./", ring_val), pattern = "^R")
	all_ouput <- mixedsort(all_ouput)

	# register number of cores
	cl <- makeCluster(12)
	registerDoParallel(cl, cores = 12)

	if ((ring_index>=1) && (ring_index<=6)){
		station_list <- c(1, 10, 19, 28)
	} else if((ring_index>=7) && (ring_index<=11)){
		station_list <- c(1, 19, 37, 55)
	} else if((ring_index>=12) && (ring_index<=16)){
		station_list <- c(1, 31, 61, 91)
	}

	# process 6 stations at a time, how many times can cover all stations
	# length(all_ouput)
	out <- foreach(q = 1:length(all_ouput), .combine='rbind', .packages=c('dplyr')) %dopar% {
			load_results <- readRDS(paste0("./", ring_val, "/", all_ouput[q])) %>% filter(Station_ID %in% station_list) %>%
			                select(Activity, Station_ID, Iter, ZELEV, ZHILL, ZFLAG, DATE, RING, Operation_Type, 
                                   AERMOD_slice, Dur, benzene, toluene, ethylbenzene, m.p.xylene=`m+p-xylene`, o.xylene=`o-xylene`, isoprene)
			return(load_results)
	}
	write.csv(as.data.frame(out), paste0("./", ring_val, "/", "/Correlation_calc_", ring_val, ".csv"), row.names =FALSE)
	# saveRDS(as.data.frame(out), paste0("./", ring_val, "/", "/Correlation_calc_", ring_val, ".csv"), compress = "xz")
}






### Step 2: pool data together
all_extract_df <- data.frame()
for (zz in 3:16){
	cat(zz, "\n")
	ring_val <- ring_val_vec[zz]
	lit_temp <- read.csv(paste0("./", ring_val, "/", "/Correlation_calc_", ring_val, ".csv"), stringsAsFactors=FALSE)
	all_extract_df <- rbind(all_extract_df, lit_temp)
}

drilling_dur_list <- sapply(strsplit(all_extract_df[,"Dur"], "_"), function(x) x[[1]])
fracking_dur_list <- sapply(strsplit(all_extract_df[,"Dur"], "_"), function(x) x[[2]])
flowback_dur_list <- sapply(strsplit(all_extract_df[,"Dur"], "_"), function(x) x[[3]])

all_extract_df$drilling_dur <- as.numeric(drilling_dur_list)
all_extract_df$fracking_dur <- as.numeric(fracking_dur_list)
all_extract_df$flowback_dur <- as.numeric(flowback_dur_list)


saveRDS(all_extract_df, "BarD_3acre_correlation_calc.rds")





### Step 3: analysis 
all_extract_df <- readRDS("BarD_3acre_correlation_calc.rds")
all_extract_df$xylene <- all_extract_df$o.xylene + all_extract_df$m.p.xylene
all_extract_df <- select(all_extract_df, -o.xylene, -m.p.xylene)


benzene_df <- all_extract_df %>% select(Activity, ZELEV, ZHILL, DATE, RING, drilling_dur, fracking_dur, flowback_dur, benzene) 
toluene_df <- all_extract_df %>% select(Activity, ZELEV, ZHILL, DATE, RING, drilling_dur, fracking_dur, flowback_dur, toluene) 
ethylbenzene_df <- all_extract_df %>% select(Activity, ZELEV, ZHILL, DATE, RING, drilling_dur, fracking_dur, flowback_dur, ethylbenzene) 
isoprene_df <- all_extract_df %>% select(Activity, ZELEV, ZHILL, DATE, RING, drilling_dur, fracking_dur, flowback_dur, isoprene) 
xylene_df <- all_extract_df %>% select(Activity, ZELEV, ZHILL, DATE, RING, drilling_dur, fracking_dur, flowback_dur, xylene) 





drilling_dur_list <- sapply(strsplit(all_extract_df[,"Dur"], "_"), function(x) x[[1]])





## benzene
cor(x=benzene_df$RING, y=benzene_df$benzene, method = 'spearman')  			#  -0.4595609
cor(x=benzene_df$ZELEV, y=benzene_df$benzene, method = 'spearman')			#   0.1228273
cor(x=benzene_df$ZHILL, y=benzene_df$benzene, method = 'spearman')          #   0.09614437


## toluene
cor(x=toluene_df$RING, y=toluene_df$toluene, method = 'spearman')  			#  -0.4216666
cor(x=toluene_df$ZELEV, y=toluene_df$toluene, method = 'spearman')			#   0.1133651
cor(x=toluene_df$ZHILL, y=toluene_df$toluene, method = 'spearman')          #   0.0888021


## ethylbenzene
cor(x=ethylbenzene_df$RING, y=ethylbenzene_df$ethylbenzene, method = 'spearman')  			#  -0.4455236
cor(x=ethylbenzene_df$ZELEV, y=ethylbenzene_df$ethylbenzene, method = 'spearman')			#  0.1192431
cor(x=ethylbenzene_df$ZHILL, y=ethylbenzene_df$ethylbenzene, method = 'spearman')			#  0.09337159


## isoprene
cor(x=isoprene_df$RING, y=isoprene_df$isoprene, method = 'spearman')  			#  -0.3680071
cor(x=isoprene_df$ZELEV, y=isoprene_df$isoprene, method = 'spearman')			#  0.09966466
cor(x=isoprene_df$ZHILL, y=isoprene_df$isoprene, method = 'spearman')			#  0.07810325


## xylene
cor(x=xylene_df$RING, y=xylene_df$xylene, method = 'spearman')  		#  -0.4407827
cor(x=xylene_df$ZELEV, y=xylene_df$xylene, method = 'spearman')			#  0.1180737
cor(x=xylene_df$ZHILL, y=xylene_df$xylene, method = 'spearman')			#  0.09245582



# function for computing mean, DS, max and min values
min.median.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), median(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

p1 <- ggplot(benzene_df, aes(y = log10(benzene), x =as.factor(drilling_dur)))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Oil and Gas Well Setup", y = bquote('Benzene Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-4.5, 5.0), breaks = seq(-4.5, 5.0, by = 1))+
      scale_x_discrete(labels=c("4" = "Vertical", "6" = "Horizontal 1 mile", "7" = "Horizontal 2 mile")) +
      theme_Publication()



p2 <- ggplot(toluene_df, aes(y = log10(toluene), x =as.factor(drilling_dur)))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Oil and Gas Well Setup", y = bquote('Toluene Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-4.0, 5.0), breaks = seq(-4.0, 5.0, by = 1))+
      scale_x_discrete(labels=c("4" = "Vertical", "6" = "Horizontal 1 mile", "7" = "Horizontal 2 mile")) +
      theme_Publication()



p3 <- ggplot(ethylbenzene_df, aes(y = log10(ethylbenzene), x =as.factor(drilling_dur)))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Oil and Gas Well Setup", y = bquote('Ethylbenzene Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-5.5, 4.0), breaks = seq(-5.5, 4.0, by = 1))+
      scale_x_discrete(labels=c("4" = "Vertical", "6" = "Horizontal 1 mile", "7" = "Horizontal 2 mile")) +
      theme_Publication()



p4 <- ggplot(isoprene_df, aes(y = log10(isoprene), x =as.factor(drilling_dur)))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Oil and Gas Well Setup", y = bquote('Isoprene Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-6.5, 3.0), breaks = seq(-6.5, 3.0, by = 1))+
      scale_x_discrete(labels=c("4" = "Vertical", "6" = "Horizontal 1 mile", "7" = "Horizontal 2 mile")) +
      theme_Publication()



p5 <- ggplot(xylene_df, aes(y = log10(xylene), x =as.factor(drilling_dur)))+
      stat_summary(fun.data = min.median.sd.max, geom="boxplot",position = position_dodge()) +
      labs(x = "Oil and Gas Well Setup", y = bquote('Xylene Concentrations (log10 scale, '*ug~ m^-3*')'), title="") +
      scale_y_continuous(limits = c(-5.0, 5.0), breaks = seq(-5.0, 5.0, by = 1))+
      scale_x_discrete(labels=c("4" = "Vertical", "6" = "Horizontal 1 mile", "7" = "Horizontal 2 mile")) +
      theme_Publication()





grid.arrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)





library(lsr)
# https://cran.r-project.org/web/packages/lsr/lsr.pdf
anova3 <- aov( benzene_df$benzene ~ benzene_df$drilling_dur ) # run the ANOVA
summary( anova3 ) # print the ANOVA table
etaSquared( anova3 ) # effect size


anova4 <- aov( toluene_df$toluene ~ toluene_df$drilling_dur ) # run the ANOVA
summary( anova4 ) # print the ANOVA table
etaSquared( anova4 ) # effect size

anova5 <- aov( ethylbenzene_df$ethylbenzene ~ ethylbenzene_df$drilling_dur ) # run the ANOVA
summary( anova5 ) # print the ANOVA table
etaSquared( anova5 ) # effect size


anova6 <- aov( isoprene_df$isoprene ~ isoprene_df$drilling_dur ) # run the ANOVA
summary( anova6 ) # print the ANOVA table
etaSquared( anova6 ) # effect size


anova7 <- aov( xylene_df$xylene ~ xylene_df$drilling_dur ) # run the ANOVA
summary( anova7 ) # print the ANOVA table
etaSquared( anova7 ) # effect size


# outcome <- c( 1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4 ) # data
# treatment1 <- factor( c( 1,1,1,2,2,2,3,3,3 )) # grouping variable
# anova1 <- aov( outcome ~ treatment1 ) # run the ANOVA
# summary( anova1 ) # print the ANOVA table
# etaSquared( anova1 ) # effect size


# treatment2 <- factor( c( 1,2,3,1,2,3,1,2,3 )) # second grouping variable
# anova2 <- aov( outcome ~ treatment1 + treatment2 ) # run the ANOVA
# summary( anova2 ) # print the ANOVA table
# etaSquared( anova2 ) # effect size
