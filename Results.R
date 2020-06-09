library(ggplot2)
library(dplyr)
library(ggfortify)
library(lubridate)
library(plotly)
library(scales)

# RGENERATEPREC

#df_rgenerateprec <- df
#write.csv(df_rgenerateprec, "df_rgenerateprec.csv")

df_rgenerateprec <- read.csv(file = "df_rgenerateprec.csv", header = T)
View(df_rgenerateprec)
str(df_rgenerateprec)
summary(df_rgenerateprec)
df_rgenerateprec$X<- NULL
df_rgenerateprec$date <- as_date(df_rgenerateprec$date)
df_rgenerateprec_ts <- ts(df_rgenerateprec$value, start = 1989, frequency = 4) 


autoplot(df_rgenerateprec_ts)
#Time Series plot for generated vs observed
ggdf_rgenerateprec <- ggplot(df_rgenerateprec, aes(x = date, y = value, color = variable)) +
  geom_line() +
  #facet_wrap(~ variable, nrow = 2) + 
  ggtitle("Time series plot between oberved and generated for RGENERATEPREC") + 
  ylab("Rainfall values of observed vs generated in mm") + scale_x_date(breaks = date_breaks("year")) +
  theme(axis.text.x = element_text(angle=45))

ggdf_rgenerateprec


df_rgenerateprec_ts_obs <- df_rgenerateprec %>% filter(variable == "obs") %>% dplyr::select(value)
df_rgenerateprec_ts_obs <- ts(df_rgenerateprec_ts_obs, start(1989, 1), frequency = 7)
autoplot(stats::acf(df_rgenerateprec_ts_obs)) + ggtitle("Auto correlation for observed rainfall")
autoplot(stats::pacf(df_rgenerateprec_ts_obs)) + ggtitle("Partial correlation for observed rainfall")

df_rgenerateprec_ts_gen <- df_rgenerateprec %>% filter(variable == "gen") %>% dplyr::select(value)
df_rgenerateprec_ts_gen <- ts(df_rgenerateprec_ts_gen, start(1989, 1), frequency = 7)
autoplot(stats::acf(df_rgenerateprec_ts_gen)) + ggtitle("Auto correlation for generated rainfall")
autoplot(stats::pacf(df_rgenerateprec_ts_gen)) + ggtitle("Partial correlation for generated rainfall")



# RMAWGEN
#rmawgen_gen <- generationP01GPCA_prec$prec_gen
#rmawgen_mes <- generationP01GPCA_prec$prec_mes


  
  
#df1 <- list(obs=rmawgen_mes[,names(rmawgen_gen)],gen=rmawgen_gen)
#for ( i in 1:length(df1)) {
#  df1[[i]]$date <- as.Date(origin)+days(1:nrow(df1[[i]]))-1
#}


#df1 <- melt(df1,id="date")
#names(df1)[names(df1)=="variable"] <- "station"
#names(df1)[names(df1)=="L1"] <- "variable"
#df1$month <- factor(month(df1$date))
#df1$season <- "none"
#df1$season[df1$month %in% c(12,2,1)] <- "1.DJF"
#df1$season[df1$month %in% c(3,4,5)] <-  "2.MAM"
#df1$season[df1$month %in% c(6,7,8)] <-  "3.JJA"
#df1$season[df1$month %in% c(9,10,11)] <-  "4.SON"

#df_rmawgen <- df1

#write.csv(df_rmawgen, "df_rmawgen.csv")

df_rmawgen <- read.csv("df_rmawgen.csv", header = T)
View(df_rmawgen)
df_rmawgen$X <- NULL
df_rmawgen$date <- as_date(df_rmawgen$date)
df_rmawgen_ts <- ts(df_rgenerateprec$value, start = 1989, frequency = 4) 




ggdf_rmawgen <- ggplot(df_rmawgen, aes(x = date, y = value, color = variable)) + 
  geom_line() +
  #facet_wrap(~ variable, nrow = 2) + 
  ggtitle("Time series plot between  oberved and generated for RMAWGEN") + 
  ylab("Rainfall values of observed vs generated in mm")  + scale_x_date(breaks = date_breaks("year")) +
  theme(axis.text.x = element_text(angle=45))

ggdf_rmawgen


df_rmawgen_ts_obs <- df_rmawgen %>% filter(variable == "obs") %>% dplyr::select(value)
df_rmawgen_ts_obs <- ts(df_rmawgen_ts_obs, start(1989, 1), frequency = 7)
autoplot(stats::acf(df_rmawgen_ts_obs)) + ggtitle("Auto correlation for observed rainfall")
autoplot(stats::pacf(df_rmawgen_ts_obs)) + ggtitle("Partial correlation for observed rainfall")

df_rmawgen_ts_gen <- df_rmawgen %>% filter(variable == "gen") %>% dplyr::select(value)
df_rmawgen_ts_gen <- ts(df_rmawgen_ts_gen, start(1989, 1), frequency = 7)
autoplot(stats::acf(df_rmawgen_ts_gen)) + ggtitle("Auto correlation for generated rainfall")
autoplot(stats::pacf(df_rmawgen_ts_gen)) + ggtitle("Partial correlation for generated rainfall")






# boxplot for rmawgen gen vs observed
#data_boxplot <- data.frame(date = df_rmawgen$date, rgenerate = df_rgenerateprec$value)
#data_boxplot_rmawgen <- df_rmawgen %>% filter(variable == "obs") %>% dplyr::select(date, value)
 


str(df_rmawgen)
ggbox_rmawgen <- ggplot(df_rmawgen , aes(x = factor(variable), y = value)) + geom_boxplot()

ggbox_rmawgen




