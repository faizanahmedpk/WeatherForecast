library(ggplot2)
library(dplyr)
library(ggfortify)

# RGENERATEPREC

#df_rgenerateprec <- df
#write.csv(df_rgenerateprec, "df_rgenerateprec.csv")

df_rgenerateprec <- read.csv(file = "df_rgenerateprec.csv", header = T)
View(df_rgenerateprec)
df_rgenerateprec$X<- NULL

#Time Series plot for generated vs observed
ggdf_rgenerateprec <- ggplot(df_rgenerateprec, aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~ variable, nrow = 2) + 
  ggtitle("Time series plot between oberved and generated") + 
  ylab("Rainfall values of observed vs generated in mm") 

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


ggdf_rmawgen <- ggplot(df_rmawgen, aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~ variable, nrow = 2) + 
  ggtitle("Time series plot between oberved and generated") + 
  ylab("Rainfall values of observed vs generated in mm") 

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
 
# boxplot for rmawgen gen vs observed

#str(df_rmawgen)
#ggbox_rmawgen <- ggplot(df_rmawgen , aes(x = factor(variable), y = value)) + geom_boxplot()

#ggbox_rmawgen


data_rmawgen <- df_rmawgen %>% filter(variable == "gen") %>% dplyr::select(value)
names(data_rmawgen) <- c("rmawgen")
#names(data_rmawgen) <- c("rmawgen", "month")
#data_rmawgen$date <- as.Date(data_rmawgen$date, format = "%Y-%m-%d")
#data_rmawgen$month <- as.factor(data_rmawgen$month)
str(data_rmawgen)

data_observed <- df_rmawgen %>% filter(variable == "obs") %>% dplyr::select(date , month, value)
#names(data_observed) <- c("observed")
names(data_observed) <- c("date", "month" ,"observed")
#data_observed$date <- as.Date(data_observed$date, format = "%Y-%m-%d")
#data_observed$month <- as.factor(data_observed$month)
str(data_observed)


data_rgenerateprec <- df_rgenerateprec %>% filter(variable == "gen") %>% dplyr::select(value)
names(data_rgenerateprec) <- c("rgenerateprec")
#names(data_rgenerateprec) <- c("rgenerateprec", "month")
#data_rgenerateprec$date <- as.Date(data_rgenerateprec$date, format = "%Y-%m-%d")
#data_rgenerateprec$month <- as.factor(data_rgenerateprec$month)
str(data_rgenerateprec)


data_all <- cbind(data_observed, data_rmawgen, data_rgenerateprec)

library(tidyr)
data_all_long <- gather(data_all, variable, value, 3:5, factor_key = TRUE)
str(data_all_long)
data_all_long$month <- factor(data_all_long$month)

# boxplot
ggplot(data_all_long, aes(x = factor(variable), y = value)) +geom_boxplot()

# boxplot for months 
ggplot(data_all_long, aes(x = month), y = vaue) + facet_wrap( ~ variable) +geom_boxplot()
ggplot(data_all_long, aes(x = month), y = vaue) + facet_grid(. ~ variable) +geom_boxplot()
# density plots 
observed.ecdf <- ecdf(data_all$observed)
autoplot(observed.ecdf) + xlab("observed") + ggtitle("Emperical density plot for observed data")


rmawgen.ecdf <- ecdf(data_all$rmawgen)
autoplot(rmawgen.ecdf) + xlab("rmawgen") + ggtitle("Emperical density plot for rmawgen data")

rgenerateprec.ecdf <- ecdf(data_all$rgenerateprec)
autoplot(rgenerateprec.ecdf) + xlab("rgenerateprec") + ggtitle("Emperical density plot for rgenerateprec data")



