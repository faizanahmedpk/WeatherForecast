
#RGeneratePrecFunction <- function(){
  rm(list=ls())
  
  
  library(RGENERATEPREC)
  load('BullooModInp.RData')
  
  
  
  str(TEMPERATURE_MAX)
  str(TEMPERATURE_MIN)
  str(PRECIPITATION)
  
  
  
  CatStations <- CatStations[CatStations$Available==1,]
  plot(BuffArea,typ='l',asp=1,xlab='longitute',ylab='latitude')
  lines(CatBoundary,col='red')
  with(CatStations,points(lon,lat))
  
  
  year_min <- 1989
  year_max <- 2018
  
  origin <- paste(year_min,1,1,sep="-")
  end <- paste(year_max,12,31,sep="-")
  
  period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
  period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max
  
  prec_mes <- PRECIPITATION[period,]
  Tx_mes <- TEMPERATURE_MAX[period_temp,]
  Tn_mes <- TEMPERATURE_MIN[period_temp,]
  
  accepted <- array(TRUE,length(names(prec_mes)))
  names(accepted) <- names(prec_mes)
  # accepted
  for (it in names(prec_mes)) {
    acc <- TRUE
    acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
    acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
    accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
    
  }
  names(accepted)
  
  
  prec_mes <- prec_mes[,accepted]
  head(prec_mes)
  
  
  Tx_mes <- Tx_mes[,accepted]
  Tn_mes <- Tn_mes[,accepted]
  
  
  valmin <- 1.0
  prec_occurence_mes <- prec_mes
  station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
  prec_occurence_mes[,station] <- prec_mes[,station]>=valmin
  head(prec_occurence_mes)
  
  
  # ---- Generation of Precipitation in 10 Stations ----
  
  
  they <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001", "T45002", "T45003", "T45004","T45007") ##station
  exogen <- Tx_mes[,they]-Tn_mes[,they]
  months <- factor(prec_mes$month)
  model <- PrecipitationOccurrenceMultiSiteModel(x=prec_mes[,they],exogen=exogen,origin=origin)
  
  names(model)
  class(model[[1]])
  class(model$ccgamma)
  class(model$K)
  class(model$type)
  
  
  prec_gen_occ <- generate(model,exogen=exogen,monthly.factor=months,n=length(months))
  
  str(prec_gen_occ)
  
  # Amount

  model_amount <- PrecipitationAmountModel(prec_mes,station=they,origin=origin)
  names(model_amount)
  class(model_amount)
  
  
  # The first elements are generalized linear models, 
  # in which Gaussianized values of precipitation depend on month of the year:
  
  for (its in model_amount$station) summary(model_amount[[its]])
  
  
  model_amount$station
  
  model_amount$sample
  
  model_amount$origin
  
  model_amount$valmin
  
  # Finally, a data frame with the generated times series for each gauging station site is obtained as follows:
  
  prec_gen <- generate(model_amount,newdata=prec_gen_occ)
  
  names(prec_gen)
  
  # Comparison 
  #Quantile-quantile plots for precipitation time series both station are created as follows
  #(if not specified, measurment units are millimiters):
    
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(reshape2)
  
  str(prec_mes)
  
  str(prec_gen)
  
  
  df <- list(obs=prec_mes[names(prec_gen)],gen=prec_gen)
  for ( i in 1:length(df)) {
    df[[i]]$date <- as.Date(origin)+days(1:nrow(df[[i]]))-1
  }
  
  
  df <- melt(df,id="date")
  names(df)[names(df)=="variable"] <- "station"
  names(df)[names(df)=="L1"] <- "variable"
  df$month <- factor(month(df$date))
  df$season <- "none"
  df$season[df$month %in% c(12,2,1)] <- "1.DJF"
  df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
  df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
  df$season[df$month %in% c(9,10,11)] <-  "4.SON"
  
  
  qqplot_ <- function(df) {
    df <- as.list(df)
    o <- qqplot(df[[1]],df[[2]],plot.it=FALSE)
    names(o) <- names(df)[1:2]
    o <- as.data.frame(o)
    return(o)
    
  }
  
  
  
  qqdf <- split(df,f=paste(df$station,df$season,sep="_")) %>% 
    lapply(FUN=function(x){list(obs=x$value[x$variable=="obs"],gen=x$value[x$variable=="gen"])}) %>%
    lapply(FUN=qqplot_) %>% 
    melt(id=c("obs","gen"))
  qqdf_add <- str_split(qqdf$L1,pattern="_") %>% do.call(what=rbind) %>% as.data.frame()
  names(qqdf_add) <- c("station","season")
  qqdf <- cbind(qqdf,qqdf_add)
  
  #they <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001", "T45002", "T45003", "T45004","T45007")
  summary(qqdf)
  qqdf_T36133 <- qqdf %>% filter(station %in% c("T36133", "T36137", "T44040", "T44120", "T45000")) 
  head(qqdf_T36133)
  ## ggplot plot
  ggdf_T36133 <- ggplot(data=qqdf_T36133)+
    geom_point(aes(x=obs,y=gen))+
    theme_bw()+
    geom_abline()+
    facet_grid(station ~ season)+xlab("observed")+
    ylab("generated")
  #ggsave("ggdf.png",height = 4, width = 8)
  
  
   show(ggdf_T36133)
  
#}
