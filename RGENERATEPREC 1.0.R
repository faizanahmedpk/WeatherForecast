
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
  #summary(qqdf)
  str(qqdf)
  
  qqdf_T36133_to_T45000 <- qqdf %>% filter(station %in% c("T36133", "T36137", "T44040", "T44120", "T45000")) 
  #head(qqdf_T36133_to_T45000)
  ## ggplot plot
  ggdf_T36133_to_T45000 <- ggplot(data=qqdf_T36133_to_T45000)+
    geom_point(aes(x=obs,y=gen))+
    theme_bw()+
    geom_abline()+
    facet_grid(station ~ season)+xlab("observed")+
    ylab("generated")
  ggsave("ggdf_T36133_to_T45000.png",height = 4, width = 8)
  
  
   show(ggdf_T36133_to_T45000)
  
   
   # Next Five station plots
   qqdf_T45001_to_T45007 <- qqdf %>% filter(station %in% c("T45001", "T45002", "T45003", "T45004","T45007"))
   head(qqdf_T45001_to_T45007)
   
   
   ggplot_T45001_to_T45007 <- ggplot(data = qqdf_T45001_to_T45007) + 
     geom_point(aes(x = obs, y= gen)) + 
     theme_bw() +
     geom_abline() +
     facet_grid(station ~ season) + 
     xlab("observed") + ylab("generated")
   ggsave("ggplot_T45001_to_T45007.png",height = 4, width = 8)
   show(ggplot_T45001_to_T45007)
   # Plots Done for rain in mm
   
   dw <- list()
   dw$obs <- dw.spell(prec_mes[,they],origin=origin)
   nn <- names(dw$obs[[1]])
   dw$obs <- melt(dw$obs,id=nn)
   dw$obs$variable <- "obs"
   dw$gen <- dw.spell(prec_gen[,they],origin=origin) %>% melt(id=nn)
   dw$gen$variable <- "gen"
   
   dw <- do.call(what=rbind,args=dw)
   names(dw)[names(dw)=="L1"] <- "station"
   
   dw$season <- "none"
   dw$season[dw$month %in% c(12,2,1)] <- "1.DJF"
   dw$season[dw$month %in% c(3,4,5)] <-  "2.MAM"
   dw$season[dw$month %in% c(6,7,8)] <-  "3.JJA"
   dw$season[dw$month %in% c(9,10,11)] <-  "4.SON"  
   
   qqdw <- split(dw,f=paste(dw$spell_state,dw$station,dw$season,sep="_")) %>% lapply(FUN=function(x){list(obs=x$spell_length[x$variable=="obs"],gen=x$spell_length[x$variable=="gen"])}) %>% lapply(FUN=qqplot_) %>% melt(id=c("obs","gen"))
   
   qqdw_add <- str_split(qqdw$L1,pattern="_") %>% do.call(what=rbind) %>% as.data.frame()
   names(qqdw_add) <- c("state","station","season")
   qqdw <- cbind(qqdw,qqdw_add)
   
   
   # Dry spells are as follows
   
   # first for 5 states
   
   qqdry_T36133_to_T45000 <- qqdw %>% 
     filter(state == "dry", station %in% c("T36133", "T36137", "T44040", "T44120", "T45000"))
   
   ggplot_dry_T36133_to_T45000 <- ggplot(data = qqdry_T36133_to_T45000) + 
     geom_point(aes(x=obs,y=gen))+
     theme_bw()+
     geom_abline()+
     facet_grid(station ~ season)+xlab("observed")+
     ylab("generated")
   
   ggsave("ggplot_dry_T36133_to_T45000.png",height = 4, width = 8)
   
   show(ggplot_dry_T36133_to_T45000)
   
   # Plots for next 5 station dry
   qqdry_T45001_to_T45007 <- qqdw %>% 
     filter(state == "dry",station %in% c("T45001", "T45002", "T45003", "T45004","T45007")) 
   
   ggplot_dry_T45001_to_T45007 <- ggplot(data = qqdry_T45001_to_T45007) + 
     geom_point(aes(x=obs,y=gen))+
     theme_bw()+
     geom_abline()+
     facet_grid(station ~ season)+xlab("observed")+
     ylab("generated")
   
   ggsave("ggplot_dry_T45001_to_T45007.png",height = 4, width = 8)
   
   show(ggplot_dry_T45001_to_T45007)
   
   
   # Now Wet spells are:
   
   # for first 5 stations
   
   qqwet_T36133_to_T45000 <- qqdw %>% 
     filter(state == "wet", station %in% c("T36133", "T36137", "T44040", "T44120", "T45000"))
   
   ggplot_wet_T36133_to_T45000 <- ggplot(data = qqwet_T36133_to_T45000) + 
     geom_point(aes(x=obs,y=gen))+
     theme_bw()+
     geom_abline()+
     facet_grid(station ~ season)+xlab("observed")+
     ylab("generated")
   
   ggsave("ggplot_wet_T36133_to_T45000.png",height = 4, width = 8)
   
   show(ggplot_wet_T36133_to_T45000)
   
   # Plots for next 5 station dry
   qqwet_T45001_to_T45007 <- qqdw %>% 
     filter(state == "wet",station %in% c("T45001", "T45002", "T45003", "T45004","T45007")) 
   
   ggplot_wet_T45001_to_T45007 <- ggplot(data = qqwet_T45001_to_T45007) + 
     geom_point(aes(x=obs,y=gen))+
     theme_bw()+
     geom_abline()+
     facet_grid(station ~ season)+xlab("observed")+
     ylab("generated")
   
   ggsave("ggplot_wet_T45001_to_T45007.png",height = 4, width = 8)
   
   show(ggplot_wet_T45001_to_T45007)
   # Plots done
   
   months <- unique(prec_mes$month)
   ks_test_T36133 <- list()
   ks_test_T36137 <- list()
   ks_test_T44040 <- list()
   ks_test_T44120 <- list()
   ks_test_T45000 <- list()
   ks_test_T45001 <- list()
   ks_test_T45002 <- list()
   ks_test_T45003 <- list()
   ks_test_T45004 <- list()
   ks_test_T45007 <- list()
   
     for (m in months) {
       ks_test_T36133[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T36133"],
                                      prec_gen[prec_mes$month==m,"T36133"])
     }
     for (m in months) {
       ks_test_T36137[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T36137"],prec_gen[prec_mes$month==m,"T36137"])
     }
     for (m in months) {
       
       ks_test_T44040[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T44040"]
                                      ,prec_gen[prec_mes$month==m,"T44040"])
     }
     for (m in months) {
       ks_test_T44120[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T44120"],
                                      prec_gen[prec_mes$month==m,"T44120"]
       )
     }
     for (m in months) {
       ks_test_T45000[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45000"],
                                      prec_gen[prec_mes$month==m,"T45000"]
       )
     }
     for (m in months) {
       ks_test_T45001[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45001"],
                                      prec_gen[prec_mes$month==m,"T45001"]
       )
     }
     for (m in months) {
       ks_test_T45002[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45002"],
                                      prec_gen[prec_mes$month==m,"T45002"]
       )
     }
     for (m in months) {
       ks_test_T45003[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45003"],
                                      prec_gen[prec_mes$month==m,"T45003"]
       )
     }
     for (m in months) {

       ks_test_T45004[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45004"],
                                      prec_gen[prec_mes$month==m,"T45004"])
     }
     for (m in months) {
       ks_test_T45007[[m]] <- ks.test(prec_mes[prec_mes$month==m,"T45007"],
                                      prec_gen[prec_mes$month==m,"T45007"])
     }
     
   
   ks_test_T36133 
   ks_test_T36137 
   ks_test_T44040 
   ks_test_T44120 
   ks_test_T45000 
   ks_test_T45001 
   ks_test_T45002 
   ks_test_T45003 
   ks_test_T45004 
   ks_test_T45007 
   
   
   #  spatial statistical correlation between observed and generated time series
   
   cc_mes <- CCGamma(prec_mes[,they],sample = "monthly",origin = origin)
   cc_gen <- CCGamma(prec_gen[,they],sample = "monthly",origin = origin)
   
   
   # 
   cc_mes$`1`$nooccurrence_correlation
   cc_mes$`2`$nooccurrence_correlation
   cc_mes$`3`$nooccurrence_correlation
   cc_mes$`4`$nooccurrence_correlation
   cc_mes$`5`$nooccurrence_correlation
   cc_mes$`6`$nooccurrence_correlation
   cc_mes$`7`$nooccurrence_correlation
   cc_mes$`8`$nooccurrence_correlation
   cc_mes$`9`$nooccurrence_correlation
   cc_mes$`10`$nooccurrence_correlation
   cc_mes$`11`$nooccurrence_correlation
   cc_mes$`12`$nooccurrence_correlation
   
   cc_gen$`1`$nooccurrence_correlation
   cc_gen$`2`$nooccurrence_correlation
   cc_gen$`3`$nooccurrence_correlation
   cc_gen$`4`$nooccurrence_correlation
   cc_gen$`5`$nooccurrence_correlation
   cc_gen$`6`$nooccurrence_correlation
   cc_gen$`7`$nooccurrence_correlation
   cc_gen$`8`$nooccurrence_correlation
   cc_gen$`9`$nooccurrence_correlation
   cc_gen$`10`$nooccurrence_correlation
   cc_gen$`11`$nooccurrence_correlation
   cc_gen$`12`$nooccurrence_correlation
   
   
   
   # Modifying Precipitation Distribution (e.g. Precipitation Futture Projection)
   
   
   df <- data.frame(obs=prec_mes[,they],gen=prec_gen[,they])
   df$date <- as.Date(origin)+days(1:nrow(df))-1
   df$month <- factor(month(df$date))
   df$season <- "none"
   df$season[df$month %in% c(12,2,1)] <- "1.DJF"
   df$season[df$month %in% c(3,4,5)] <-  "2.MAM"
   df$season[df$month %in% c(6,7,8)] <-  "3.JJA"
   df$season[df$month %in% c(9,10,11)] <-  "4.SON"
   
   names(df)
   
   # ---- station T36133
   df_T36133 <- df[df$obs.T36133>valmin, c("obs.T36133","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_T36133 <- ggplot(data=df_T36133)+
     geom_density(aes(x=obs.T36133,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T36133.png")
   show(gdens_T36133)
   
   # ---- station T36137
   df_obs.T36137 <- df[df$obs.T36137>valmin, c("obs.T36137","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T36137 <- ggplot(data=df_obs.T36137)+
     geom_density(aes(x=obs.T36137,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T36133.png")
   show(gdens_obs.T36137)
   
   
   
   # ---- station T44040
   df_obs.T44040 <- df[df$obs.T44040>valmin, c("obs.T44040","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T44040 <- ggplot(data=df_obs.T44040)+
     geom_density(aes(x=obs.T44040,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T44040.png")
   show(gdens_obs.T44040)
   
   # ---- station T44120
   df_obs.T44120 <- df[df$obs.T44120>valmin, c("obs.T44120","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T44120 <- ggplot(data=df_obs.T44120)+
     geom_density(aes(x=obs.T44120,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T44120.png")
   show(gdens_obs.T44120)
   
   
   # ---- station T45000
   df_obs.T45000 <- df[df$obs.T45000>valmin, c("obs.T45000","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45000 <- ggplot(data=df_obs.T45000)+
     geom_density(aes(x=obs.T45000,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45000.png")
   show(gdens_obs.T45000)
   
   
   # ---- station T45001
   df_obs.T45001 <- df[df$obs.T45001>valmin, c("obs.T45001","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45001 <- ggplot(data=df_obs.T45001)+
     geom_density(aes(x=obs.T45001,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45001.png")
   show(gdens_obs.T45001)
   
   
   # ---- station T45002
   df_obs.T45002 <- df[df$obs.T45002>valmin, c("obs.T45002","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45002 <- ggplot(data=df_obs.T45002)+
     geom_density(aes(x=obs.T45002,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45002.png")
   show(gdens_obs.T45002)
   
   # ---- station T45003
   df_obs.T45003 <- df[df$obs.T45003>valmin, c("obs.T45003","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45003 <- ggplot(data=df_obs.T45003)+
     geom_density(aes(x=obs.T45003,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45003.png")
   show(gdens_obs.T45003)
   
   # ---- station T45004
   df_obs.T45004 <- df[df$obs.T45004>valmin, c("obs.T45004","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45004 <- ggplot(data=df_obs.T45004)+
     geom_density(aes(x=obs.T45004,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45004.png")
   show(gdens_obs.T45004)
   
   
   # ---- station T45007
   df_obs.T45007 <- df[df$obs.T45007>valmin, c("obs.T45007","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T45007 <- ggplot(data=df_obs.T45007)+
     geom_density(aes(x=obs.T45007,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_T45007.png")
   show(gdens_obs.T45007)
   
   
   
   
   
   
   #install.packages("lmom")
   
   #library(lmom)
   
   #prec_val_m_T36137 <- df_obs.T36137$obs.T36137 %>% split(df_obs.T36137$month) 
   #lmoms <- prec_val_m_T36137 %>% lapply(FUN=samlmu) 
   
   #params <- lmoms %>% lapply(FUN=pelln3,bound=valmin) 
   #kstest <-params %>% mapply(FUN=ks.test,x=prec_val_m_T36137,y="cdfln3",SIMPLIFY=FALSE) 
   #kstest
#}

