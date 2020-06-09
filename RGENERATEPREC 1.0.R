
#RGeneratePrecFunction <- function(){
  rm(list=ls())
  
  library(tidyverse)
  library(lubridate)
  library(reshape2)
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
    
  
  
  str(prec_mes)
  
  str(prec_gen)
  
  
  df <- list(obs=prec_mes[,names(prec_gen)],gen=prec_gen)
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
   
   qqdw <- split(dw,f=paste(dw$spell_state,dw$station,dw$season,sep="_")) %>% 
      lapply(FUN=function(x){list(obs=x$spell_length[x$variable=="obs"],gen=x$spell_length[x$variable=="gen"])}) %>% 
      lapply(FUN=qqplot_) %>% melt(id=c("obs","gen"))
   
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
   df_obs.T36133 <- df[df$obs.T36133>valmin, c("obs.T36133","date","month", "season")] 
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   
   gdens_obs.T36133 <- ggplot(data=df_obs.T36133)+
     geom_density(aes(x=obs.T36133,color=month,group=month),trim=TRUE)+
     facet_grid(season ~ .)+
     theme_bw() + 
     scale_color_manual(values = color_month)
   ggsave("gdens_obs.T36133.png")
   show(gdens_obs.T36133)
   
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
   
   library(lmom)
   
   
   
   # station T36133
   prec_val_m_T36133 <- df_obs.T36133$obs.T36133 %>% split(df_obs.T36133$month) 
   lmoms_T36133 <- prec_val_m_T36133 %>% lapply(FUN=samlmu) 
   
   params_T36133 <- lmoms_T36133 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T36133 <-params_T36133 %>% mapply(FUN=ks.test,x=prec_val_m_T36133,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T36133
   
   
   
   
   
   # station T36137
   prec_val_m_T36137 <- df_obs.T36137$obs.T36137 %>% split(df_obs.T36137$month) 
   lmoms_T36137 <- prec_val_m_T36137 %>% lapply(FUN=samlmu) 
   
   params_T36137 <- lmoms_T36137 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T36137 <-params_T36137 %>% mapply(FUN=ks.test,x=prec_val_m_T36137,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T36137
   
   
   # station T44040
   prec_val_m_T44040 <- df_obs.T44040$obs.T44040 %>% split(df_obs.T44040$month) 
   lmoms_T44040 <- prec_val_m_T44040 %>% lapply(FUN=samlmu) 
   
   params_T44040 <- lmoms_T44040 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T44040 <-params_T44040 %>% mapply(FUN=ks.test,x=prec_val_m_T44040,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T44040
   
   
   
   # station T44120
   prec_val_m_T44120 <- df_obs.T44120$obs.T44120 %>% split(df_obs.T44120$month) 
   lmoms_T44120 <- prec_val_m_T44120 %>% lapply(FUN=samlmu) 
   
   params_T44120 <- lmoms_T44120 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T44120 <-params_T44120 %>% mapply(FUN=ks.test,x=prec_val_m_T44120,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T44120
   
   
   # station T45000
   prec_val_m_T45000 <- df_obs.T45000$obs.T45000 %>% split(df_obs.T45000$month) 
   lmoms_T45000 <- prec_val_m_T45000 %>% lapply(FUN=samlmu) 
   
   params_T45000 <- lmoms_T45000 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45000 <-params_T45000 %>% mapply(FUN=ks.test,x=prec_val_m_T45000,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45000
   
   
   # station T45001
   prec_val_m_T45001 <- df_obs.T45001$obs.T45001 %>% split(df_obs.T45001$month) 
   lmoms_T45001 <- prec_val_m_T45001 %>% lapply(FUN=samlmu) 
   
   params_T45001 <- lmoms_T45001 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45001 <-params_T45001 %>% mapply(FUN=ks.test,x=prec_val_m_T45001,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45001
   
   
   # station T45002
   prec_val_m_T45002 <- df_obs.T45002$obs.T45002 %>% split(df_obs.T45002$month) 
   lmoms_T45002 <- prec_val_m_T45002 %>% lapply(FUN=samlmu) 
   
   params_T45002 <- lmoms_T45002 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45002 <-params_T45002 %>% mapply(FUN=ks.test,x=prec_val_m_T45002,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45002
   
   
   # station T45003
   prec_val_m_T45003 <- df_obs.T45003$obs.T45003 %>% split(df_obs.T45003$month) 
   lmoms_T45003 <- prec_val_m_T45003 %>% lapply(FUN=samlmu) 
   
   params_T45003 <- lmoms_T45003 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45003 <-params_T45003 %>% mapply(FUN=ks.test,x=prec_val_m_T45003,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45003
   
   
   # station T45004
   prec_val_m_T45004 <- df_obs.T45004$obs.T45004 %>% split(df_obs.T45004$month) 
   lmoms_T45004 <- prec_val_m_T36137 %>% lapply(FUN=samlmu) 
   
   params_T45004 <- lmoms_T45004 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45004 <-params_T45004 %>% mapply(FUN=ks.test,x=prec_val_m_T45004,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45004
   
   
   # station T45007
   prec_val_m_T45007 <- df_obs.T45007$obs.T45007 %>% split(df_obs.T45007$month) 
   lmoms_T45007 <- prec_val_m_T45007 %>% lapply(FUN=samlmu) 
   
   params_T45007 <- lmoms_T45007 %>% lapply(FUN=pelln3,bound=valmin) 
   kstest_T45007 <-params_T45007 %>% mapply(FUN=ks.test,x=prec_val_m_T45007,y="cdfln3",SIMPLIFY=FALSE) 
   kstest_T45007
   
   
   
   
   
   
   
   modify_lmoments <- function(x){x[2] <- x[2]*1.3; return(x)}
   paraml <- function(x,valmin) {
      ip <- which(x>=valmin)
      x <- x[ip] 
      out <- list()
      lmomx <- samlmu(x)
      out$obs <- pelln3(lmom=lmomx,bound=valmin)
      lmomx_m <- modify_lmoments(lmomx)
      out$mod  <- pelln3(lmom=lmomx_m,bound=valmin)
      return(out)
   }
   
   modify_distribution <- function(x,paraml,valmin){
      
      out <- x
      ip <- which(x>=valmin)
      out[ip] <- x[ip] %>% cdfln3(para=paraml$obs) %>% qualn3(para=paraml$mod)
      return(out)
      
   }
   
   
   # station T36133
   para_monthly_T36133 <- df_obs.T36133$obs.T36133 %>% split(df_obs.T36133$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T36133$bs_mod <- df_obs.T36133$obs.T36133
   
   for (mo in unique(df_obs.T36133$month))  {
      im <- which(df_obs.T36133$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T36133$obs.T36133s[im],paraml=para_monthly_T36133[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T36133 <- ggplot()+
      geom_line(data=df_obs.T36133,mapping=aes(x=obs.T36133,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T36133 <- gg_T36133 +scale_color_manual(values = color_month)
   ggsave("gg_T36133.png", width = 8)
   show(gg_T36133)
   
   
   
   
   # station T36137
   para_monthly_T36137 <- df_obs.T36137$obs.T36137 %>% split(df_obs.T36137$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T36137$bs_mod <- df_obs.T36137$obs.T36137
   
   for (mo in unique(df_obs.T36137$month))  {
      im <- which(df_obs.T36137$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T36137$obs.T36137s[im],paraml=para_monthly_T36137[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T36137 <- ggplot()+
      geom_line(data=df_obs.T36137,mapping=aes(x=obs.T36137,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T36137 <- gg_T36137 +scale_color_manual(values = color_month)
   ggsave("gg_T36137.png", width = 8)
   show(gg_T36137)
   
   
   
   # station T44040
   para_monthly_T44040 <- df_obs.T44040$obs.T44040 %>% split(df_obs.T44040$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T44040$bs_mod <- df_obs.T44040$obs.T44040
   
   for (mo in unique(df_obs.T44040$month))  {
      im <- which(df_obs.T44040$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T44040$obs.T44040s[im],paraml=para_monthly_T44040[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T44040 <- ggplot()+
      geom_line(data=df_obs.T44040,mapping=aes(x=obs.T44040,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T44040 <- gg_T44040 +scale_color_manual(values = color_month)
   ggsave("gg_T44040.png", width = 8)
   show(gg_T44040)
   
   
   # station T44120
   para_monthly_T44120 <- df_obs.T44120$obs.T44120 %>% split(df_obs.T44120$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T44120$bs_mod <- df_obs.T44120$obs.T44120
   
   for (mo in unique(df_obs.T44120$month))  {
      im <- which(df_obs.T44120$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T44120$obs.T44120s[im],paraml=para_monthly_T44120[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T44120 <- ggplot()+
      geom_line(data=df_obs.T44120,mapping=aes(x=obs.T44120,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T44120 <- gg_T44120 +scale_color_manual(values = color_month)
   ggsave("gg_T44120.png", width = 8)
   show(gg_T44120)
   
   
   # station T45000
   para_monthly_T45000 <- df_obs.T45000$obs.T45000 %>% split(df_obs.T45000$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45000$bs_mod <- df_obs.T45000$obs.T45000
   
   for (mo in unique(df_obs.T45000$month))  {
      im <- which(df_obs.T45000$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45000$obs.T45000s[im],paraml=para_monthly_T45000[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45000 <- ggplot()+
      geom_line(data=df_obs.T45000,mapping=aes(x=obs.T45000,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45000 <- gg_T45000 +scale_color_manual(values = color_month)
   ggsave("gg_T45000.png", width = 8)
   show(gg_T45000)
   
   
   # station T45001
   para_monthly_T45001 <- df_obs.T45001$obs.T45001 %>% split(df_obs.T45001$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45001$bs_mod <- df_obs.T45001$obs.T45001
   
   for (mo in unique(df_obs.T45001$month))  {
      im <- which(df_obs.T45001$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45001$obs.T45001s[im],paraml=para_monthly_T45001[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45001 <- ggplot()+
      geom_line(data=df_obs.T45001,mapping=aes(x=obs.T45001,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45001 <- gg_T45001 +scale_color_manual(values = color_month)
   ggsave("gg_T45001.png", width = 8)
   show(gg_T45001)
   
   
   # station T45002
   para_monthly_T45002 <- df_obs.T45002$obs.T45002 %>% split(df_obs.T45002$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45002$bs_mod <- df_obs.T45002$obs.T45002
   
   for (mo in unique(df_obs.T45002$month))  {
      im <- which(df_obs.T45002$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45002$obs.T45002s[im],paraml=para_monthly_T45002[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45002 <- ggplot()+
      geom_line(data=df_obs.T45002,mapping=aes(x=obs.T45002,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45002 <- gg_T45002 +scale_color_manual(values = color_month)
   ggsave("gg_T45002.png", width = 8)
   show(gg_T45002)
   
   
   # station T45003
   para_monthly_T45003 <- df_obs.T45003$obs.T45003 %>% split(df_obs.T45003$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45003$bs_mod <- df_obs.T45003$obs.T45003
   
   for (mo in unique(df_obs.T45003$month))  {
      im <- which(df_obs.T45003$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45003$obs.T45003s[im],paraml=para_monthly_T45003[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45003 <- ggplot()+
      geom_line(data=df_obs.T45003,mapping=aes(x=obs.T45003,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45003 <- gg_T45003 +scale_color_manual(values = color_month)
   ggsave("gg_T45003.png", width = 8)
   show(gg_T45003)
   
   
   # station T45004
   para_monthly_T45004 <- df_obs.T45004$obs.T45004 %>% split(df_obs.T45004$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45004$bs_mod <- df_obs.T45004$obs.T45004
   
   for (mo in unique(df_obs.T45004$month))  {
      im <- which(df_obs.T45004$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45004$obs.T45004s[im],paraml=para_monthly_T45004[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45004 <- ggplot()+
      geom_line(data=df_obs.T45004,mapping=aes(x=obs.T45004,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45004 <- gg_T45004 +scale_color_manual(values = color_month)
   ggsave("gg_T45004.png", width = 8)
   show(gg_T45004)
   
   
   # station T45007
   para_monthly_T45007 <- df_obs.T45007$obs.T45007 %>% split(df_obs.T45007$month) %>% lapply(FUN=paraml,valmin=valmin)
   df_obs.T45007$bs_mod <- df_obs.T45007$obs.T45007
   
   for (mo in unique(df_obs.T45007$month))  {
      im <- which(df_obs.T45007$month==mo)
      
      df$obs_mod[im] <- modify_distribution(x=df_obs.T45007$obs.T45007s[im],paraml=para_monthly_T45007[[mo]],valmin=valmin)
   }
   
   color_month <-  rep(c("#E6352F","#34A74B","#3D79F3" ),4)[c(12,1:11)]
   gg_T45007 <- ggplot()+
      geom_line(data=df_obs.T45007,mapping=aes(x=obs.T45007,y=bs_mod,col=month))+facet_grid(. ~ season)+theme_bw()
   gg_T45007 <- gg_T45007 +scale_color_manual(values = color_month)
   ggsave("gg_T45007.png", width = 8)
   show(gg_T45007)
   
   
   
   
   
   
   
   
#}

