library(RMAWGEN)

# ---- Percipitation ----
rm(list = ls())
load('BullooModInp.RData')


TX_CLIMATE <- NULL 
TN_CLIMATE <- NULL 
PREC_CLIMATE <- NULL 

# Calibration period 
year_max <- 2018
year_min <- 1989
origin <- "1989-1-1"
station <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001", "T45002", "T45003", "T45004", "T45007")
names(ELEVATION) <- STATION_NAMES
names(LOCATION) <- STATION_NAMES

ELEVATION[station]
LOCATION[station]

n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5

p_test <- 1
p_prec <- 3

exogen <- NULL
exogen_sim <- exogen

generationP03GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                              prec_all=PRECIPITATION,
                                                              year_min=year_min,
                                                              year_max=year_max,
                                                              p=p_prec,
                                                              n_GPCA_iteration=n_GPCA_iter,
                                                              n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                              exogen=exogen,
                                                              exogen_sim=exogen_sim,
                                                              sample="monthly",
                                                              mean_climate_prec=PREC_CLIMATE,
                                                              no_spline=FALSE)

generationP01GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                              prec_all=PRECIPITATION,
                                                              year_min=year_min,
                                                              year_max=year_max,
                                                              p=p_test,
                                                              n_GPCA_iteration=n_GPCA_iter,
                                                              n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                              exogen=exogen,
                                                              exogen_sim=exogen_sim,
                                                              sample="monthly",
                                                              mean_climate_prec=PREC_CLIMATE,
                                                              no_spline=FALSE)


generationP03_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                          prec_all=PRECIPITATION,
                                                          year_min=year_min,
                                                          year_max=year_max,
                                                          p=p_prec,
                                                          n_GPCA_iteration=0,
                                                          n_GPCA_iteration_residuals=0,
                                                          exogen=exogen,
                                                          exogen_sim=exogen_sim,
                                                          sample="monthly",
                                                          mean_climate_prec=PREC_CLIMATE,
                                                          no_spline=FALSE)

generationP01_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                          prec_all=PRECIPITATION,
                                                          year_min=year_min,
                                                          year_max=year_max,
                                                          p=p_test,
                                                          n_GPCA_iteration=0,
                                                          n_GPCA_iteration_residuals=0,
                                                          exogen=exogen,
                                                          exogen_sim=exogen_sim,
                                                          sample="monthly",
                                                          mean_climate_prec=PREC_CLIMATE,
                                                          no_spline=FALSE)

# VAR select 

VARselect(generationP03_prec$data_prec,lag.max=20) 
VARselect(generationP03GPCA_prec$var@GPCA_data$final_results,lag.max=20)


normality_test(generationP01_prec$var)
normality_test(generationP03_prec$var)
normality_test(generationP01GPCA_prec$var)
normality_test(generationP03GPCA_prec$var)


serial_test(generationP01_prec$var)
serial_test(generationP03_prec$var)
serial_test(generationP01GPCA_prec$var)
serial_test(generationP03GPCA_prec$var)


# Collecting the measured and generated time series 
prec_mes <- generationP01_prec$prec_mes

prec_gen <- list(P03GPCA=generationP03GPCA_prec$prec_gen,
                 P01GPCA=generationP01GPCA_prec$prec_gen,
                 P03=generationP03GPCA_prec$prec_gen,
                 P01=generationP01GPCA_prec$prec_gen)


# season 
NDAY <- nrow(prec_mes)	
days <- list()
days$DJF <-  extractmonths(data=1:NDAY,when=c("Dec","Jan","Feb"),origin=origin)
days$MAM <- extractmonths(data=1:NDAY,when=c("Mar","Apr","May"),origin=origin)
days$JJA <- extractmonths(data=1:NDAY,when=c("Jun","Jul","Aug"),origin=origin)
days$SON <- extractmonths(data=1:NDAY,when=c("Sep","Oct","Nov"),origin=origin)	  


# SET THE CORRECT PATH WHERE TO PLOT THE FIGURES 
wpath <- getwd()
CEX <- 1.4
station00 <- station <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001",
                          "T45002", "T45003", "T45004", "T45007")
for (stat in station00){
  print(stat)
  for (it in names(days)) {
    str(it)
    name <- it
    season <- days[[it]]
    lag <- 1
    pdf_prec <- paste(wpath,"/QQpLots/",stat,"prec_qqplot_",lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
    
    main_prec  <- paste(names(prec_gen),stat,'lag',lag,"days",it,sep=" ")
    Rangex <- range(prec_mes[season,])
    Rangey <- range(c(prec_gen[[1]][season,],prec_gen[[2]][season,],prec_gen[[3]][season,],prec_gen[[4]][season,]))
    XLim <- YLim  <- c(min(c(Rangex,Rangey)),max(c(Rangex,Rangey)))
    
    qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=stat,when=season,pdf=pdf_prec,
                        lag=lag,cex.main=CEX,cex.lab=CEX,cex.axis=CEX,xlim=XLim,ylim=YLim,cex=1.2)
    
    
  }
  
  
  for (it in names(days)){
    str(it)
    name <- it
    season <- days[[it]]
    
    lag <- 2
    pdf_prec <- paste(wpath,"/QQplots/",stat,"prec_qqplot_",'lag',lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
    main_prec  <- paste(names(prec_gen),stat,lag,"days",it,sep=" ")
    
    qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=stat,when=season,pdf=pdf_prec,
                        lag=lag,xlim=XLim,cex.main=CEX,cex.lab=CEX,cex.axis=CEX,ylim=YLim,cex=1.2)
  }
}


# ACF Function 
pdf(paste(wpath,"QQplots/acf_prec_P03GPCA.pdf",sep="/"))
plot(acf(prec_gen$P03GPCA,lag=10),xlab="lag [day]")
dev.off()
pdf(paste(wpath,"QQplots/acf_prec_mes.pdf",sep="/"))
plot(acf(prec_mes,lag=10))
dev.off()

#---- Temperature ----

TN_CLIMATE <- NULL
TX_CLIMATE <- NULL

# Calibration period 
year_max <- 2018
year_min <- 1989
origin <- "1989-1-1"
station <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001", "T45002", "T45003", "T45004", "T45007")
n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5
p_test <- 1
p_temp <- 10


exogen <- normalizeGaussian_severalstations(x=prec_mes,
                                            data=prec_mes,
                                            sample="monthly", origin_x=origin,
                                            origin_data=origin, step=0)

exogen_sim <- normalizeGaussian_severalstations(x=prec_gen$P03GPCA,
                                                data=prec_gen$P03GPCA,
                                                sample="monthly",
                                                origin_x=origin,
                                                origin_data=origin,step=0)


# order p = 10 and PCA Gaussianization
generationP10GPCA_temp <- ComparemprehensiveTemperatureGenerator(station=station, 
                                                            Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,
                                                            year_min=year_min, year_max=year_max,p=p_temp,
                                                            n_GPCA_iteration=n_GPCA_iter,
                                                            n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                            exogen=exogen,exogen_sim=exogen_sim,sample="monthly",
                                                            mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

# order p = 1 and PCA Gaussianization
generationP01GPCA_temp <- ComprehensiveTemperatureGenerator(station=station, 
                                                            Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,
                                                            year_min=year_min, year_max=year_max,p=p_test,
                                                            n_GPCA_iteration=n_GPCA_iter,
                                                            n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                            exogen=exogen,exogen_sim=exogen_sim,sample="monthly",
                                                            mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
# order p = 10 and without PCA Gaussianization
generationP10_temp <- ComprehensiveTemperatureGenerator(station=station,
                                                        Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,
                                                        year_min=year_min,year_max=year_max,p=p_temp,
                                                        n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,
                                                        exogen=exogen,exogen_sim=exogen_sim,sample="monthly",
                                                        mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
# order p = 1 and without PCA Gaussianization
generationP01_temp <- ComprehensiveTemperatureGenerator(station=station,
                                                         Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,
                                                         year_min=year_min,year_max=year_max,p=p_test,
                                                         n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,
                                                         exogen=exogen,exogen_sim=exogen_sim,sample="monthly",
                                                         mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

# Normality and serial Test
serial_test(generationP10GPCA_temp$var)
serial_test(generationP01GPCA_temp$var)
serial_test(generationP10_temp$var)
serial_test(generationP01_temp$var)

normality_test(generationP10GPCA_temp$var)
normality_test(generationP01GPCA_temp$var)
normality_test(generationP10_temp$var)
normality_test(generationP01_temp$var)

# Season

Tx_gen <- list(P01_temp = generationP01_temp$output$Tx_gen,
               P10_temp = generationP10_temp$output$Tx_gen,
               P01GPCA_temp = generationP01GPCA_temp$output$Tx_gen,
               P10GPCA_temp = generationP10GPCA_temp$output$Tx_gen
)

Tn_gen <- list(P01_temp = generationP01_temp$output$Tn_gen,
               P10_temp = generationP10_temp$output$Tn_gen,
               P01GPCA_temp = generationP01GPCA_temp$output$Tn_gen,
               P10GPCA_temp = generationP10GPCA_temp$output$Tn_gen
)


#Tx_gen <- generationP01_temp$output$Tx_gen
#Tn_gen <- generationP01_temp$output$Tn_gen

Tx_mes <- generationP01_temp$input$Tx_mes
Tn_mes <- generationP01_temp$input$Tn_mes


NDAY <- nrow(Tx_mes)	
days <- list()
days$DJF <-  extractmonths(data=1:NDAY,when=c("Dec","Jan","Feb"),origin=origin)
days$MAM <- extractmonths(data=1:NDAY,when=c("Mar","Apr","May"),origin=origin)
days$JJA <- extractmonths(data=1:NDAY,when=c("Jun","Jul","Aug"),origin=origin)
days$SON <- extractmonths(data=1:NDAY,when=c("Sep","Oct","Nov"),origin=origin)	  



wpath <- getwd()
station00 <- c("T36133", "T36137", "T44040", "T44120", "T45000", "T45001",
               "T45002", "T45003", "T45004", "T45007")
CEX <- 1.4
for (stat in station00){
  print(stat)
  for (it in names(days)) {
    str(it)
    name <- it
    season <- days[[it]]
    
    pdf_temp <- paste(wpath,"/QQplots/",stat,"_Tx_qqplot","_",year_min,"_",year_max,"_",it,".pdf",sep="")
    
    main_temp  <- paste(names(Tx_gen),stat,"days",it,sep=" ")
    
    Rangex <- range(Tx_mes[season,])
    Rangey <- range(c(Tx_gen[[1]][season,],Tx_gen[[2]][season,],Tx_gen[[3]][season,],Tx_gen[[4]][season,]))
    
    XLim <- YLim  <- c(min(c(Rangex,Rangey)),max(c(Rangex,Rangey)))
    
    qqplot_RMAWGEN_Tx(Tx_mes = Tx_mes, Tx_gen = Tx_gen, Tn_gen = Tn_gen, Tn_mes = Tn_mes, Tx_spline = NULL,
                    Tn_spline = NULL, main = main_temp, station = stat, when = season, pdf = pdf_temp,
                    cex.main = CEX, cex.lab = CEX,cex.axis=CEX,xlim=XLim,ylim=YLim,cex=1.2)
  }
  
  for (it in names(days)) {
    str(it)
    name <- it
    season <- days[[it]]
    
    pdf_temp <- paste(wpath,"/QQplots/",stat,"_Tn_qqplot","_",year_min,"_",year_max,"_",it,".pdf",sep="")
    
    main_temp  <- paste(names(Tn_gen),stat,"days",it,sep=" ")
    
    Rangex <- range(Tx_mes[season,])
    Rangey <- range(c(Tn_gen[[1]][season,],Tn_gen[[2]][season,],Tn_gen[[3]][season,],Tn_gen[[4]][season,]))
    
    XLim <- YLim  <- c(min(c(Rangex,Rangey)),max(c(Rangex,Rangey)))
    
    qqplot_RMAWGEN_Tn(Tx_mes = Tx_mes, Tx_gen = Tx_gen, Tn_gen = Tn_gen, Tn_mes = Tn_mes, Tx_spline = NULL,
                      Tn_spline = NULL, main = main_temp, station = stat, when = season, pdf = pdf_temp,
                      cex.main = CEX, cex.lab = CEX,cex.axis=CEX,xlim=XLim,ylim=YLim,cex=1.2)
  }
}

