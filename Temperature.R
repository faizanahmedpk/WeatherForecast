
library(RMAWGEN)


TN_CLIMATE <- NULL
TX_CLIMATE <- NULL

year_max <- 1990
year_min <- 1961
origin <- "1961-1-1"
station <- c("T45002","T45003")
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
generationP10GPCA_temp <- ComprehensiveTemperatureGenerator(station=station, 
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


#Tx_gen <- generationP01_temp$output$Tx_gen
Tn_gen <- generationP01_temp$output$Tn_gen

Tx_mes <- generationP01_temp$input$Tx_mes
Tn_mes <- generationP01_temp$input$Tn_mes


NDAY <- nrow(Tx_mes)	
days <- list()
days$DJF <-  extractmonths(data=1:NDAY,when=c("Dec","Jan","Feb"),origin=origin)
days$MAM <- extractmonths(data=1:NDAY,when=c("Mar","Apr","May"),origin=origin)
days$JJA <- extractmonths(data=1:NDAY,when=c("Jun","Jul","Aug"),origin=origin)
days$SON <- extractmonths(data=1:NDAY,when=c("Sep","Oct","Nov"),origin=origin)	  



wpath <- getwd()
station00 <- "T45002"
CEX <- 1.4
for (it in names(days)) {
  str(it)
  name <- it
  season <- days[[it]]
  
  pdf_temp <- paste(wpath,"/Tx_qqplot_","_",year_min,"_",year_max,"_",it,".pdf",sep="")
  
  main_temp  <- paste(names(Tx_gen),station00,"days",it,sep=" ")
  Rangex <- range(Tx_mes[season,])
  Rangey <- range(c(Tx_gen[[1]][season,],Tx_gen[[2]][season,],Tx_gen[[3]][season,],Tx_gen[[4]][season,]))
  XLim <- YLim  <- c(min(c(Rangex,Rangey)),max(c(Rangex,Rangey)))
  
  qqplot_RMAWGEN_Tx(Tx_mes = Tx_mes, Tx_gen = Tx_gen, Tn_gen = Tn_gen, Tn_mes = Tn_mes, Tx_spline = NULL,
                    Tn_spline = NULL, main = main_temp, station = station00, when = season, pdf = pdf_temp,
                    cex.main = CEX, cex.lab = CEX,cex.axis=CEX,xlim=XLim,ylim=YLim,cex=1.2)
  

}
