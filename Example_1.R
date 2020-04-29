RMAWGEN_EXAMPLE <- function(){
	SelCatch <- c('Brisbane','Bulloo','Burdekin','CooperCreek','Fitzroy','Johnstone',
	              'LoganAlbert','SouthCoast','Pine','Warrego') 
	i10 <- 2 # select the number of your catchment
	subDir <- SelCatch[i10]
	#mainDir <- paste("E:/Fiverr/Checks/Rishikasharm941/ExampleProjectRISHIKA") #change this to the directory where you have stored the file 
	
	#setwd(mainDir) #change this to the directory where you have stored the file
	#save(ELEVATION,LOCATION,PRECIPITATION,PRECIPITATION_MEASUREMENT_END_DAY,
	# PRECIPITATION_MEASUREMENT_START_DAY,STATION_LATLON,STATION_NAMES,TEMPERATURE_MAX,
	# TEMPERATURE_MEASUREMENT_END_DAY,TEMPERATURE_MEASUREMENT_START_DAY,
	# TEMPERATURE_MIN,CatStations,CatBoundary,BuffArea,file=paste(subDir,'ModInp.RData',sep=''))  
	
	
	load(file=paste(subDir,'ModInp.RData',sep=''))
	#load(file=paste(subDir,'.RData',sep=''))
	CatStations <- CatStations[CatStations$Available==1,]
	plot(BuffArea,typ='l',asp=1,xlab='longitute',ylab='latitude')
	lines(CatBoundary,col='red')
	with(CatStations,points(lon,lat))
	#
	###############
	   library(RMAWGEN)
	#
	TX_CLIMATE <- NULL 
	TN_CLIMATE <- NULL 
	PREC_CLIMATE <- NULL 

	# Calibration period 
	year_max <- 1990
	year_min <- 1961
	origin <- "1961-1-1"
	station <- c("T45002", "T45003", "T45004")
	names(ELEVATION) <- STATION_NAMES
	names(LOCATION) <- STATION_NAMES
	
	ELEVATION[station]
	LOCATION[station]
	

	# Simulation period (Stochastic Generation)
	# MONTHLY CLIMATOLOGY

	# specific parameter for model calibration 

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
	station00 <- "T45002"
	CEX <- 1.4

	for (it in names(days)) {
	  str(it)
  	name <- it
  	season <- days[[it]]
  	lag <- 1
  	main_prec  <- paste("prec",names(prec_gen),station00,lag,"days",it,sep=" ")
  	filename =paste(name,lag,".pdf",sep = "")
  	pdf(filename)
  	qqplot_RMAWGEN_prec(prec_mes=prec_mes,
  	                    prec_gen=prec_gen,
  	                    main=main_prec,
  	                    station=station00,
  	                    when=season,
  	                    lag=lag,
  	                    cex.main=CEX,
  	                    cex.lab=CEX,
  	                    cex.axis=CEX)
  	dev.off()
  	#readline()
	}

  for(it in names(days)){
    str(it)
    name <- it
    sason <- days[[it]]
    
    lag <- 2
    main_prec  <- paste("prec",names(prec_gen),station00,lag,"days",it,sep=" ")
    
    filename =paste(name,lag,".pdf",sep = "")
    pdf(filename)
    qqplot_RMAWGEN_prec(prec_mes=prec_mes,
                        prec_gen=prec_gen,
                        main=main_prec,
                        station=station00,
                        when=season,
                        lag=lag,
                        xlim=range(prec_mes)*lag,
                        cex.main=CEX,
                        cex.lab=CEX,
                        cex.axis=CEX)
    dev.off()
    #readline()
  }

	# ACF Function 
	pdf(paste(wpath,"acf_prec_P03GPCA.pdf",sep="/"))
	plot(acf(prec_gen$P03GPCA,lag=10),xlab="lag [day]")
	dev.off()
	pdf(paste(wpath,"acf_prec_mes.pdf",sep="/"))
	plot(acf(prec_mes,lag=10))
	dev.off()

} #RMAWGEN_EXAMPLE

RGENERATEPREC_EXAMPLE <- function(){ 
	library(RGENERATEPREC)
	rm(list=ls())
	SelCatch <- c('Brisbane','Bulloo','Burdekin','CooperCreek','Fitzroy','Johnstone',
	'LoganAlbert','SouthCoast','Pine','Warrego') 
	i10 <- 9 # select the number of your catchment
	subDir <- SelCatch[i10]
	mainDir <- paste("E:/Fiverr/Checks/Rishikasharm941/rishikasharm941-attachments")  #change this to the directory where you have stored the file 
	setwd(mainDir) 
	#save(ELEVATION,LOCATION,PRECIPITATION,PRECIPITATION_MEASUREMENT_END_DAY,
	# PRECIPITATION_MEASUREMENT_START_DAY,STATION_LATLON,STATION_NAMES,TEMPERATURE_MAX,
	# TEMPERATURE_MEASUREMENT_END_DAY,TEMPERATURE_MEASUREMENT_START_DAY,
	# TEMPERATURE_MIN,CatStations,CatBoundary,BuffArea,file=paste(subDir,'ModInp.RData',sep=''))  
	load(file=paste(subDir,'ModInp.RData',sep=''))
	CatStations <- CatStations[CatStations$Available==1,]
	plot(BuffArea,typ='l',asp=1,xlab='longitute',ylab='latitude')
	lines(CatBoundary,col='red')
	with(CatStations,points(lon,lat))
	#
	###############

	year_min <- 1961
	year_max <- 1990

	origin <- paste(year_min,1,1,sep="-")
	end <- paste(year_max,12,31,sep="-")

	period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
	period_temp <- TEMPERATURE_MAX$year>=year_min & TEMPERATURE_MAX$year<=year_max

	prec_mes <- PRECIPITATION[period,]
	Tx_mes <- TEMPERATURE_MAX[period_temp,]
	Tn_mes <- TEMPERATURE_MIN[period_temp,]
	accepted <- array(TRUE,length(names(prec_mes)))
	names(accepted) <- names(prec_mes)
	for (it in names(prec_mes)) {
		acc <- TRUE
		acc <- (length(which(!is.na(Tx_mes[,it])))==length(Tx_mes[,it]))
		acc <- (length(which(!is.na(Tn_mes[,it])))==length(Tn_mes[,it])) & acc
		accepted[it]  <- (length(which(!is.na(prec_mes[,it])))==length(prec_mes[,it])) & acc
		
	}

	valmin <- 1.0
	prec_mes <- prec_mes[,accepted]

	Tx_mes <- Tx_mes[,accepted]
	Tn_mes <- Tn_mes[,accepted]
	prec_occurrence_mes <- prec_mes>=valmin

	station <- names(prec_mes)[!(names(prec_mes) %in% c("day","month","year"))]
	it <- station[2]
	vect <- Tx_mes[,it]-Tn_mes[,it]
	months <- factor(prec_mes$month)

	#

	model <-
			PrecipitationOccurrenceModel(x=prec_mes[,it],exogen=vect,
					monthly.factor=months,valmin=valmin)
	#
	obs <- prec_mes[,it]>=valmin
	#
	gen <- generate(model,exogen=vect,monthly.factor=months,n=length(months))


	### MultiSite Generation


	station <- station[1:2]
	exogen <- Tx_mes[,station]-Tn_mes[,station]

	months <- factor(prec_mes$month)


	model_multisite <-
			PrecipitationOccurrenceMultiSiteModel(x=prec_mes[,station],
					exogen=exogen,origin=origin,multisite_type="wilks")

	#
	## LOGIT-type Model
	model_multisite_logit <-
			PrecipitationOccurrenceMultiSiteModel(x=prec_mes,exogen=exogen,
					origin=origin,multisite_type="logit",station=station)
	#
	#
	obs_multisite <- prec_mes[,station]>=valmin
	#
	gen_multisite <- generate(model_multisite,exogen=exogen,origin=origin,end=end)
	#
	gen_multisite_logit <- generate(model_multisite_logit,exogen=exogen,origin=origin,end=end)


	#amount
	precamount <- PrecipitationAmountModel(prec_mes,station=station,origin=origin)
	val <- predict(precamount)
	prec_gen <- generate(precamount)
	month <- adddate(as.data.frame(residuals(precamount[[1]])),origin=origin)$month 
	plot(factor(month),residuals(precamount[[1]]))
	qqplot(prec_mes[,station[1]],prec_gen[[1]])
	abline(0,1)

} # RGENERATEPREC_EXAMPLE
