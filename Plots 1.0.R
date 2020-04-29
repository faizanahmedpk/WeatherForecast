for (it in names(days)) {
    str(it)
    name <- it
    season <- days[[it]]
    lag <- 1
    pdf_prec <- paste(wpath,"/prec_qqplot_",lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
    
    main_prec  <- paste(names(prec_gen),station00,'lag',lag,"days",it,sep=" ")
    Rangex <- range(prec_mes[season,])
    Rangey <- range(c(prec_gen[[1]][season,],prec_gen[[2]][season,],prec_gen[[3]][season,],prec_gen[[4]][season,]))
    XLim <- YLim  <- c(min(c(Rangex,Rangey)),max(c(Rangex,Rangey)))
    
    qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=station00,when=season,#pdf=pdf_prec,
    lag=lag,cex.main=CEX,cex.lab=CEX,cex.axis=CEX,xlim=XLim,ylim=YLim,cex=1.2)
    
    x11()
    
    lag <- 2
    pdf_prec <- paste(wpath,"/prec_qqplot_",'lag',lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
    main_prec  <- paste(names(prec_gen),station00,lag,"days",it,sep=" ")

    qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=station00,when=season,#pdf=pdf_prec,

    lag=lag,xlim=XLim,cex.main=CEX,cex.lab=CEX,cex.axis=CEX,ylim=YLim,cex=1.2)
}
