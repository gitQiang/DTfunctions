tsAnalysis1 <- function(data0, cols, ic="aic", plot=FALSE){
        ## data0: data table;
        ## cols: colnames for time series analysis;
        ## ic: information criterion to be used in model selection;
        ## plot: whether plot or not;
        
        library(forecast) 
        x <- as.numeric(data0[,cols])
        fit <- auto.arima(x, ic=ic)
        if(plot) plot(forecast(fit,h=1))
        resP <- sapply( c(5,10,15), function(ii) Box.test(residuals(fit), lag=ii, type="Ljung-Box", fitdf=length(fit$coef))$p.value )
        
        list(fit=fit, resP=resP)
}

