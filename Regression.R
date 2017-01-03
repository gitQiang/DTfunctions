Regression <- function(data0,ylab,med, plot=FALSE){
        ## Inputs:
        ## data0: data matrix
        ## med: "lm", "glm", "logis"
        ## plot: plot or not
        
        ## Outputs:
        ## fit: fitted model
        
        data0 <- as.data.frame(data0)
        formu <- paste(ylab, " ~ .", sep="")
        if(med=="lm"){
                fit <- lm(formu, data=data0)
        }
        
        if(med=="glm"){
                fit <- glm(formu, data=data0)
        }
        
        if(med=="logis"){
                labs <- unique(data0[,ylab])
                data0[data0[,ylab]==labs[1], ylab] <- 0
                data0[data0[,ylab]==labs[2], ylab] <- 1
                fit <- glm(formu, data=data0, family='binomial')
                fit <- list(fit=fit, label=cbind(labs,c(0,1)))
        }
        
        fit
}

