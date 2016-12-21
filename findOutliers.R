findOutliers <- function(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=1, ws){
        ## tablename: table name; ex: 调研数据-PTA.csv
        ## desname: described file; ex: 调研数据-PTA.csvmeta
        ## cols: colnames for outlier analysis
        ## pcut: p-value cutoff to decide whether it is normal distribution or not
        ## pnor: outlier cutofff from normal distribution
        ## pqua: outlier cutoff from quantile function
        ## mod1: 1 sum; 2 max; 3; weighted; modes of outlier analysis based on multiple columns
        ## ws: weights of multiple columns
        
        
        ## input data from table or database ======= need to normalize
        data0 <- read.delim(tablename)
        data0 <- data0[!is.na(data0[,cols[1]]), ]
        
        
        
        
        library(nortest)
        ind <- c()
        if(nrow(data0) > 10){ ## if data point is smaller than 10, we don't do outlier analysis.
                n <- length(cols)
                if(n==1){
                        x1 <- as.numeric(data0[,cols])
                        tmp <- outn1(x1, pcut)
                        sc <- tmp$sc
                        ind <- tmp$ind
                        fg <- tmp$fg
                        
                        sc1 <- sc[ind]
                        data1 <- cbind(data0[ind, ],sc1)
                        if(fg==0) data1 <- data1[sc1 < pnor, ]
                        if(fg==1) data1 <- data1[sc1 < pqua, ]
                }
                
                if(n > 1){
                        scM <- sapply(1:n, function(ii) outn1(as.numeric(data0[,cols[ii]]), pcut)$sc )
                        if(mod==1) sc <- rowSums(scM)/ncol(scM)
                        if(mod==2) sc <- apply(scM, 1, min)       
                        if(mod==3) sc <- apply(scM, 1, weighted.mean, ws, na.rm=TRUE)
                        
                        ind <- sort(sc, index.return=TRUE)$ix
                        sc1 <- sc[ind]
                        data1 <- cbind(data0[ind, ],sc1)
                        data1 <- data1[sc1 < pqua, ]
                }
        }
        
        if(nrow(data0)!=length(ind)){
                data1 <- c()
        }
        
        data1
}

outn1 <- function(x1, pcut=0.05){
        
        fg <- 0
        p <- lillie.test(x1)$p.value
        if(p >= pcut){  ## normal distribution
                fg <- 0
                mu <- mean(x1)
                sig <- sd(x1)
                sc <- pnorm(x1,mu,sig)
        }else{
                n <- length(x1)
                sc <- sapply(x1, function(ii) sum(x1<=ii)/n)
                fg <- 1
        }
        sc[sc > 0.5] <- 1 -  sc[sc > 0.5]
        ind <- sort(sc, index.return=TRUE)$ix
        #sc1 <- sc[ind]
        
        list(sc=sc, ind=ind, fg=fg)        
}


