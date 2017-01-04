exportTables <- function(data0, td=NULL, colt, ids, total){
        ## td: time dimension, that is, day, week, month, season and year
        ## colt: colnames of time or date column
        ## ids: conserved ids
        ## total: orders of sumup in different ways, ex: list(ins=c("ÊýÁ¿"), ons=c("numS"), fs="sum")
        
        if(is.null(td)){
                uniDate <- NULL        
        }else{
                useDates <- data0[ ,colt]
                useg <- dateGroup(useDates, di=td)
                uniDate <- unique(useg)
        }
        
        if(length(ids)==1){
                cLabs <- data0[ ,ids]
        }else{
                cLabs <- apply(data0[ ,ids], 1, paste, sep="", collapse="_")       
        }
        #uniLab <- unique(cLabs)
        
        res <- list()
        for(i in 1:length(total)){
                
                oneOrder <- total[[i]]
                subExport <- c()
                
                if(is.null(uniDate)){
                        subTable <- data0
                        oneLab <- cLabs
                        
                        tmp <- c()
                        for(k in 1:length(oneOrder[[3]])){
                                oneR <- aggregate(subTable[ ,oneOrder[[1]]], by=list(oneLab), oneOrder[[3]][k])[ ,-1]
                                tmp <- cbind(tmp,as.matrix(oneR))
                        }
                        oneR <- aggregate(subTable[ ,oneOrder[[1]]], by=list(oneLab), oneOrder[[3]][1])[ ,1]
                        tmp <- cbind(subTable[match(oneR, oneLab), ids], tmp)
                        rownames(tmp) <- paste(uniDate[j], 1:length(oneR), sep=":")
                        subExport <- tmp
                }else{
                        for(j in 1:length(uniDate)){
                                subTable <- data0[(useg %in% uniDate[j]),  ]
                                oneLab <- cLabs[useg %in% uniDate[j]]
                                
                                tmp <- c()
                                for(k in 1:length(oneOrder[[3]])){
                                        oneR <- aggregate(subTable[ ,oneOrder[[1]]], by=list(oneLab), oneOrder[[3]][k])[ ,-1]
                                        tmp <- cbind(tmp,as.matrix(oneR))
                                }
                                oneR <- aggregate(subTable[ ,oneOrder[[1]]], by=list(oneLab), oneOrder[[3]][1])[ ,1]
                                tmp <- cbind(subTable[match(oneR, oneLab), ids], tmp)
                                rownames(tmp) <- paste(uniDate[j], 1:length(oneR), sep=":")
                                subExport <- rbind(subExport,tmp)
                        }
                }
                
                colnames(subExport) <- c(ids, oneOrder[[2]])
                res[[i]] <- subExport
        }
        
        res
 
}

dateGroup <- function(useDates, di="week"){
        
        useDates <- as.Date(useDates)
        if(di=="day"){
                useg <- paste(year(useDates), month(useDates), day(useDates), sep="-")
        }else if(di=="week"){
                useg <- paste(year(useDates),week(useDates),sep="-")
        }else if(di=="month"){
                useg <- paste(year(useDates),month(useDates),sep="-")
        }else if(di=="season"){
                useg <- paste(year(useDates),quarter(useDates),sep="-")     
        }else if(di=="year"){
                useg <- year(useDates)
        }
        
        useg
}

