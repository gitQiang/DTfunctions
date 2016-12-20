exportTables <- function(tablename, desname, td=NULL, ids, total){
        ## tablename: table name; ex: 调研数据-PTA.csv
        ## desname: described file; ex: 调研数据-PTA.csvmeta
        ## td: time dimension, that is, day, week, month, season and year
        ## ids: conserved ids
        ## total: orders of sumup in different ways, ex: list(ins=c("数量"), ons=c("numS"), fs="sum")
        
 
        ## input data from table or database ======= need to normalize
        data0 <- read.delim(tablename)
        data0 <- data0[!is.na(data0[,1]), ]
        des <- read.table(desname,skip=4,header=FALSE,sep=":")
        data0[,"日期"] <- gsub("\\.","-",data0[,"日期"])
        
        ### del columns and rows
        subids <- which(des[,2]=="标识")
        conservedM <- sapply(subids, function(ii) data0[,ii] %in% ids )
        keepSub <- setdiff(1:ncol(data0), subids)
        colSub <- apply(conservedM, 2, any)
        #rowSub <- apply(conservedM, 1, any)
        rowSub <- apply(conservedM[ ,colSub, drop=FALSE], 1, all)
        keepSub <- c(subids[colSub], keepSub)
        subTable <- data0[rowSub, keepSub]
        
        ## sum up by total
        if(is.null(td)){
                subExport <- sapply(1:length(total[[3]]), function(ii) apply(subTable[ ,total[[1]], drop=FALSE], 2,  total[[3]][ii]) )
                subExport <- as.matrix(subExport)
                rownames(subExport) <- total[[1]]
        }else{
                subT <- which(des[keepSub,2]=="时间标签")[1]  #!!!!!
                useDates <- subTable[ ,subT]
                subTable <- subTable[order(as.Date(useDates)), ]
                useDates <- subTable[ ,subT]
                
                useg <- dateGroup(useDates, di=td)
                subExport <- sapply(1:length(total[[1]]), function(ii) aggregate(subTable[,total[[1]][ii]], by=list(useg), total[[3]][ii])[,2] )
                rownames(subExport) <- aggregate(subTable[,total[[1]][1]], by=list(useg), total[[3]][1])[,1]
                
                tmpsub <- match(rownames(subExport), useg)
                subExport <- subExport[order(tmpsub), ,drop=FALSE]
        }
        colnames(subExport) <- total[[2]]

        list(subTable=subTable, subExport=subExport)      
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