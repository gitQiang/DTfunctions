readData <- function(tablename, get="getCol", para=0){
        ## get: orders include: rowNum, colNum, colName, colType, getCol, getRow
        ## para: int i used with getCol or getRow
        
        library(RMySQL)
        library(DBI)
        con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
        
        res <- list()
        for(j in 1:length(get)){
                if(get[j]=="rowNum") res[[j]] <- rowNum(tablename, con)  
                if(get[j]=="colNum") res[[j]] <- colNum(tablename, con)  
                if(get[j]=="colName") res[[j]] <- colName(tablename, con)  
                if(get[j]=="colType") res[[j]] <- colType(tablename, con)  
                if(get[j]=="getCol") res[[j]] <- getCol(tablename, con, para)  
                if(get[j]=="getRow") res[[j]] <- getRow(tablename, con, para)  
        }
        
        dbDisconnect(con)

        res
}

rowNum <- function(tablename, con){
        rs <- dbGetQuery(con, paste("SELECT count(*) FROM ", tablename, sep="") )
        unlist(rs)[1]
}

colNum <- function(tablename, con){
        rs <- dbGetQuery(con, paste("select count(*) from information_schema.columns where table_name='", tablename ,"'", sep="") )
        unlist(rs)[1]
}

colName <- function(tablename, con){
        rs <- dbGetQuery(con, paste("select COLUMN_NAME from information_schema.columns where table_name='", tablename ,"'", sep="") )
        unlist(rs)
}

colType <- function(tablename, con){
        rs <- dbGetQuery(con, paste("select COLUMN_TYPE from information_schema.columns where table_name='", tablename ,"'", sep="") ) 
        ##rs <- dbGetQuery(con, paste("select DATA_TYPE from information_schema.columns where table_name='", tablename ,"'", sep="") ) 
        unlist(rs)
}

getCol <- function(tablename, con, para){
        cols <- colName(tablename, con)
        if(para[1]==0){
                rs <- dbReadTable(con, tablename)
                colnames(rs) <- cols
        }else{
                rs <- dbGetQuery(con, paste("select ", paste(cols[para],sep="",collapse=","), " from ", tablename, sep="") ) 
                rs <- as.matrix(rs)
                colnames(rs) <- cols[para]
        }
        
        rs
}

getRow <- function(tablename, con, para){
        cols <- colName(tablename, con)
        if(para[1]==0){
                rs <- dbReadTable(con, tablename)
        }else{
                rs <- dbGetQuery(con, paste("select * from ", tablename, " limit ", para[1], ",", para[2]-para[1]+1, sep="") )
                rs <- as.matrix(rs)
        }  
        colnames(rs) <- cols
        
        rs
}

