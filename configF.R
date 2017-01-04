configF <- function(protype1, data0, attr){
       
        protype <- c("Classification","Clustering","findOutliers","priorRules","rating2","tsAnalysis1","Regression", "exportTables") 
        if(is.numeric(protype1)) protype1 <- protype[protype1]
        
        if(protype1 %in% protype){
                proConf1 <- proConfig(protype1) 
                para <- getModel_Para(proConf1, protype1)
                input <- getInput(para, data0, attr, protype1)
                
                ## source functions
                source("Classification.R")
                source("Clustering.R")
                source("findOutliers.R")
                source("priorRules.R")
                source("rating2.R")
                source("tsAnalysis1.R")
                source("Regression.R")
                source("exportTables.R")
                
                ## running
                #if(protype1=="Classification") res <- Classification(input$xtrain,input$ytrain,input$kcv,input$xpred,input$plot, input$med)
                if(protype1=="Clustering") res <- Clustering(input$x, input$med, input$k, input$dis, input$plot)
                if(protype1=="findOutliers") res <- findOutliers(input$data0, input$cols, input$pcut, input$pnor, input$pqua, input$mod, input$ws)
                if(protype1=="priorRules") res <- priorRules(input$data0, input$med, input$min_sup, input$min_conf, input$maxlen, input$plot)
                if(protype1=="rating2") res <- rating2(input$dt, input$wfile, input$NAnum, input$rate_range)
                if(protype1=="tsAnalysis1") res <- tsAnalysis1(input$data0, input$cols, input$ic, input$plot)
                # if(protype1=="Regression") res <- Regression(data0,ylab,med, plot=FALSE)
                if(protype1=="exportTables") res <- exportTables(input$data0, input$td, input$colt, input$ids, input$total)
        }else{
                print("model type error!")
                res <- c()
        }
        
        res
}


proConfig <- function(protype1){
        
        protype <- c("Classification","Clustering","findOutliers","priorRules","rating2","tsAnalysis1","Regression", "exportTables") 
        proConf <- list()
        proConf$Classification <- c("xtrain", "ytrain", "kcv(optional)", "xpred(optional)", "plot(flag,optional)", "method(rpart, adaBoost, RF, svm, optional)")
        proConf$Clustering <- c("x: matrix of sample * feature", "method(hclust:default, kmeans)", "k for kmeans", "distance method", "plot(flag,optional)")
        proConf$findOutliers <- c("data0", "cols(optional)", "pcut=0.05", "pnor=0.05", "pqua=0.05", "mod=1", "ws")
        proConf$priorRules <- c("data0", "method(apriori:default)", "min_sup", "min_conf", "maxlen", "plot")
        proConf$rating2 <- c("data table", "wfile", "NAnum", "rate_range=")
        proConf$tsAnalysis1 <- c("data0", "cols", "ic(aic)", "plot")
        proConf$Regression <- c("data0","ylab", "med", "plot")
        proConf$exportTables <- c("data0", "td", "colt", "ids", "total")
        
        if(protype1 %in% protype){
                return(proConf[[which(protype1==protype)]])
        }else{
                print("Please choose one of the following models: Classification, Clustering, ...
                      findOutliers, priorRules, rating2, tsAnalysis1, Regression, exportTables") 
                return(NULL)
        }
}

getModel_Para <- function(proConf1, protype1){
        
        n <- length(proConf1)
        print(paste("Input parameters for ", protype1, sep=""))
        para <- list()
        for(i in 1:n){
                cat(paste("Please give the parameter: ", proConf1[i], sep=""))
                x <- readLines(con=stdin(), 1)
                para[[i]] <- x
        }
        
        para
}

getInput <- function(para, data0, attr, protype1){
        
        input <- list()
        if(protype1=="Classification"){
                input$xtrain <- data0[, para[[1]]]
                input$ytrain <- data0[, para[[2]]]
                input$kcv <- ifelse(para[[3]]=="", 1, as.numeric(para[[3]]))
                input$xpred <- ifelse(para[[4]]=="", NULL, data0[, para[[4]]])
                input$plot <- ifelse(as.numeric(para[[5]]) > 0, TRUE, FALSE)
                input$med <- ifelse(para[[6]]=="", "RF", para[[6]])
        }
        
        if(protype1=="Clustering"){
                input$x <- data0[, para[[1]]]
                input$med <- ifelse(para[[2]]=="", "hclust", para[[2]])
                input$k <- ifelse(para[[3]]=="", 3, as.numeric(para[[3]]))
                input$dis <- ifelse(para[[4]]=="", "euclidean", para[[4]])
                if(is.na(as.numeric(para[[5]]))){ input$plot <- FALSE
                }else{ input$plot <- ifelse(as.numeric(para[[5]]) > 0, TRUE, FALSE)}
        }
        
        if(protype1=="findOutliers"){
                if( para[[1]] %in% c("","all") ){ input$data0 <- data0
                }else{ input$data0 <- data0[, unlist(strsplit(para[[1]], " "))] }
                
                if(para[[2]]==""){input$cols <- NULL
                }else{ input$cols <- unlist(strsplit(para[[2]], " ")) }
                
                input$pcut <- ifelse(para[[3]]=="", 0.05, as.numeric(para[[3]]))
                input$pnor <- ifelse(para[[4]]=="", 0.05, as.numeric(para[[4]]))
                input$pqua <- ifelse(para[[5]]=="", 0.05, as.numeric(para[[5]]))
                input$mod <- ifelse(para[[6]]=="", 1, as.numeric(para[[6]]))
                
                if(para[[6]]==""){ input$ws <- ""
                }else{ input$ws  <- as.numeric(unlist(strsplit(para[[7]]," ")))}
        }
        
        if(protype1=="exportTables"){
                if( para[[1]] %in% c("","all") ){ input$data0 <- data0
                }else{ input$data0 <- data0[, unlist(strsplit(para[[1]], " "))] }
                
                input$td <- ifelse(para[[2]]=="", NULL, para[[2]])
                input$colt <- ifelse(para[[3]]=="", "", para[[3]])
                
                if(para[[4]]==""){input$ids <- ""
                }else{input$ids <- unlist(strsplit(para[[4]], "  *"))}
                
                if(para[[5]]==""){ input$total <- ""
                }else{ 
                        tmp <- unlist(strsplit(para[[5]], "  *"))
                        input$total <- list(list(tmp[1], tmp[2], tmp[3]))
                }
        }
        
        if(protype1=="priorRules"){
                if( para[[1]] %in% c("","all") ){ input$data0 <- data0
                }else{ input$data0 <- data0[, unlist(strsplit(para[[1]], "  *"))] }
                input$med <- ifelse(para[[2]]=="", "apriori", para[[2]])
                input$min_sup <- ifelse(para[[3]]=="", 0.05, as.numeric(para[[3]]))
                input$min_conf <- ifelse(para[[4]]=="", 0.05, as.numeric(para[[4]]))
                input$maxlen <- ifelse(para[[5]]=="", 3, as.numeric(para[[5]]))
                if(is.na(as.numeric(para[[6]]))){ input$plot <- FALSE
                }else{ input$plot <- ifelse(as.numeric(para[[6]]) > 0, TRUE, FALSE)}
        }
        
        if(protype1=="rating2"){
                if( para[[1]] %in% c("","all") ){ input$dt <- data0
                }else{ input$dt <- data0[, unlist(strsplit(para[[1]], "  *"))] }
                input$wfile <- ifelse(para[[2]]=="", "warning", para[[2]])
                input$NAnum <- ifelse(para[[3]]=="", 1, as.numeric(para[[3]]))
                input$rate_range <- ifelse(para[[4]]=="", "overall", para[[4]] )
        }
        
        if(protype1=="tsAnalysis1"){
                if( para[[1]] %in% c("","all") ){ input$data0 <- data0
                }else{ input$data0 <- data0[, unlist(strsplit(para[[1]], "  *"))] }
                input$cols <- ifelse(para[[2]]=="", "warning", para[[2]])
                input$ic <- ifelse(para[[3]]=="", "aic", para[[3]])
                if(is.na(as.numeric(para[[4]]))){ input$plot <- FALSE
                }else{ input$plot <- ifelse(as.numeric(para[[4]]) > 0, TRUE, FALSE)}
        }
        
        
        input
}


