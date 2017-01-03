configF <- function(protype1, data0, attr){
       
        proConf1 <- proConfig(protype1)
        para <- getModel_Para(proConf1, protype1)
        input <- getInput(para, data0, attr, protype1)
        
        ## running
        protype <- c("Classification","Clustering","findOutliers","priorRules","rating2","tsAnalysis1","Regression", "exportTables") 
        if(protype1=="Classification") res <- Classification(xtrain,ytrain,kcv=1,xpred=NULL,plot=FALSE, med="RF")
        
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
        
        input
}

