Classification <- function(xtrain,ytrain,kcv=1,xpred=NULL,plot=FALSE, med="RF"){
        ## Inputs:
        ## xtrain: training samples
        ## ytrain: training labels
        ## kcv: number of cross validation
        ## xpred: predicting samples (optional)
        ## plot: plot or not
        ## med: "RF": randomForest
        
        ## Outputs:
        ## model: fitted model
        ## cvr: cross validation results
        ## ypred: predicted labels
        
        
        #### Cross validation
        nsample <- nrow(xtrain)
        cvlist <- sample.cross(nsample, kcv)
        pV <- rep(0,nsample)
        for(kk in 1:kcv){
                ## train data
                trainsub <- cvlist$train[[kk]]
                xt <- xtrain[trainsub, ]
                yt <- ytrain[trainsub]
                
                if(med %in% c("RF","rpart")){
                        a1 <- data.frame(xt,yt)
                        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                        rownames(a1) <- 1:nrow(a1)
                }
                
                if(med %in% c("adaBoost","svm")){
                        yt <- as.character(yt)
                        options(stringsAsFactors = TRUE)
                        a1 <- data.frame(xt,yt)
                        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                        rownames(a1) <- 1:nrow(a1)  
                }
                
                predisub <- cvlist$pred[[kk]]
                newdata <- xtrain[predisub, ]
                colnames(newdata) <- paste("X",1:(ncol(a1)-1),sep="")
                
                ## one train and test
                if(med=="adaBoost"){
                        library(adabag)
                        fit <- boosting(Class ~.,data=data.frame(a1), coeflearn="Zhu")
                        preModel <- predict(fit,newdata)$prob[,2]
                }
                
                if(med=="rpart"){
                        library(rpart)
                        fit <- rpart(Class ~.,data=a1)
                        preModel <- predict(fit, newdata=as.data.frame(newdata))[ ,1]
                }
                
                if(med=="svm"){
                        library(e1071)
                        fit <- svm(Class ~., data=a1, probability = TRUE)
                        tmp <- predict(fit, newdata,probability = TRUE)
                        preModel <- attr(tmp,"probabilities")[ ,1]
                }
                
                if(med=="RF"){
                        library(randomForest)
                        fit <- randomForest(x=a1[,1:(ncol(a1)-1)],y=as.factor(a1[,"Class"])) ### RandomForest method
                        preModel <- predict(fit,as.data.frame(newdata),type="prob")[,2]
                }
                
                pV[predisub] <- preModel
        }
        
        #### model assessment
        library(ROCR)
        pred <- prediction( pV, ytrain)
        perf <- performance(pred,"tpr","fpr")
        if(plot) plot(perf)
        prf <- performance(pred,"prec","rec")
        if(plot) plot(prf)
        aucf <- performance(pred,"auc")@y.values[[1]]
        aceR <- list(perf=perf, prf=prf, auc=aucf)
        
        ### training and predicting 
        xt <- xtrain
        yt <- ytrain
        
        if(med %in% c("RF","rpart")){
                a1 <- data.frame(xt,yt)
                colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                rownames(a1) <- 1:nrow(a1)
        }
        if(med %in% c("adaBoost","svm")){
                yt <- as.character(yt)
                options(stringsAsFactors = TRUE)
                a1 <- data.frame(xt,yt)
                colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                rownames(a1) <- 1:nrow(a1)  
        }
        
        
        ## one train and test
        if(is.null(xpred)){
                predM <- c()        
        }else{
                newdata <- xpred
                colnames(newdata) <- paste("X",1:(ncol(a1)-1),sep="")
                
                if(med=="adaBoost"){
                        fit <- boosting(Class ~.,data=data.frame(a1), coeflearn="Zhu")
                        predM <- predict(fit,newdata)
                }
                
                if(med=="rpart"){
                        fit <- rpart(Class ~.,data=a1)
                        predM <- predict(fit, newdata=as.data.frame(newdata))
                }
                
                if(med=="svm"){
                        fit <- svm(Class ~., data=a1, probability = TRUE)
                        predM <- predict(fit, newdata, probability = TRUE)
                }
                
                if(med=="RF"){
                        fit <- randomForest(x=a1[,1:(ncol(a1)-1)],y=as.factor(a1[,"Class"])) ### RandomForest method
                        predM <- predict(fit,as.data.frame(newdata),type="prob")
                }
        
        }
        
        list(pV=pV, aceR=aceR, fit=fit, predM=predM)

}


sample.cross <- function(nsample, K){
        
        train_sample <- list()
        pred_sample <- list()
        
        nk <- floor(nsample/K)
        sam <- sample(nsample)
        
        for(i in 1:K){
                pred_sample[[i]] <- sam[((i-1)*nk+1): min(i*nk, nsample)]
                train_sample[[i]] <- setdiff(sam,pred_sample[[i]])
        }
        
        list(train=train_sample,pred=pred_sample)
}
