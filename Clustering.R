Clustering <- function(x, med="hclust", k=3, dis="euclidean", plot=FALSE){
        ## Inputs:
        ## x: A numeric matrix of data
        ## med: "hclust", "kmeans", "knn"
        ## k: the number of clusters for kmeans and the number of neighbors for knn
        ## dis: distance method for kmeans or hclust
        ## plot: plot or not
        
        ## Outputs:
        ## fit: fitted model
       
        if(med=="hclust"){
                d <- dist(x, method = dis)
                fit <- hclust(d, method = "complete", members = NULL)
                if(plot) plot(fit)
        }
        
        if(med=="kmeans"){
                library(amap)        
                fit <- Kmeans(x, centers=k, iter.max = 100, nstart = 1, method = dis)
                if(plot) plot(x, col = fit$cluster)
        }
        
        if(med=="knn"){
                library(kknn) 
                fit <- 0
        }
        
        fit
}

