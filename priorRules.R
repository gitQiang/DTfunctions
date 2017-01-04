priorRules <- function(data0, med="apriori", min_sup=0.1, min_conf=0.5, maxlen=3, plot=FALSE){
        ## Inputs:
        ## data0: data matrix
        ## med: "apriori", "eclat"
        ## plot: plot or not
        
        ## Outputs:
        ## fit: fitted model
        
        ## 加载需要的程序包
        library(arules)
        if(typeof(data0)!="S4") data0 <- as(as.matrix(data0), "transactions")
        
        if(med=="apriori") rulesR <- apriori(data0,parameter=list(support=min_sup,confidence=min_conf,minlen=2,maxlen=maxlen))
       
        if(med=="eclat") rulesR <- eclat(data0,parameter=list(support=min_sup,minlen=2,maxlen=maxlen))  
                
        if(plot){
                ##查看data0前三位产品的支持度（稀疏矩阵中商品排列）
                plot(density(itemFrequency(data0)))
                
                ##显示data0中支持度至少为10%的几类商品
                itemFrequencyPlot(data0, support=0.1)
                
                ##显示data0中支持度前20的商品
                itemFrequencyPlot(data0, topN=20)
                
                ##显示随机抽样100次交易的稀疏矩阵
                image(data0[sample(min(100,nrow(data0))), ])
        }
        
        rulesR
}

