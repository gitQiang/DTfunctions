priorRules <- function(data0, med="apriori", min_sup=0.1, min_conf=0.5, maxlen=3, plot=FALSE){
        ## Inputs:
        ## data0: data matrix
        ## med: "apriori", "eclat"
        ## plot: plot or not
        
        ## Outputs:
        ## fit: fitted model
        
        ## ������Ҫ�ĳ����
        library(arules)

        if(med=="apriori") rulesR <- apriori(data0,parameter=list(support=min_sup,confidence=min_conf,minlen=2,maxlen=maxlen))
       
        if(med=="eclat") rulesR <- eclat(data0,parameter=list(support=min_sup,minlen=2,maxlen=maxlen))  
                
        if(plot){
                ##�鿴data0ǰ��λ��Ʒ��֧�ֶȣ�ϡ���������Ʒ���У�
                plot(density(itemFrequency(data0)))
                
                ##��ʾdata0��֧�ֶ�����Ϊ10%�ļ�����Ʒ
                itemFrequencyPlot(data0, support=0.1)
                
                ##��ʾdata0��֧�ֶ�ǰ20����Ʒ
                itemFrequencyPlot(data0, topN=20)
                
                ##��ʾ�������100�ν��׵�ϡ�����
                image(data0[sample(min(100,nrow(data0))), ])
        }
        
        rulesR
}
