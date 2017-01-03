rating2 <- function(dt, wfile, NAnum=1, rate_range="overall"){
        
        library(data.table)
        library(rating)
        #1. clean and prepare data (or mannually compute the basic index here)
        # note that please convert the data.frame to data.table
        dt <- data.table(dt)
        
        #2. get the index system and its weight (ensure the definition is consistent with prepared data)
        # input: index system which is designed beforehand, fields formated as <pathString, weight, ...>
        weight <- getWeight(wfile)
        
        #3. compute the basic index (if the first step alread did, ignore this step)
        
        #4. standandlize the index according to the proper category
        # input: input data.table, index column, id column, category column, (both number or name are ok)
        # dt.index <- getStandlized(dt, c(3:10), 1, 2)
        dt.index <- dt
        
        #5. compute the derived index using proper aggragative model
        # input: index data, weight data, category colunm (can be ignored if no classification)
        dt.index <- getIndexSystem(dt.index, weight, NAnum)
        
        #6. get the rating results
        # input: index data, allowed na numbers (exceedance will not rate)
        dt.index <- getRatingResult(dt.index, rate_range)       

        dt.index       
}

