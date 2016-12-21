setwd("D:/data/科研中心/方法论实践/java/input")
source("D:/data/科研中心/方法论实践/R/findOutliers.R")
source("D:/data/科研中心/方法论实践/R/exportTables.R")
source("D:/data/科研中心/方法论实践/R/readData.R")
library(lubridate)

tablename <- "逸盛石化销售.txt"
desname <- "调研数据-PTA1.txt"


##### findOutliers
test_findOutliers <- function(tablename, desname){
cols  <- "数量"
a1 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=1, ws=c(1,2,3))

cols  <- c("数量","金额")
a2 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=1, ws=c(1,2,3))

cols  <- c("数量","金额")
a3 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=2, ws=c(1,2,3))

cols  <- c("数量","金额")
a4 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=3, ws=c(1,2))
}


#### exportTables
test_exportTables <- function(tablename, desname){
total <- list(list(c("数量"),c("d1"),c("sum")))
res <- exportTables(tablename, desname, td="month", colt= "日期", ids="客户", total=total)

total <- list(list(c("数量"),c("d1"),c("sum")))
res <- exportTables(tablename, desname, td="month", colt= "日期",ids=c("客户","订单"), total=total)

total <- list(list(c("数量"),c("d1"),c("sum")),list(c("金额"),c("d2"),c("sum")))
res <- exportTables(tablename, desname, td="month", colt= "日期",ids=c("客户","订单"), total=total)

total <- list(list(c("数量","金额"),c("d1","d2"),c("sum")))
res <- exportTables(tablename, desname, td="month", colt= "日期", ids=c("客户","订单"), total=total)

total <- list(list(c("数量","金额"),c("d1","d2","d3","d4"),c("sum","mean")))
res <- exportTables(tablename, desname, td="month", colt= "日期", ids=c("客户","订单"), total=total)

total <- list(list(c("数量"),c("d1","d2"),c("sum","mean")))
res <- exportTables(tablename, desname, td="month", colt= "日期", ids=c("客户","订单"), total=total)

total <- list(list(c("数量"),c("d1","d2"),c("sum","mean")))
res <- exportTables(tablename, desname, td="month", colt= "日期", ids=c("客户"), total=total)
}


#### readData
tablename <- "hengyi_erp"
desname <- ""
test_readData <- function(tablename, desname){
        
        get <- c("rowNum", "colNum", "colName", "colType")
        res <- readData(tablename, get=get, para=0)
                
        get <- "getCol"
        res <- readData(tablename, get=get, para=c(1,2,3,4))
        
        get <- "getCol"
        res <- readData(tablename, get=get, para=2)

        get <- "getRow"
        res <- readData(tablename, get=get, para=c(10,20))
        
}

