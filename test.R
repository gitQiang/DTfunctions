setwd("D:/data/科研中心/方法论实践/java/input")
source("D:/data/科研中心/方法论实践/R/findOutliers.R")
source("D:/data/科研中心/方法论实践/R/exportTables.R")

tablename <- "逸盛石化销售.txt"
desname <- "调研数据-PTA1.txt"


##### findOutliers
cols  <- "数量"
a1 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=1, ws=c(1,2,3))

cols  <- c("数量","金额")
a2 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=1, ws=c(1,2,3))

cols  <- c("数量","金额")
a3 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=2, ws=c(1,2,3))

cols  <- c("数量","金额")
a4 <- findOutliers(tablename, desname, cols, pcut=0.05, pnor=0.05, pqua=0.05, mod=3, ws=c(1,2))

#### exportTables
res <- exportTables(tablename, desname, td=NULL, ids="A000", total=list(ins="数量",ons="dd",fs="sum"))

res <- exportTables(tablename, desname, td="month", ids=c("A000","306346","308189","306312"), total=list(ins="数量",ons="dd",fs="sum"))

res <- exportTables(tablename, desname, td="month", ids=c("A000","306346","308189","306312"), total=list(ins=c("数量","金额"),ons=c("sd1","sd2"),fs=c("sum","sum")) )


