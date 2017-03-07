library(data.table)
setwd("~//Desktop/GB")

require("plyr")
require("dplyr")
require("reshape2")
require("data.table")

client <- fread("cliente_tabla.csv", header = T, stringsAsFactors = F)
summary(client)
prod <- fread("producto_tabla.csv", header = T, stringsAsFactors = F)
head(prod, 20)
townst<-fread("town_state.csv", header = T, stringsAsFactors = F)
townst
# Train data
h <- fread("train.csv", header=T, stringsAsFactors=F)
names(h)
names(h) <- c("week", "agencyid", "channelid", "routeid", "cid", "pid", "weekunits", "weeksales",
              "returnunits", "returnsales", "demand")
# Test data
t <- fread("test.csv", header=T, stringsAsFactors=F)
names(t)
names(t) <- c("id", "week", "agencyid", "channelid", "routeid", "cid", "pid")

#== making sbset for train
## select = c('cid', 'pid', 'agencyid', 'routeid', 'demand')
h1 <- subset(h, select = c('cid', 'pid', 'agencyid', 'routeid', 'demand'))

## making subset for test
##  select = c('id', 'cid', 'pid', 'agencyid', 'routeid')

t1 <- subset(t, select = c('id', 'cid', 'pid', 'agencyid', 'routeid'))

## -----Computing means-----
#transform target variable to log(1 + demand) -  as we're 
#trying to minimize rmsle and the mean minimizes rmse:
h1$log_demand = log1p(h1$demand) 
mean_total <- mean(h1$log_demand) # overall mean
mean-total
#mean by product
mean_P <-  h1[, .(MP = mean(log_demand)), by = .(pid)]
#mean by product and routeid
mean_PR <- h1[, .(MPR = mean(log_demand)), by = .(pid, routeid)] 
#mean by product, client, agencyia
mean_PCA <- h1[, .(MPCA = mean(log_demand)), by = .(pid, cid, agencyid)]

## Merging means with test set
submit <- merge(t1, mean_PCA, all.x = TRUE, by = c('pid', 'cid', 'agencyid'))
submit <- merge(submit, mean_PR, all.x = TRUE, by = c("pid", "routeid"))
submit <- merge(submit, mean_P, all.x = TRUE, by = "pid")
submit
# Now create Predictions column;
submit$Pred <- expm1(submit$MPCA)*0.725+expm1(submit$MPR)*0.195+0.086
submit[is.na(Pred)]$Pred <- expm1(submit[is.na(Pred)]$MPR)*0.748+0.168
submit[is.na(Pred)]$Pred <- expm1(submit[is.na(Pred)]$MP)*0.761+1.485
submit[is.na(Pred)]$Pred <- expm1(mean_total)*1.0705

submit$Pred <- round(submit$Pred,3)

##Write out submission file
# now relabel columns ready for creatig submission
setnames(submit,"Pred","Demanda_uni_equil")
# Any results you write to the current directory are saved as output.
write.csv(submit[,.(id,Demanda_uni_equil)],"submit_means_LB_gb8.csv", row.names = FALSE)
## end

submit
ss <- read.csv("submit_means_LB_gb8.csv")
ss(head())
head(ss, 50)
ss
expm1(submit$MPCA)
