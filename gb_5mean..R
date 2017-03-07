setwd("~//Desktop/GB")

require("plyr")
require("dplyr")
require("reshape2")
require("data.table")
ss <- fread("sample_submission.csv")
ss

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

##install.packages("drat", repos="https://cran.rstudio.com")
##drat:::addRepo("dmlc")
##install.packages("mxnet")

#transform target variable to log(1 + demand) - this makes sense since we're 
#trying to minimize rmsle and the mean minimizes rmse:
h$log_demand = log1p(h$demand) 

#set a table key to enable fast aggregations
setkey(h, pid, cid, routeid)
setkey(t, pid, cid, routeid)

print("Computing means")
mean_total <- mean(h$demand) #overall mean
mean_Prod <-  h[, .(mean_prod = mean(log_demand)), 
                by = .(pid)]
mean_Prod_route <- h[, .(mean_prod_route = mean(log_demand)),
                     by = .(pid, routeid)]          #mean by product and ruta
mean_Cid_pid_agencyid <- h[, .(mean_CPA = mean(log_demand)),
                           by = .(pid, cid, agencyid)]      #mean by product, client, agencia


print("Merging means with test set")
submit <- merge(t, mean_Cid_pid_agencyid, all.x = TRUE, by = c("pid", "cid", "agencyid"))
submit <- merge(submit, mean_Prod_route, all.x = TRUE, by = c("pid", "routeid"))
submit <- merge(submit, mean_Prod, all.x = TRUE, by = "pid")
head(submit)
#use cid+pid+routid combo if found, otherwise use route+product and if not use product.
preds = submit$mean_CPA
preds[is.na(preds)] = submit$mean_prod_route[is.na(preds)]
preds[is.na(preds)] = submit$mean_prod[is.na(preds)]
preds[is.na(preds)] = mean_total
preds

#transform the preds back - scaling the preds worked better in some scripts 
#so it made me curious altought it doesn't make too much sense mathematically.
##preds = expm1(preds)*0.94 subbg4 gave me 728 on leaderboard
preds = expm1(preds)*0.93
preds
submission = data.frame(id = submit$id, Demanda_uni_equil = preds)
write.csv(submission, "submission_gb5.csv", row.names = FALSE)
