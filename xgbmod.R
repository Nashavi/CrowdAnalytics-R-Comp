source("Setup_datafiles.R")

trnew$Category<-NULL
tstnew$Category<-NULL

###xgb
library(xgboost)
library(data.table)
library(Matrix)

response<-trnew$Dependent
trnew<-trnew[,-c(1,2)]
trnew<- data.table(trnew,keep.rownames = F)
trainmat<- sparse.model.matrix(~.-1, data = trnew)
dtrnew<- xgb.DMatrix(data = trainmat, label = response)


eresponse<-tstnew$Dependent
tstnew<-tstnew[,-c(1,2)]
tstnew<- data.table(tstnew,keep.rownames = F)
testmat<- sparse.model.matrix(~.-1, data = tstnew)
dtstnew <- xgb.DMatrix(data = testmat, label = eresponse)

eval_watchlist <- list(eval=dtstnew,train=dtrnew)


xgbmod <- xgb.train(data=dtrnew
                 , max.depth=10
                 , eta= .01
                 , nthread = 2
                 , nround=5000
                 , objective =  "binary:logistic" #"multi:softmax"
                 , eval_metric= "auc"
                 , watchlist=eval_watchlist
                 , num_parallel_tree = 1000
                 #, num_class=2
                 #, base_score=.9
                 , min_child_weight = 10
                 , early.stop.round = 10
                 , subsample=.5
                 , lambda=.6
                 , colsample_bylevel=0.6
)


submit_CAX<-as.data.frame(test$CAX_ID)

test$Category<-NULL
test$Dependent<-NULL
test$CAX_ID<-NULL

preds<- data.table(test,keep.rownames = F)
predsmat <- sparse.model.matrix(~.-1, data = preds)
dpreds <- xgb.DMatrix(data = predsmat)
preds

pred_CAX2<- predict(xgbmod, dpreds)
submit_CAX$Dependent<- pred_CAX2
colnames(submit_CAX)<- c("CAX_ID", "Dependent")
write.csv(submit_CAX,"xgbPredictions4.csv",row.names=F)
