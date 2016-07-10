source("Setup_datafiles.R")

# install developer tool and then woe package from gitub
# refer http://www.r-bloggers.com/r-credit-scoring-woe-information-value-in-woe- package/
#install.packages("devtools")
library(devtools)
#install_github("tomasgreif/woe")
library(woe)
# only dependent and independent variable of training set # should be there in data frame
trnew$CAX_ID<-NULL
# calculation of information value
IV<-iv.mult(trnew,y="Dependent",TRUE)
# Ignore warnig message for variable which have 0 WOE # (anyway you will remove these before modeling)

# selecting variables with 0.1<IV<0.5
var<-IV[which(IV$InformationValue>0.1),]
var1<-var[which(var$InformationValue<0.5),]
final_var<-var1$Variable

x_train<-train_new[final_var]
Dependent<-train$Dependent
train_final<-cbind(Dependent,x_train)


trnew$Category<-NULL
mod<-step(glm(Dependent~., family = binomial(link=logit),data = trnew))
summary(mod)

# Prediction on test set
pred_prob<-predict (mod, newdata=tstnew, type="response")
# model accuracy measures
library (ROCR)
pred <- prediction (pred_prob, tstnew$Dependent) # Area under the curve
performance (pred, 'auc')
# creating ROC curve
roc <- performance (pred,"tpr","fpr")
plot (roc)

library(SDMTools)
confusion.matrix (tstnew$Dependent, pred_prob, threshold = 0.42)

pred_CAX<- predict(mod, newdata=test, type="response")
submit_CAX<- cbind(test$CAX_ID,pred_CAX)
colnames(submit_CAX)<- c("CAX_ID", "Dependent")
write.csv(submit_CAX,"Predictions.csv",row.names=F)
