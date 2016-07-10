source("Setup_datafiles.R")

head(trnew)
head(tstnew)
head(test)

head(trnew)
attach(trnew)

trnew$Dependent<-as.factor(as.character(trnew$Dependent))
trnew$Category<-NULL

str(trnew)

grid <- expand.grid(mtry = c(120,140, 160, 180, 188, 200, 210, 220, 240, 260, 280, 300))
rfFit <- train(Dependent~.,data=trnew,method="rf",
               trControl=trainControl(method="repeatedcv",number=5,repeats=5),tuneGrid = grid)
plot(rfFit)

varImp(rfFit)

newrfFit <- train(Dependent~Company_senior_team_count+Company_business_model+Company_avg_investment_time+Founders_skills_score+Founders_Domain_skills_score+Company_investor_count_seed+Founders_Entrepreneurship_skills_score+Founders_Marketing_skills_score,data=trnew,method="rf",
               trControl=trainControl(method="repeatedcv",number=10,repeats=5),tuneGrid = grid)

plot(newrfFit)

pred_CAX2<- predict(newrfFit, newdata=test, type="prob")
submit_CAX<- cbind(test$CAX_ID,pred_CAX)
colnames(submit_CAX)<- c("CAX_ID", "Dependent")
write.csv(submit_CAX,"rfPredictions.csv",row.names=F)

pred_CAX2[,2]


glmFit <- train(Dependent~Company_senior_team_count+Company_business_model+Company_avg_investment_time+Founders_skills_score+Founders_Domain_skills_score+Company_investor_count_seed+Founders_Entrepreneurship_skills_score+Founders_Marketing_skills_score,data=trnew,method="LogitBoost", 
                  trControl=trainControl(method="repeatedcv",number=5,repeats=5))

plot(glmFit)

bglmFit <- train(Dependent~Company_senior_team_count+Company_business_model+Company_avg_investment_time+Founders_skills_score+Founders_Domain_skills_score+Company_investor_count_seed+Founders_Entrepreneurship_skills_score+Founders_Marketing_skills_score,data=trnew,method="bayesglm", 
                trControl=trainControl(method="repeatedcv",number=5,repeats=5))

bglmFit

lmmod<-glm(Dependent~Company_senior_team_count+Company_business_model+Company_avg_investment_time+Founders_skills_score+Founders_Domain_skills_score+Company_investor_count_seed+Founders_Entrepreneurship_skills_score+Founders_Marketing_skills_score,family = binomial(link=logit),data = trnew)

pred_CAX<- predict(lmmod, newdata=test, type="response")
submit_CAX<- data.frame(test$CAX_ID,pred_CAX)
colnames(submit_CAX)<- c("CAX_ID", "Dependent")
write.csv(submit_CAX,"lm4Predictions.csv",row.names=F)

pred_CAX=(pred_CAX+pred_CAX2[,2])/2
