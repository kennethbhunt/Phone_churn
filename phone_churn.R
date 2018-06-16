phone <- read.csv("phone.csv", stringsAsFactors = F)
View(phone)

library(ggplot2)

ggplot(phone, aes(x=age, y=income)) + geom_point() +
  geom_smooth(method=lm)+ labs(title= "Age Vs. Income") + xlab("Age") +
  ylab("Income")
ggplot(phone, aes(churn)) + geom_bar(fill="red") +
  labs(title="Phone Company Sales Churn", subtitle="Class Imbalance Problem", caption="Kenneth B. Hunt, MBA", face="bold")
ggplot(phone, aes(x=age, y=tenure)) + geom_point() +
  geom_smooth(method=lm) + labs(title="Age Vs. Tenure") +xlab("Age") +
  ylab("Tenure")
sapply(phone, function(x) sum(is.na(x)))
str(phone)
phone$churn <- as.factor(phone$churn)
summary(phone)
prop.table(table(phone$churn))

ind <- sample(2, nrow(phone), replace=T, prob = c(0.7, 0.3))
training <- phone[ind==1,]
testing <- phone[ind==2, ]

#Data for developing predictive model 
table(training$churn)
prop.table(table(training$churn))
summary(training)

### Random Forest for predictive model 

library(randomForest)

rftrain <-randomForest(churn~., data=training)

#predictive model evaluation with testing data using caret

library(caret)

library(e1071)

confusionMatrix(predict(rftrain, testing), testing$churn, positive = '1')

##Oversampling for better sensitivity 
library(ROSE)

## ROSE - Randomly oversampling examples 
over <-ovun.sample(churn~., data=training, method="over", N=1000)$data
table(over$churn)

###remapling is added - Both classes are now at 500

##predictive model evaluation with new data 

rftrain <-randomForest(churn~., data=training)
rfover <-randomForest(churn~., data=over)
rfunder <-randomForest(churn~., data=under)
rfboth <-randomForest(churn~., data=both)
rfrose <-randomForest(churn~., data=rose)

confusionMatrix(predict(rftrain, testing), testing$churn, positive = '1')
confusionMatrix(predict(rfover, testing), testing$churn, positive = '1')
confusionMatrix(predict(rfunder, testing), testing$churn, positive = '1')
confusionMatrix(predict(rfboth, testing), testing$churn, positive = '1')
confusionMatrix(predict(rfrose, testing), testing$churn, positive = '1')
### Undersampling 
under <-ovun.sample(churn~., data=training, method="under", N=356)$data
table(under$churn)

both <-ovun.sample(churn~., data=training, method="both",
                   p=0.5, 
                   seed=222, 
                   N=678)$data
table(both$churn)


###Synthetic data

rose <-ROSE(churn~., data = training, N=1000, seed=222)$data
table(rose$churn)
summary(rose)





