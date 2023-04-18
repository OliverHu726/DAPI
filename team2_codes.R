library(ggplot2)
library(dplyr)
library(caret)
library(gbm)
library(randomForest)
library(pROC)

data=read.csv("https://raw.githubusercontent.com/OliverHu726/DAPI/main/trainingData.csv",header = T)

## EDA
# destination
destination = data %>%
  group_by(destination) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(destination, aes(x = destination)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Destination", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# passanger
passanger = data %>%
  group_by(passanger) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(passanger, aes(x = passanger)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Passanger", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# weather
weather = data %>%
  group_by(weather) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())
ggplot(weather, aes(x = weather)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Weather", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# temperature
temperature = data %>%
  group_by(temperature) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())
ggplot(temperature, aes(x = temperature)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Temperature", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# time
time = data %>%
  group_by(time) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(time, aes(x = time)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Time", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# coupon
coupon = data %>%
  group_by(coupon) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(coupon, aes(x = coupon)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Coupon", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# expiration
expiration = data %>%
  group_by(expiration) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(expiration, aes(x = expiration)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Expiration", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# gender
gender = data %>%
  group_by(gender) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(gender, aes(x = gender)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Gender", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# age
age = data %>%
  group_by(age) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(age, aes(x = age)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Age", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# maritalStatus
maritalStatus = data %>%
  group_by(maritalStatus) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(maritalStatus, aes(x = maritalStatus)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "MaritalStatus", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# has_children
has_children = data %>%
  group_by(has_children) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(has_children, aes(x = has_children)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Has_children", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# education
education = data %>%
  group_by(education) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(education, aes(x = education)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Education", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# occupation
occupation = data %>%
  group_by(occupation) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

p = ggplot(occupation, aes(x = occupation)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Occupation", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

p+theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

# income
income = data %>%
  group_by(income) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())

ggplot(income, aes(x = income)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Income", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# duration
data$duration = ifelse(data$toCoupon_GEQ15min == 0 & data$toCoupon_GEQ25min == 0, "15less", 
                       ifelse(data$toCoupon_GEQ15min == 1 & data$toCoupon_GEQ25min == 0, "15to25", "25more"))
duration = data %>%
  group_by(duration) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())
ggplot(duration, aes(x = duration)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Duration", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# direction
direction = data %>%
  group_by(direction_same) %>%
  summarize(percent = mean(Y),
            Y = sum(Y),
            total=n())
ggplot(direction, aes(x = direction_same)) +
  geom_bar(aes(y = total), stat = "identity", position = "dodge",fill="steelblue") +
  geom_bar(aes(y = Y), stat = "identity", position = "dodge",fill="orange") +
  labs(x = "Direction", y = "Number") +
  geom_text(aes(y= total, label = paste0(round(percent * 100), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme_minimal()

# General acceptance
data$Y = ifelse(data$Y == 0, "no", "yes")
data$Y = as.factor(data$Y)

Y = data %>% 
  group_by(Y) %>%
  summarize(count=n())

ggplot(Y, aes(x = Y)) +
  geom_bar(aes(y=count), stat = "identity", position = "dodge",fill="lightgreen")+
  labs(x = "Acceptance", y = "Number") +
  geom_text(aes(y= count, label = paste0(count/100, "%")), 
            position = position_stack(vjust = 0.5), color = "black") +
  theme_minimal()

# Data Imputation
# duration
data$duration = ifelse(data$toCoupon_GEQ15min == 0 & data$toCoupon_GEQ25min == 0, "15less", 
                       ifelse(data$toCoupon_GEQ15min == 1 & data$toCoupon_GEQ25min == 0, "15to25", "25more"))

# direction
data$direction = ifelse(data$direction_same == 1 & data$direction_opp == 0, "same", "Opposite")

# selecting  variables for modeling.  A total of 21 variables with name updates
promotion.df <- select(data, 
                    destination,
                    passanger,
                    weather,
                    temperature,
                    time,
                    coupon,
                    expiration,
                    gender,
                    age,
                    maritalStatus,
                    has_children,
                    education,
                    occupation,
                    income,
                    Bar,
                    CoffeeHouse,
                    CarryAway,
                    RestaurantLessThan20,
                    Restaurant20To50,
                    duration,
                    direction,
                    Y)

# missing values are found in "Bars", "CoffeeHouse"," CarryAway", " RestaurantLessThan20" and "Restaurant20To50"
# replace missing values "" with median "1~3"
promotion.df = data.frame(lapply(promotion.df, function(x) ifelse(x == "", "1~3", x)))

# Convert categorical variables to factors
factorVar = c("destination", "passanger", "weather","temperature", "time","coupon","expiration","gender",
              "income", "age","maritalStatus","has_children","education","occupation","Bar","CoffeeHouse",
              "CarryAway","RestaurantLessThan20","Restaurant20To50","Y","duration","direction")
promotion.df[, factorVar] = lapply(promotion.df[, factorVar], as.factor)

# MODELING
outcomeName = 'Y'
predictorNames = names(promotion.df)[names(promotion.df) != outcomeName]  

# Split the data set into training and testing sets
set.seed(123)
index = createDataPartition(promotion.df$Y, p = 0.8, list = FALSE, times = 1)
training = promotion.df[index,]
testing = promotion.df[-index,]

# Building models 
# GBM model setup and tuning
# model 1
fitControl.gbm1 = trainControl(method = "cv",
                               number = 10,
                               sampling = "up")

gbm1=train(training[,predictorNames],training[,outcomeName],  
                 method='gbm',
                 trControl=fitControl.gbm1)

# model 2
fitControl.gbm2 = trainControl(method = "cv",
                               number = 15,
                               sampling = "up")

gbm2<-train(training[,predictorNames],training[,outcomeName],
            method='gbm',
            trControl=fitControl.gbm2)

# model 3
fitControl.gbm3 = trainControl(method = "cv",
                               number = 20,
                               sampling = "up")

gbm3<-train(training[,predictorNames],training[,outcomeName],
            method='gbm',
            trControl=fitControl.gbm3)

# model 4
fitControl.gbm4 = trainControl(method = "repeatedcv",
                               number = 15,
                               repeats = 5,
                               sampling = "up")

gbm4<-train(training[,predictorNames],training[,outcomeName],
            method='gbm',
            trControl=fitControl.gbm4)

# model 5
fitControl.gbm5 = trainControl(method = "repeatedcv",
                               number = 15,
                               repeats = 6,
                               sampling = "up")

gbm5=train(training[,predictorNames],training[,outcomeName],
            method='gbm',
            trControl=fitControl.gbm5,
            verbose = F)

# model 6
fitControl.gbm6 = trainControl(method = "repeatedcv",
                               number = 15,
                               repeats = 7,
                               sampling = "up",
                               search = "grid")

gbm6=train(training[,predictorNames],training[,outcomeName],
           method='gbm',
           trControl=fitControl.gbm6,
           verbose = F)

# advanced tuning
fitControl.gbm7 = trainControl(method = "repeatedcv",
                               number = 15,
                               repeats = 6,
                               sampling = "up",
                               search = "grid")
grid <- expand.grid(
  n.trees = c(500, 1000, 2000),
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.1, 0.01, 0.001),
  n.minobsinnode = c(10, 30, 50))

gbm7=train(training[,predictorNames],training[,outcomeName],
           method='gbm',
           trControl=fitControl.gbm7,
           tuneGrid = grid,
           verbose = F)
gbm7$bestTune

# tuning
fitControl.gbm8 = trainControl(method = "repeatedcv",
                               number = 15,
                               repeats = 6,
                               sampling = "up",
                               search = "grid")
grid <- expand.grid(
  n.trees = 1800,
  interaction.depth = 8,
  shrinkage = 0.12,
  n.minobsinnode = 9)

gbm8=train(training[,predictorNames],training[,outcomeName],
           method='gbm',
           trControl=fitControl.gbm8,
           tuneGrid = grid,
           verbose = F)

# measuring Performance
gbm1.predict<-predict(gbm1,testing[,predictorNames],type="raw")
CM1 = confusionMatrix(gbm1.predict,testing[,outcomeName])

gbm2.predict<-predict(gbm2,testing[,predictorNames],type="raw")
CM2 = confusionMatrix(gbm2.predict,testing[,outcomeName])

gbm3.predict<-predict(gbm3,testing[,predictorNames],type="raw")
CM3 = confusionMatrix(gbm3.predict,testing[,outcomeName])

gbm4.predict<-predict(gbm4,testing[,predictorNames],type="raw")
CM4 = confusionMatrix(gbm4.predict,testing[,outcomeName])

gbm5.predict<-predict(gbm5,testing[,predictorNames],type="raw")
CM5 = confusionMatrix(gbm5.predict,testing[,outcomeName])

gbm6.predict<-predict(gbm6,testing[,predictorNames],type="raw")
CM6 = confusionMatrix(gbm6.predict,testing[,outcomeName])

gbm7.predict<-predict(gbm7,testing[,predictorNames],type="raw")
CM7 = confusionMatrix(gbm7.predict,testing[,outcomeName])

gbm8.predict<-predict(gbm8,testing[,predictorNames],type="raw")
CM8 = confusionMatrix(gbm8.predict,testing[,outcomeName])
CM8

# calculate F-score
f_score_gbm1 = 2*(CM1$byClass["Precision"]*CM1$byClass["Recall"])/(CM1$byClass["Precision"] + CM1$byClass["Recall"])
f_score_gbm1

f_score_gbm2 = 2*(CM2$byClass["Precision"]*CM2$byClass["Recall"])/(CM2$byClass["Precision"] + CM2$byClass["Recall"])
f_score_gbm2

f_score_gbm3 = 2*(CM3$byClass["Precision"]*CM3$byClass["Recall"])/(CM3$byClass["Precision"] + CM3$byClass["Recall"])
f_score_gbm3

f_score_gbm4 = 2*(CM4$byClass["Precision"]*CM4$byClass["Recall"])/(CM4$byClass["Precision"] + CM4$byClass["Recall"])
f_score_gbm4

f_score_gbm5 = 2*(CM5$byClass["Precision"]*CM5$byClass["Recall"])/(CM5$byClass["Precision"] + CM5$byClass["Recall"])
f_score_gbm5

f_score_gbm6 = 2*(CM6$byClass["Precision"]*CM6$byClass["Recall"])/(CM6$byClass["Precision"] + CM6$byClass["Recall"])
f_score_gbm6

f_score_gbm7 = 2*(CM7$byClass["Precision"]*CM7$byClass["Recall"])/(CM7$byClass["Precision"] + CM7$byClass["Recall"])
f_score_gbm7

# Draw ROC curve
gbm1.probs = predict(gbm1,testing[,predictorNames],type="prob")
gbm2.probs = predict(gbm2,testing[,predictorNames],type="prob")
gbm3.probs = predict(gbm3,testing[,predictorNames],type="prob")
gbm4.probs = predict(gbm4,testing[,predictorNames],type="prob")
gbm5.probs = predict(gbm5,testing[,predictorNames],type="prob")
gbm6.probs = predict(gbm6,testing[,predictorNames],type="prob")
gbm7.probs = predict(gbm7,testing[,predictorNames],type="prob")

gbm1.plot=plot(roc(testing$Y,gbm1.probs[,2]))
gbm2.plot=lines(roc(testing$Y,gbm2.probs[,2]), col="blue")
gbm3.plot=lines(roc(testing$Y,gbm3.probs[,2]), col="red")
gbm4.plot=lines(roc(testing$Y,gbm4.probs[,2]), col="green")
gbm5.plot=lines(roc(testing$Y,gbm5.probs[,2]), col="yellow")
gbm6.plot=lines(roc(testing$Y,gbm6.probs[,2]), col="purple")
gbm7.plot=lines(roc(testing$Y,gbm7.probs[,2]), col="orange")
legend(x=0.0,y=1, legend=c("gbm2", "gbm1","gbm3","gbm4","gbm5","gbm6","gbm7"), col=c("blue", "black","red","green","yellow","purple","orange"), 
       cex=0.75, lwd=3)  

# Calculate the area under ROC curve
auc(testing$Y,gbm1.probs[,2])
auc(testing$Y,gbm2.probs[,2])
auc(testing$Y,gbm3.probs[,2])
auc(testing$Y,gbm4.probs[,2])
auc(testing$Y,gbm5.probs[,2])
auc(testing$Y,gbm6.probs[,2])
auc(testing$Y,gbm7.probs[,2])

# RF model setup and tuning
set.seed(123)
# model 1
model_rf1 = randomForest(Y ~ ., data = training, type="classification", ntree = 1000, mtry = 9, importance = TRUE)
model_rf1

# model 2
model_rf2 = randomForest(Y ~ ., data = training, type="classification", ntree = 1500, mtry = 9, importance = TRUE)
model_rf2

# model 3
model_rf3 = randomForest(Y ~ ., data = training, type="classification", ntree = 2000, mtry = 9, importance = TRUE)
model_rf3

# model 4
model_rf4 = randomForest(Y ~ ., data = training, type="classification", ntree = 1500, mtry = 7, importance = TRUE)
model_rf4

# model 5
model_rf5 = randomForest(Y ~ ., data = training, type="classification", ntree = 1500, mtry = 11, importance = TRUE)
model_rf5

# model 6
model_rf6 = randomForest(Y ~ ., data = training, type="classification", ntree = 1600, mtry = 11, importance = TRUE)
model_rf6

# make predictions on the testing data set and evaluate performance
predictions_rf1 = predict(model_rf1, testing)
CM1 = confusionMatrix(predictions_rf1, testing$Y)
CM1

predictions_rf2 = predict(model_rf2, testing)
CM2 = confusionMatrix(predictions_rf2, testing$Y)
CM2

predictions_rf3 = predict(model_rf3, testing)
CM3 = confusionMatrix(predictions_rf3, testing$Y)
CM3

predictions_rf4 = predict(model_rf4, testing)
CM4 = confusionMatrix(predictions_rf4, testing$Y)
CM4

predictions_rf5 = predict(model_rf5, testing)
CM5 = confusionMatrix(predictions_rf5, testing$Y)
CM5

predictions_rf6 = predict(model_rf6, testing)
CM6 = confusionMatrix(predictions_rf6, testing$Y)
CM6

# calculate the accuracy and f-score
accuracy_rf1 = sum(predictions_rf1 == testing$Y)/length(predictions_rf1)
f_score_rf1 = 2*(CM1$byClass["Precision"]*CM1$byClass["Recall"])/(CM1$byClass["Precision"] + CM1$byClass["Recall"])
table(accuracy_rf1, f_score_rf1)

accuracy_rf2 = sum(predictions_rf2 == testing$Y)/length(predictions_rf2)
f_score_rf2 = 2*(CM2$byClass["Precision"]*CM2$byClass["Recall"])/(CM2$byClass["Precision"] + CM2$byClass["Recall"])
table(accuracy_rf2, f_score_rf2)

accuracy_rf3 = sum(predictions_rf3 == testing$Y)/length(predictions_rf3)
f_score_rf3 = 2*(CM3$byClass["Precision"]*CM3$byClass["Recall"])/(CM3$byClass["Precision"] + CM3$byClass["Recall"])
table(accuracy_rf3, f_score_rf3)

accuracy_rf4 = sum(predictions_rf4 == testing$Y)/length(predictions_rf4)
f_score_rf4 = 2*(CM4$byClass["Precision"]*CM4$byClass["Recall"])/(CM4$byClass["Precision"] + CM4$byClass["Recall"])
table(accuracy_rf4, f_score_rf4)

accuracy_rf5 = sum(predictions_rf5 == testing$Y)/length(predictions_rf5)
f_score_rf5 = 2*(CM5$byClass["Precision"]*CM5$byClass["Recall"])/(CM5$byClass["Precision"] + CM5$byClass["Recall"])
table(accuracy_rf5, f_score_rf5)

accuracy_rf6 = sum(predictions_rf6 == testing$Y)/length(predictions_rf6)
f_score_rf6 = 2*(CM6$byClass["Precision"]*CM6$byClass["Recall"])/(CM6$byClass["Precision"] + CM6$byClass["Recall"])
table(accuracy_rf6, f_score_rf6)

# ROC
# Draw ROC curve
rf1.probs = predict(model_rf1,testing[,predictorNames],type="prob")
rf2.probs = predict(model_rf2,testing[,predictorNames],type="prob")
rf3.probs = predict(model_rf3,testing[,predictorNames],type="prob")
rf4.probs = predict(model_rf4,testing[,predictorNames],type="prob")
rf5.probs = predict(model_rf5,testing[,predictorNames],type="prob")
rf6.probs = predict(model_rf6,testing[,predictorNames],type="prob")

rf1.plot=plot(roc(testing$Y,rf1.probs[,2]))
rf2.plot=lines(roc(testing$Y,rf2.probs[,2]), col="blue")
rf3.plot=lines(roc(testing$Y,rf3.probs[,2]), col="red")
rf4.plot=lines(roc(testing$Y,rf4.probs[,2]), col="green")
rf5.plot=lines(roc(testing$Y,rf5.probs[,2]), col="yellow")
rf6.plot=lines(roc(testing$Y,rf6.probs[,2]), col="purple")
legend(x=0.0,y=1, legend=c("rf2", "rf1","rf3","rf4","rf5","rf6"), col=c("blue", "black","red","green","yellow", "purple"), 
       cex=0.75, lwd=3)  

# Calculate the area under ROC curve
auc(testing$Y,rf1.probs[,2])
auc(testing$Y,rf2.probs[,2])
auc(testing$Y,rf3.probs[,2])
auc(testing$Y,rf4.probs[,2])
auc(testing$Y,rf5.probs[,2])
auc(testing$Y,rf6.probs[,2])

# computes variable importance for the model
rfImp <- caret::varImp(model_rf5, scale = TRUE)
rfImp = sort(rfImp[,"1"], decreasing = T)



## Final Model for scoring
set.seed(123)
fitControl.gbm = trainControl(method = "repeatedcv",
                              number = 15,
                              repeats = 6,
                              sampling = "up",
                              search = "grid")
grid = expand.grid(
  n.trees = 1800,
  interaction.depth = 7,
  shrinkage = 0.1,
  n.minobsinnode = 10)

gbm=train(promotion.df[,predictorNames],promotion.df[,outcomeName],
          method='gbm',
          trControl=fitControl.gbm,
          tuneGrid = grid,
          verbose = F)

# Codes for saving and loading the model
saveRDS(gbm,"gbm_model.rds")
gbm = readRDS("gbm_model.rds")

# Importance plot
gbmImp <- caret::varImp(gbm, scale = TRUE, splitLevels = T)
plot(gbmImp, top = 20)
