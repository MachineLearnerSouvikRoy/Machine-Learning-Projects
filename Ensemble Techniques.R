df <-read.csv("D:/Courses/ML in R/Data Files/3 Decision Tree Dataset/Movie_classification.csv")

#Data Pre-processing
summary(df)

df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)

#Test Train Split
library('caTools')
set.seed(0)
split = sample.split(df, SplitRatio = 0.8)
train = subset(df,split==TRUE)
test = subset(df, split== FALSE)

#Bagging
install.packages('randomForest')
library('randomForest')
set.seed(0)

bagging = randomForest(Collection~.,data= train, mtry = 17)
test$bagging <- predict(bagging,test)
MSE2bagging <- mean((test$bagging-test$Collection)^2)




#Random Forest
install.packages('randomForest')

set.seed(0)
randomfor <- randomForest(Collection~.,data = train, ntree = 500)

#predict output for Random Forest
test$random <- predict(randomfor,test)
MSE2randomfor <-mean((test$random-test$Collection)^2)
table(test$Collection, test$random)

#Gradient Boosting
install.packages('gbm')
library(gbm)
set.seed(0)
boosting = gbm(Collection~., data = train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4,shrinkage = 0.2,verbose = F)
test$boo