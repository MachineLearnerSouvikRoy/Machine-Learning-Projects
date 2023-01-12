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


#run classification model on train set
install.packages('rpart')
install.packages('rpart.plot')
library('rpart')
library('rpart.plot')

classtree <- rpart(formula = Start_Tech_Oscar~.,data = train, method= 'class',control = rpart.control(maxdepth = 3))

#Plot classification tree
rpart.plot(classtree,box.palette = "RdBu", digits = -3)

#Predicting value at any point
test$pred = predict(classtree, test,type = "class" )

#Creating confusion matrix
table(test$Start_Tech_Oscar,test$pred)





