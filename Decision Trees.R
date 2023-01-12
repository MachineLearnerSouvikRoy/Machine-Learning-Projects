movie <- read.csv("D:/Courses/ML in R/Data Files/3 Decision Tree Dataset/Movie_regression.csv")

#data preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)

#splitting into test and train set
library("caTools")
set.seed(0)
split = sample.split(movie,SplitRatio = 0.8)
train = subset(movie,split == TRUE)
test = subset(movie, split== FALSE)

#package installation
install.packages('rpart')
install.packages('rpart.plot')
library('rpart')
library('rpart.plot')

#run regression model on train set
regtree <- rpart(formula = Collection~.,data = train, control = rpart.control(maxdepth = 3))

#plot decision tree
rpart.plot(regtree, box.palette = "RdBu", digits = -2)


#predicting value at any point

test$pred <- predict(regtree, test,type = "vector")
MSE2 <- mean((test$pred-test$Collection)^2)

#Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control(cp=0))
rpart.plot(fulltree, box.palette = "RdBu", digits = -3)
printcp (fulltree)
plotcp (fulltree)                  


mincp  <- regtree$cptable[which.min(regtree$cptable[,"xerror"]), "CP"]

prunedtree <- prune(regtree, cp= mincp)
rpart.plot(prunedtree, box.palette = "RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2a <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2b <- mean((test$pruned-test$Collection)^2)


