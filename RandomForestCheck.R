#USING RANDOM FOREST

# Data Partition
set.seed(177) 
ind <- sample(1:nrow(student),0.7*dim(student)[1])
train <- student[ind,]
test <- student[-ind,]
ntrain = dim(train)[1] 
ntrain
ntest = dim(test)[1]
ntest

# Random Forest
library(randomForest)


cat("Random Forest 1\n")
set.seed(24)
rf.student = randomForest(binClass~., data=train,importance=TRUE)
print(rf.student)
plot(rf.student,main = "RF 1")
cat("Train Accuracy\n")
p1 = predict(rf.student, train)
confusionMatrix(p1, train$binClass)
tab = confusionMatrix(p1, train$binClass)$table
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #1
cat("Test Accuracy\n")
p2 = predict(rf.student, test)
confusionMatrix(p2, test$binClass)
tab = confusionMatrix(p2, test$binClass)$table
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.9243697


cat("Random Forest 2\n")
set.seed(2433)
rf.student2 = randomForest(binClass~., data=train, ntree=300,importance=TRUE, proximity=TRUE)
print(rf.student2)
plot(rf.student2,main = "RF 2")
cat("Train Accuracy\n")
p1 = predict(rf.student2, train)
confusionMatrix(p1, train$binClass)
tab = confusionMatrix(p1, train$binClass)$table
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #1
cat("Test Accuracy\n")
p2 = predict(rf.student2, test)
confusionMatrix(p2, test$binClass)
tab = confusionMatrix(p2, test$binClass)$table
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.9159664


cat("Random Forest 3\n")
set.seed(21444)
rf.student3 = randomForest(binClass~., data=train, ntree=20000, mtry=5,importance=TRUE, proximity=TRUE)
print(rf.student3)
plot(rf.student3,main = "RF 3")
cat("Train Accuracy\n")
p1 = predict(rf.student3, train)
confusionMatrix(p1, train$binClass)
tab = confusionMatrix(p1, train$binClass)$table
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #1
cat("Test Accuracy\n")
p2 = predict(rf.student3, test)
confusionMatrix(p2, test$binClass)
tab = confusionMatrix(p2, test$binClass)$table
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.9243697


#Tunnig
control = trainControl(method="repeatedcv", number=10, repeats=3, search="random")
control
set.seed(1235)
mtry = sqrt(ncol(student))
mtry
rf_random = train(binClass~., data=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


cat("Random Forest 4\n")
set.seed(8477)
rf.student4 = randomForest(binClass~., data=train, ntree=20000, mtry=rf_random$bestTune[[1]],importance=TRUE, proximity=TRUE)
print(rf.student4)
plot(rf.student4,main = "RF 4")
cat("Train Accuracy\n")
p1 = predict(rf.student4, train)
confusionMatrix(p1, train$binClass)
tab = confusionMatrix(p1, train$binClass)$table
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #1
cat("Test Accuracy\n")
p2 = predict(rf.student4, test)
confusionMatrix(p2, test$binClass)
tab = confusionMatrix(p2, test$binClass)$table
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.9243697

cat("Importance of the variables: Mean Decrease Accuray and Mean Decrease GINI")
importance(rf.student4)
varImpPlot(rf.student4,sort = T,main="Variable Importance")
attributes(importance(rf.student4))
cat("Plot of the importance variables")
listImp = attributes(importance(rf.student4))$dimnames[[1]]
n = length(listImp)
for(i in 1:n){
  partialPlot(rf.student4, train, listImp[i],"Pass",
              main = paste("Partial Dependence on",listImp[i],collapse = " " ))
}
#G2, G1, absences and failuresClasses are the relevant features and it match with the parent 
#nodes of previus trees configurations


