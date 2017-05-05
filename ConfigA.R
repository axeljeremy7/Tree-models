
#CONFIGURATION A
#With all variables except G3

#Partition of Dataset
set.seed(1234) 
ind = sample(1:nrow(student),0.7*dim(student)[1])
train = student[ind,]
test = student[-ind,]
ntrain = dim(train)[1] 
ntrain
ntest = dim(test)[1]
ntest

#Setting first tree
tree.student=tree(binClass~.,train)
plot(tree.student, main=title(main = "First  Tree A"))
text(tree.student,pretty=0,col="blue", cex = 0.8)
summary(tree.student)

#Accuracy of the first Tree with the Training set:
tree.pred=predict(tree.student,train,type="class")
tab = table(tree.pred,train$binClass)
cat("Table")
tab
trainAccuracy = sum(diag(tab))/ntrain
cat("Train accuracy is , ",trainAccuracy, "\n") #0.9456522

#Accuracy of the first Tree with the Test set: 
tree.pred=predict(tree.student,test,type="class")
tab = table(tree.pred,test$binClass)
cat("Table \n")
tab
testAccuracy = sum(diag(tab))/ntest
cat("Test accuracy is , ",testAccuracy, "\n") #0.8655462 

#Improving the tree with Pruning and Cross-Validation
cv.student =cv.tree(tree.student ,FUN=prune.misclass )
par(mfrow=c(1,2))
plot(cv.student$size,cv.student$dev,type="b",xlab = "size", ylab = "CV errors")
plot(cv.student$k,cv.student$dev,type="b",xlab = "k", ylab = "CV errors")
par(mfrow=c(1,1))
cv.student
index = which.min(cv.student$dev)
index
newSize = cv.student[[1]][index]
newSize
prune.student=prune.misclass(tree.student, best=newSize)
summary(prune.student)
plot(prune.student,  main=title(main = "Prune Tree"))
text(prune.student,pretty=0,col="brown")

#Accuracy of testing set in the Prune Tree:
tree.pred=predict(prune.student,test,type="class")
tab = table(tree.pred,test$binClass)
cat("Table \n")
tab
testAccuracy = sum(diag(tab))/ntest
cat("Test accuracy is , ",testAccuracy, "\n") #0.8907563   

#Finding a tree with more interpretability level than the previous one
#maintaining or improving the current test accuracy
set.seed(127) 
i=3
hc = c()
while(i<=10){
  tree.student=tree(binClass~.,train,control = tree.control(minsize = i,nobs = ntrain))
  tree.pred=predict(tree.student,test,type="class")
  tab = table(tree.pred,test$binClass)
  accuracy = sum(diag(tab))/ntest
  if(accuracy>=testAccuracy){
    hc[i] = accuracy
    print(i)
    print(accuracy)
  }
  i = i+1
}

index = which.max(hc) # for option minsize
index #3
newAccuracy= hc[index]
newAccuracy #0.9159664  
tree.student=tree(binClass~.,train,control = tree.control(minsize = index, nobs = ntrain))
plot(tree.student, main=title(main = "New Tree A"))
text(tree.student,pretty=0,cex=0.7)

#Accuracy of the Testing set in this new Tree:
tree.pred=predict(tree.student,test,type="class")
tab = table(tree.pred,test$binClass)
cat("Table \n")
tab
accuracy = sum(diag(tab))/ntest
cat("Test accuracy is , ",accuracy, "\n") #0.8991597



