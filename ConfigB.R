#CONFIGURATION B
#With all the variables except G3 and G2. 
studentB = student
studentB = studentB[-32]# Take out G2
#View(studentB)#works only in RStudio

#Partition of Dataset
set.seed(4567) 
indB = sample(1:nrow(studentB),0.7*dim(studentB)[1])
trainB = studentB[indB,]
testB = studentB[-indB,]
ntrain = dim(trainB)[1] 
ntrain
ntest = dim(testB)[1]
ntest

#Setting first tree
tree.studentB=tree(binClass~.,trainB)
plot(tree.studentB, main=title(main = "First  Tree B"))
text(tree.studentB,pretty=0,col="blue", cex = 0.8)
summary(tree.studentB)

#Accuracy of the first Tree with the Training set:
tree.pred=predict(tree.studentB,trainB,type="class")
tab = table(tree.pred,trainB$binClass)
tab
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #0.8913043

#Accuracy of the first Tree with the Test set: 
tree.pred=predict(tree.studentB,testB,type="class")
tab = table(tree.pred,testB$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.8067227 

#Improving the tree with Pruning and Cross-Validation
cv.student =cv.tree(tree.studentB ,FUN=prune.misclass )
par(mfrow=c(1,2))
plot(cv.student$size,cv.student$dev,type="b",xlab = "size", ylab = "CV errors")
plot(cv.student$k,cv.student$dev,type="b",xlab = "k", ylab = "CV errors")
par(mfrow=c(1,1))
cv.student
index = which.min(cv.student$dev)
index
newSize = cv.student[[1]][index]
newSize
prune.student=prune.misclass(tree.studentB, best=newSize)
summary(prune.student)
plot(prune.student,  main=title(main = "Prune Tree B"))
text(prune.student,pretty=0,col="brown")

#Accuracy of testing set in the Prune Tree:
tree.pred=predict(prune.student,testB,type="class")
tab = table(tree.pred,testB$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.8235294   

#Finding a tree with more interpretability level than the previous one if possible
#maintaining or improving the current test accuracy
set.seed(8765)
i=3
hc = c()
while(i<=10){
  tree.student=tree(binClass~.,trainB,control = tree.control(minsize = i,nobs = ntrain))
  tree.pred=predict(tree.student,testB,type="class")
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
index #10
newAccuracy= hc[index]
newAccuracy #0.8235294
tree.studentB=tree(binClass~.,trainB,control = tree.control(minsize = index, nobs = ntrain))
plot(tree.studentB, main=title(main = "New Tree B"))
text(tree.studentB,pretty=0,cex=0.7)

#Accuracy of the Testing set in this new Tree:
tree.pred=predict(tree.studentB,testB,type="class")
tab = table(tree.pred,testB$binClass)
tab
accuracy = sum(diag(tab))/ntest
accuracy #0.8235294
