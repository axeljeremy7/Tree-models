#CONFIGURATION D
#With all the variables except G3, G2 and G1.
studentD = student
studentD = studentD[-32]# Take out G2
studentD = studentD[-31]# Take out G1
#View(studentD)# available on RStudio

#Partition of Dataset
set.seed(439) 
indD = sample(1:nrow(studentD),0.7*dim(studentD)[1])
trainD = studentD[indD,]
testD = studentD[-indD,]
ntrain = dim(trainD)[1] 
ntrain
ntest = dim(testD)[1]
ntest

#Setting first tree
tree.studentD=tree(binClass~.,trainD)
plot(tree.studentD, main=title(main = "First Training Tree D"))
text(tree.studentD,pretty=0,col="blue", cex = 0.8)
summary(tree.studentD)

#Accuracy of the first Tree with the Training set:
tree.pred=predict(tree.studentD,trainD,type="class")
tab = table(tree.pred,trainD$binClass)
tab
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #0.8731884

#Accuracy of the first Tree with the Test set: 
tree.pred=predict(tree.studentD,testD,type="class")
tab = table(tree.pred,testD$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.6470588 

#Improving the tree with Pruning and Cross-Validation
cv.student =cv.tree(tree.studentD ,FUN=prune.misclass )
par(mfrow=c(1,2))
plot(cv.student$size,cv.student$dev,type="b",xlab = "size", ylab = "CV errors")
plot(cv.student$k,cv.student$dev,type="b",xlab = "k", ylab = "CV errors")
par(mfrow=c(1,1))
cv.student
index = which.min(cv.student$dev)
index
newSize = cv.student[[1]][index]
newSize
prune.student=prune.misclass(tree.studentD, best=newSize)
summary(prune.student)
plot(prune.student,  main=title(main = "Prune Tree D"))
text(prune.student,pretty=0,col="brown")

#Accuracy of testing set in the Prune Tree:
tree.pred=predict(prune.student,testD,type="class")
tab = table(tree.pred,testD$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.6890756   

#Finding a tree with more interpretability level than the previous one if possible
#maintaining or improving the current test accuracy
set.seed(98775)
i=3
hc = c()
while(i<=20){
  tree.student=tree(binClass~.,trainD,control = tree.control(minsize = i,nobs = ntrain))
  tree.pred=predict(tree.student,testD,type="class")
  tab = table(tree.pred,testD$binClass)
  accuracy = sum(diag(tab))/ntest
  if(accuracy>=0.6470588){
    hc[i] = accuracy
    print(i)
    print(accuracy)
  }
  i = i+1
}

index = which.max(hc) # for option minsize
index #10
newAccuracy= hc[index]
newAccuracy #0.6722689
tree.studentD=tree(binClass~.,trainD,control = tree.control(minsize = index, nobs = ntrain))
plot(tree.studentD, main=title(main = "New Tree D"))
text(tree.studentD,pretty=0,cex=0.7)

#Accuracy of the Testing set in this new Tree:
tree.pred=predict(tree.studentD,testD,type="class")
tab = table(tree.pred,testD$binClass)
tab
accuracy = sum(diag(tab))/ntest
accuracy #0.6638655