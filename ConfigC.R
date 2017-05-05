#CONFIGURATION C
#With all the variables except G3 and G1.
studentC = student
studentC = studentC[-31]# Take out G1
#View(studentC)# available on RStudio

#Partition of Dataset
set.seed(8796) 
indC = sample(1:nrow(studentC),0.7*dim(studentC)[1])
trainC = studentC[indC,]
testC = studentC[-indC,]
ntrain = dim(trainC)[1] 
ntrain
ntest = dim(testC)[1]
ntest

#Setting first tree
tree.studentC=tree(binClass~.,trainC)
plot(tree.studentC, main=title(main = "First  Tree C"))
text(tree.studentC,pretty=0,col="blue", cex = 0.8)
summary(tree.studentC)

#Accuracy of the first Tree with the Training set:
tree.pred=predict(tree.studentC,trainC,type="class")
tab = table(tree.pred,trainC$binClass)
tab
trainAccuracy = sum(diag(tab))/ntrain
trainAccuracy #0.9637681

#Accuracy of the first Tree with the Test set: 
tree.pred=predict(tree.studentC,testC,type="class")
tab = table(tree.pred,testC$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.8907563 

#Improving the tree with Pruning and Cross-Validation
cv.student =cv.tree(tree.studentC ,FUN=prune.misclass )
par(mfrow=c(1,2))
plot(cv.student$size,cv.student$dev,type="b",xlab = "size", ylab = "CV errors")
plot(cv.student$k,cv.student$dev,type="b",xlab = "k", ylab = "CV errors")
par(mfrow=c(1,1))
cv.student
index = which.min(cv.student$dev)
index#4
newSize = cv.student[[1]][index]
newSize#2
prune.student=prune.misclass(tree.studentC, best=newSize)
summary(prune.student)
plot(prune.student,  main=title(main = "Prune Tree C"))
text(prune.student,pretty=0,col="brown")

#Accuracy of testing set in the Prune Tree:
tree.pred=predict(prune.student,testC,type="class")
tab = table(tree.pred,testC$binClass)
tab
testAccuracy = sum(diag(tab))/ntest
testAccuracy #0.9243697   

#Finding a tree with more interpretability level than the previous one if possible
#maintaining or improving the current test accuracy
set.seed(4291)
i=3
hc = c()
while(i<=20){
  tree.student=tree(binClass~.,trainC,control = tree.control(minsize = i,nobs = ntrain))
  tree.pred=predict(tree.student,testC,type="class")
  tab = table(tree.pred,testC$binClass)
  accuracy = sum(diag(tab))/ntest
  if(accuracy>=0.8907563){
    hc[i] = accuracy
    print(i)
    print(accuracy)
  }
  i = i+1
}

index = which.max(hc) # for option minsize
index #20
newAccuracy= hc[index]
newAccuracy #0.907563
tree.studentC=tree(binClass~.,trainC,control = tree.control(minsize = index, nobs = ntrain))
plot(tree.studentC, main=title(main = "New Tree C"))
text(tree.studentC,pretty=0,cex=0.7)

#Accuracy of the Testing set in this new Tree:
tree.pred=predict(tree.studentC,testC,type="class")
tab = table(tree.pred,tesC$binClass)
tab
accuracy = sum(diag(tab))/ntest
accuracy #0.907563
