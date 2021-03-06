
###Bosques aleatorios#################################
#install.packages("randomForest")
library(randomForest)
library(rpart)
#install.packages("mlbench")
library(mlbench)
data(BostonHousing)
set.seed(111)
train<-sample(1:nrow(BostonHousing),400)
BostonHousing$train<-FALSE
BostonHousing$train[train]<-TRUE
BostonHousing_train<-BostonHousing[BostonHousing$train,]
BostonHousing_test<-BostonHousing[!BostonHousing$train,]
BostonHousing_train$train<-NULL

arbol_housing<-rpart(medv~.,data=BostonHousing_train,method="anova",
                     control=rpart.control(cp=0.001,xval=10))
?printcp
printcp(arbol_housing)
plotcp(arbol_housing)
mean((predict(arbol_housing, BostonHousing_test)-BostonHousing_test$medv)^2)


bosque_Housing<-randomForest(medv~.,data=BostonHousing_train,ntree=600,importance=TRUE,do.trace=TRUE)
 
print(bosque_Housing)
plot(bosque_Housing$mse)
#Comparación de las predicciones OOB con los observados
plot(predict(bosque_Housing),BostonHousing_train$medv)

bosque_Housing$importance 
varImpPlot(bosque_Housing,scale=FALSE)


pred_bosque <- predict(bosque_Housing,newdata=BostonHousing_test)
mean((pred_bosque-BostonHousing_test$medv)^2)






#Variando m? Número de árboles?
bosque_Housing_temp12<-randomForest(medv~.,data=BostonHousing_train,ntree=500,mtry=12)
bosque_Housing_temp10<-randomForest(medv~.,data=BostonHousing_train,ntree=500,mtry=10)

bosque_Housing_temp6<-randomForest(medv~.,data=BostonHousing_train,ntree=500,mtry=6)
bosque_Housing_temp4<-randomForest(medv~.,data=BostonHousing_train,ntree=500,mtry=4)
bosque_Housing_temp2<-randomForest(medv~.,data=BostonHousing_train,ntree=500,mtry=2)

bosque_Housing_temp2
bosque_Housing_temp4
bosque_Housing_temp6
bosque_Housing_temp10
bosque_Housing_temp12

#Graficar estimacíones OOB
plot(bosque_Housing_temp2$mse,type="l",ylim=c(0,40),lwd=2)
points(bosque_Housing_temp4$mse,col="red",type="l",pch=16)
points(bosque_Housing_temp6$mse,col="green",type="l")
points(bosque_Housing_temp10$mse,col="purple",type="l")
points(bosque_Housing_temp12$mse,col="blue",type="l")
abline(a=17,b=0)


# ===================
# = Ejemplo de SPAM =
# ===================
####Spam


spam.train<-read.table("./spam.train",sep=",")
names(spam.train) <- c("wfmake", "wfaddress", "wfall", "wf3d", "wfour",
	"wfover", "wfremove", "wfinternet", "wforder", "wfmail", 
	"wfreceive", "wfwill", "wfpeople", "wfreport", "wfaddresses", 
	"wffree", "wfbusiness", "wfemail", "wfyou", "wfcredit", "wfyour", 
	"wffont", "wf000", "wfmoney", "wfhp", "wfhpl", "wfgeorge", "wf650", 
	"wflab", "wflabs", "wftelnet", "wf857", "wfdata", "wf415", "wf85", 
	"wftechnology", "wf1999", "wfparts", "wfpm", "wfdirect", "wfcs", 
	"wfmeeting", "wforiginal", "wfproject", "wfre", "wfedu", "wftable", 
	"wfconference", "cfsc", "cfpar", "cfbrack", "cfexc", "cfdollar", 
	"cfpound", "crlaverage", "crllongest", "crltotal", "spam")
spam.train$spam<-as.factor(spam.train$spam)

spam.test<-read.table("spam.test",sep=",")
names(spam.test) <- c("wfmake", "wfaddress", "wfall", "wf3d", "wfour",
	"wfover", "wfremove", "wfinternet", "wforder", "wfmail", 
	"wfreceive", "wfwill", "wfpeople", "wfreport", "wfaddresses", 
	"wffree", "wfbusiness", "wfemail", "wfyou", "wfcredit", "wfyour", 
	"wffont", "wf000", "wfmoney", "wfhp", "wfhpl", "wfgeorge", "wf650", 
	"wflab", "wflabs", "wftelnet", "wf857", "wfdata", "wf415", "wf85", 
	"wftechnology", "wf1999", "wfparts", "wfpm", "wfdirect", "wfcs", 
	"wfmeeting", "wforiginal", "wfproject", "wfre", "wfedu", "wftable", 
	"wfconference", "cfsc", "cfpar", "cfbrack", "cfexc", "cfdollar", 
	"cfpound", "crlaverage", "crllongest", "crltotal", "spam")
spam.test$spam<-as.factor(spam.test$spam)      



bosque_spam<-randomForest(spam~.,data=spam.train,ntree=1000,importance=TRUE,do.trace=TRUE)

bosque_spam$type

varImpPlot(bosque_spam)

bosque_spam2<-randomForest(spam~.,data=spam.train,ntree=500,do.trace=TRUE,mtry=2)
bosque_spam6<-randomForest(spam~.,data=spam.train,ntree=500,do.trace=TRUE,mtry=6)
bosque_spam20<-randomForest(spam~.,data=spam.train,ntree=500,do.trace=TRUE,mtry=20)

plot(bosque_spam20$err.rate[,1],type="l")
points(bosque_spam6$err.rate[,1],type="l",col="red")
points(bosque_spam2$err.rate[,1],type="l",col="blue")


mean(predict(bosque_spam, newdata= spam.test)!=spam.test$spam)


#Para un árbol, obteníamos tasas de error del 8%
