###Ejemplo donde no bagging no ayuda mucho (HTF ,p. 288)##########################

library(ipred)
library(rpart)
x1<-runif(500,0,1)
x2<-runif(500,0,1)

clase<-factor(x1>x2)
datossim<-data.frame(clase,x1,x2)
names(datossim)<-c("clase","x1","x2")
bag_stumps<-bagging(clase~.,data=datossim,nbagg=1000,method="class",control=rpart.control(maxdepth=1,xval=0))
pred_stumps<-predict(bag_stumps,data=datossim,aggregation="majority",type="class")
x1<-rep(seq(0.01,1,by=0.01),100)
x2<-rep(seq(0.01,1,by=0.01),each=100)
pred_stumps2<-predict(bag_stumps,newdata=data.frame(x1,x2),aggregation="majority",type="class"  )
pred_stumps_col<-"white"
pred_stumps_col[pred_stumps2=="TRUE"]<-"salmon"
plot(x1,x2,col=pred_stumps_col,pch=16,cex=1.5)
points(datossim$x1,datossim$x2,pch=as.numeric(datossim$clase))          