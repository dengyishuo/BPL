#read 5 years data
d1 = read.csv("PL2012-13.csv",header = T) 
d2 = read.csv("PL2011-12.csv",header = T) 
d3 = read.csv("PL2010-11.csv",header = T) 
d4 = read.csv("PL2009-10.csv",header = T) 
d5 = read.csv("PL2008-09.csv",header = T) 

label = c("HomeTeam","AwayTeam","FTHG","FTAG","HS","AS","HST","AST","HC","AC","HF","AF","HR","AR","Lambda.H.","Lambda.A.")
data = d1[,label]
data = rbind(data,d2[,label])
data = rbind(data,d3[,label])
data = rbind(data,d4[,label])
data = rbind(data,d5[,label])

H_minus_lambda = data[,"FTHG"] - data[,"Lambda.H."]
A_minus_lambda = data[,"FTAG"] - data[,"Lambda.A."]

m1 = ifelse((H_minus_lambda + A_minus_lambda>=0),1,0)
m2 = ifelse((H_minus_lambda - A_minus_lambda>=0),1,0)

dat1 =  data.frame(x=data[,6:14],y=as.factor(m1))  
dat2 =  data.frame(x=data[,6:14],y=as.factor(m2))  

#scaling for whole data set
dat1[,1:9]= data.frame(apply(dat1[,c(1:9)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
dat2[,1:9]= data.frame(apply(dat2[,c(1:9)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))

#for classification 1
#random split training and testing data
n=nrow(dat1)
set.seed(1)
train=sample(n,n*0.8)

library(e1071) #load package for 
#use linear model
set.seed(1)
#cross validation to search best parameter
tune.out_linear = tune(svm,y~.,data=dat1[train,],kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out_linear)
plot(tune.out_linear)
bestmod_linear=tune.out_linear$best.model #get best model
summary(bestmod_linear)
table(bestmod_linear$fitted,dat1[train,]$y)
#testing data
pred_linear=predict(bestmod_linear,newdata=dat1[-train,])
table(pred_linear,dat1[-train,]$y) #accuracy around

#use radial kernel 
set.seed(1)
#cross-validation to get roughly parameter range
gammaRange = 2^(-7:5)
costRange = 2^(-5:5)
tune.out_radial = tune.svm(y ~ ., data = dat1[train,], gamma=gammaRange, cost=costRange,
                           tunecontrol = tune.control(sampling="cross", cross=10))
summary(tune.out_radial)
plot(tune.out_radial)
bestmod_radial_1=tune.out_radial$best.model #get best model 
summary(bestmod_radial_1)
table(bestmod_radial_1$fitted,dat1[train,]$y) #training accuracy improve a lot
#testing data
pred.te=predict(bestmod_radial_1,newdata=dat1[-train,])
table(pred.te,dat1[-train,]$y) #testing accuracy almost unchange compared to linear kernel

#search for better paramter
#a big problem
gamma <- 2^-7
cost  <- 2^3
gammaRange =2^seq(-8,-6,0.2)
costRange = 2^seq(2,4,0.2)
set.seed(1)
tune.out_radial = tune.svm(y ~ ., data = dat1[train,], gamma=gammaRange, cost=costRange,
                           tunecontrol = tune.control(sampling="cross", cross=8))
summary(tune.out_radial)
plot(tune.out_radial)
bestmod_radial=tune.out_radial$best.model #get best model
summary(bestmod_radial)
#plot(bestmod_radial,dat1[train,])
table(bestmod_radial$fitted,dat1[train,]$y) #training accuracy improve a lot
#testing data
pred.te=predict(bestmod_radial,newdata=dat1[-train,])
table(pred.te,dat1[-train,]$y) #testing accuracy almost unchange compared to linear kernel


#for classification 2
set.seed(1)
#cross validation to search best parameter
tune.out_linear = tune(svm,y~.,data=dat2[train,],kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out_linear)
plot(tune.out_linear)
bestmod_linear=tune.out_linear$best.model #get best model
summary(bestmod_linear)
table(bestmod_linear$fitted,dat2[train,]$y)
#testing data
pred_linear=predict(bestmod_linear,newdata=dat2[-train,])
table(pred_linear,dat2[-train,]$y) #accuracy around

#use radial kernel
#cross-validation to get roughly parameter range
gammaRange = 2^(-5:5)
costRange = 2^(-5:5)
tune.out_radial = tune.svm(y ~ ., data = dat2[train,], gamma=gammaRange, cost=costRange,
                           tunecontrol = tune.control(sampling="cross", cross=8))
summary(tune.out_radial)
plot(tune.out_radial)
bestmod_radial=tune.out_radial$best.model #get best model
summary(bestmod_radial)
table(bestmod_radial$fitted,dat2[train,]$y) #training accuracy improve a lot
#testing data
pred.te=predict(bestmod_radial,newdata=dat2[-train,])
table(pred.te,dat2[-train,]$y) #testing accuracy almost unchange compared to linear kernel

#search for better paramter
gamma <- 0.0625
cost  <- 0.5
#exponentially growing sequence
gammaRange <- 2^seq(log10(gamma)-1,log10(gamma)+1,length=11)[2:10]
costRange  <- 2^seq(log10(cost)-1 ,log10(cost)+1 ,length=11)[2:10]
set.seed(1)
tune.out_radial = tune.svm(y ~ ., data = dat1[train,], gamma=gammaRange, cost=costRange,
                           tunecontrol = tune.control(sampling="cross", cross=8))
summary(tune.out_radial)
plot(tune.out_radial)
bestmod_radial=tune.out_radial$best.model #get best model
summary(bestmod_radial)
#plot(bestmod_radial,dat1[train,])
table(bestmod_radial$fitted,dat2[train,]$y) #training accuracy improve a lot
#testing data
pred.te=predict(bestmod_radial,newdata=dat2[-train,])
table(pred.te,dat2[-train,]$y) 







