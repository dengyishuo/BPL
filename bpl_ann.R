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

dat1 =  data.frame(x=data[,5:14],y=as.factor(m1))  
dat2 =  data.frame(x=data[,5:14],y=as.factor(m2))  


#function to avoid being stuck in local minimum
ann=function(x,y,size,maxit=2000,linout=F,try=100,decay = 5e-4,rang =0.0001) {
  ann1=nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,decay=0,rang = 0.0001)
  v1=ann1$value
  
  for (i in 2:try) {
    ann=nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,decay=0,rang = 0.0001)
    if (ann$value<v1) {
      v1=ann$value
      ann1=ann
    }
  }
  ann1
}  

#data scaling for whole data set
dat1[,1:10]= data.frame(apply(dat1[,c(1:10)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
dat2[,1:10]= data.frame(apply(dat2[,c(1:10)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
#random split training and testing data 80%-20%
n=nrow(dat1)
set.seed(1)
train=sample(n,n*0.8)

#classification 1
library(nnet)
set.seed(1)
ann1=ann(dat1[train,][,1:10],dat1[train,][,11],size =5,maxit =20000,try=100)
p1 = ifelse(ann1$fitted.values>0.5,1,0)
table(p1,dat1[train,"y"]) #training result
table(predict(ann1,dat1[-train,],type="class"),dat1[-train,'y']) #testing result 55%

#size 6
ann2 = ann(dat1[train,][,1:10],dat1[train,][,11],size =6,maxit =20000,try=100)
p2 = ifelse(ann2$fitted.values>0.5,1,0)
table(p2,dat1[train,"y"]) #training result
table(predict(ann2,dat1[-train,],type="class"),dat1[-train,'y']) #test result 56.57%
#with size =6, test accuarcy decrease

#size 4
ann3 = ann(dat1[train,][,1:10],dat1[train,][,11],size =4,maxit =20000,try=100)
p3 = ifelse(ann3$fitted.values>0.5,1,0)
table(p3,dat1[train,"y"]) #training result
table(predict(ann3,dat1[-train,],type="class"),dat1[-train,'y']) #test result 57.36

#size 3
ann4 = ann(dat1[train,][,1:10],dat1[train,][,11],size =3,maxit =20000,try=100)
p4 = ifelse(ann4$fitted.values>0.5,1,0)
table(p4,dat1[train,"y"]) #training result
table(predict(ann4,dat1[-train,],type="class"),dat1[-train,'y']) #test result 60.52%

#size 2
ann5 = ann(dat1[train,][,1:10],dat1[train,][,11],size =2,maxit =20000,try=100)
p5 = ifelse(ann5$fitted.values>0.5,1,0)
table(p5,dat1[train,"y"]) #training result
table(predict(ann5,dat1[-train,],type="class"),dat1[-train,'y']) #test result 62.36%

#size 1
ann6 = ann(dat1[train,][,1:10],dat1[train,][,11],size =1,maxit =20000,try=100)
p6 = ifelse(ann6$fitted.values>0.5,1,0)
table(p6,dat1[train,"y"]) #training result
table(predict(ann6,dat1[-train,],type="class"),dat1[-train,'y']) #test result 60.78%

#classification 2
#size 1
set.seed(1)
ann_1 = ann(dat2[train,][,1:10],dat2[train,][,11],size =1,maxit =20000,try=100)
p_1 = ifelse(ann_1$fitted.values>0.5,1,0)
table(p_1,dat2[train,"y"]) #training result
table(predict(ann_1,dat2[-train,],type="class"),dat2[-train,'y']) #test result 60.52

#size 2
ann_2 = ann(dat2[train,][,1:10],dat1[train,][,11],size =2,maxit =20000,try=100)
p_2 = ifelse(ann_2$fitted.values>0.5,1,0)
table(p_2,dat2[train,"y"]) #training result
table(predict(ann_2,dat2[-train,],type="class"),dat2[-train,'y']) #test result 55%

#size 3
ann_3 = ann(dat2[train,][,1:10],dat2[train,][,11],size =3,maxit =20000,try=100)
p_3 = ifelse(ann_3$fitted.values>0.5,1,0)
table(p_3,dat2[train,"y"]) #training result
table(predict(ann_3,dat2[-train,],type="class"),dat2[-train,'y']) #test result 61.3%

#size 4
ann_4 = ann(dat2[train,][,1:10],dat2[train,][,11],size =4,maxit =20000,try=100)
p_4 = ifelse(ann_4$fitted.values>0.5,1,0)
table(p_4,dat2[train,"y"]) #training result
table(predict(ann_4,dat2[-train,],type="class"),dat2[-train,'y']) #test result 55.5% seems over-fitting

#size 5
ann_5  =ann(dat2[train,][,1:10],dat2[train,][,11],size =5,maxit =20000,try=100)
p_5 = ifelse(ann_5$fitted.values>0.5,1,0)
table(p_5,dat2[train,"y"]) #training result
table(predict(ann_5,dat2[-train,],type="class"),dat2[-train,'y']) #57%




#visualize neural network
#seems have to use linear output to visualize the ann model


try=300
set.seed(3)
mod1<-nnet(dat1[train,][,1:10],as.numeric(dat1[train,][,11]),size=2,linout=T)
v1 = mod1$value
for (i in 1:try) {
  ann<-nnet(dat1[train,][,1:10],as.numeric(dat1[train,][,11]),size=2,maxit=1000,decay=0,rang = 0.0001,linout=T)
  if (ann$value<v1) {
    v1=ann$value
    ann1=ann
  }
}
mod1 = ann1

source('nnet_plot_update.r')
par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot(mod1)
plot(mod1,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.5,rel.rsc=15,
          circle.cex=7,cex=1.2,
          circle.col='brown')

#sensitivity analysis
source('lek_fun.r')
lek.fun(mod1)
head(lek.fun(mod1,val.out=T))


#try fewer parameter
a1 = ann(dat1[train,][,c(1:2,5:10)],dat1[train,][,11],size =2,maxit =20000,try=100)
p = ifelse(a1$fitted.values>0.5,1,0)
table(p,dat1[train,"y"]) #training result
table(predict(a1,dat1[-train,],type="class"),dat1[-train,'y']) #test result 60.52%
summary(a1)

a2 = ann(dat1[train,][,3:10],dat1[train,][,11],size =2,maxit =20000,try=100)
p = ifelse(a2$fitted.values>0.5,1,0)
table(p,dat1[train,"y"]) #training result
table(predict(a2,dat1[-train,],type="class"),dat1[-train,'y']) #test result 60.52%
summary(a2)


