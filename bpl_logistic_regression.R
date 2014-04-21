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

#data scaling for whole data set
#dat1[,1:9]= data.frame(apply(dat1[,c(1:9)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
#dat2[,1:9]= data.frame(apply(dat2[,c(1:9)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
#random split training and testing data 80%-20%
n=nrow(dat1)
set.seed(1)
train=sample(n,n*0.8)

summary(dat1)
library(aod)
library(ggplot2)

#logistic regression for classidfication problem 1
mylogit_1 <- glm(y ~., data = dat1[train,], family = "binomial")
summary(mylogit_1)
confint(mylogit_1)
anova(mylogit_1, test="Chisq")

#model selection based on AIC
require("MASS")
m1=stepAIC(glm(y ~., data = dat1[train,], family = "binomial"))
p_m=predict(m1, dat1[-train,], type="response") 
p_m = ifelse(p_m>0.5,1,0)
table(p_m,dat1[-train,"y"])   #around 61.31%

#predict
p = ifelse(mylogit_1$fitted.value>0.5,1,0)
table(p,dat1[train,"y"])
p1=predict(mylogit_1, dat1[-train,], type="response") 
p1 = ifelse(p1>0.5,1,0)
table(p1,dat1[-train,"y"])  #prediction accuracy=61.84% ANN seems have no advantage at all compare to this simple model!


#logistic regression for classidfication problem 2
mylogit_2 <- glm(y ~., data = dat2[train,], family = "binomial")
summary(mylogit_2)
confint(mylogit_2)
anova(mylogit_2, test="Chisq")


#predict
p2=predict(mylogit_2, dat2[-train,], type="response") 
p2 = ifelse(p2>0.5,1,0)
table(p2,dat2[-train,"y"])  #prediction accuracy=60.53%











