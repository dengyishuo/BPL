#recent 5 seasons, 1900 observations

#seems no statistics on offsides?
#HomeTeam = Home Team 
#AwayTeam = Away Team 
#FTHG = Full Time Home Team Goals
#FTAG = Full Time Away Team Goals
#HS = Home Team Shots
#AS = Away Team Shots
#HST = Home Team Shots on Target
#AST = Away Team Shots on Target
#HC = Home Team Corners
#AC = Away Team Corners
#HF = Home Team Fouls Committed
#AF = Away Team Fouls Committed
#HR = Home Team Red Cards
#AR = Away Team Red Cards

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

#label 4 class
# m1 = I(Home goal - lambda_H)
num_class =4
m1 = ifelse((H_minus_lambda + A_minus_lambda>=0),1,0)
m2 = ifelse((H_minus_lambda - A_minus_lambda>=0),1,0)
m3 = as.matrix(cbind(m1,m2))
Res_class  =apply(m3,1,function(x){ifelse(sum(x)==0,1,
                            ifelse(x[1]==1 & x[2]==0,2,
                            ifelse(x[1]==0 & x[2]==1,3,4)))})

#combine predictor and response
d = cbind(data[,3:14],Res_class)

#can perform any statistical test?
#average home/away team shots under 4 class
with(d,do.call(rbind,tapply(HS, Res_class,function(x)c(M=mean(x),SD=sd(x)))))
with(d,do.call(rbind,tapply(AS, Res_class,function(x)c(M=mean(x),SD=sd(x)))))
with(d,do.call(rbind,tapply(HST, Res_class,function(x)c(M=mean(x),SD=sd(x)))))
with(d,do.call(rbind,tapply(AST, Res_class,function(x)c(M=mean(x),SD=sd(x)))))
#average home/away team red cards under 4 class
with(d,do.call(rbind,tapply(HR, Res_class,function(x)c(M=mean(x),SD=sd(x)))))
with(d,do.call(rbind,tapply(AR, Res_class,function(x)c(M=mean(x),SD=sd(x)))))


#construct target table for logistic output in nueral network
target = matrix(c(rep(0,nrow(d)*num_class)),nrow=nrow(d),ncol=num_class)
for( i in 1:nrow(d)){
  target[i,d[i,13]]=1
}

library(nnet)
#apply Feed-forward Neural Networks 
#seems wired why fixed result?
#is normalization necessary?
ann = nnet(d[,1:12],target,size =10,decay=0,rang = 0,maxit =20000)
#why all predict class 4?
p = max.col(ann$fit)
table(p,Res_class)


#try Multinomial Logistic Regression
test<-multinom(Res_class~.,data=d[,3:13])
summary(test)
#how to performing model diagnostics?
z<-summary(test)$coefficients/summary(test)$standard.errors
#The multinom package does not include p-value calculation for the regression coefficients, 
#so calculate p-values using Wald tests (here z-tests).

(p<-(1-pnorm(abs(z),0,1))*2)  # 2-tailed z test

exp(coef(test)) #how to explain coefficients?
pre = max.col(fitted(test)) #predicted probabilities for each of our outcome levels
table(pre,Res_class) #training table





















