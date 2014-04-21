

#*****************************************************************************#
#define functions to process the data 
#revise done
process_data = function(data){
H_minus_lambda = data[,"FTHG"] - data[,"Lambda.H."]
A_minus_lambda = data[,"FTAG"] - data[,"Lambda.A."]

total = ifelse((H_minus_lambda + A_minus_lambda>=0),1,0)
difference = ifelse((H_minus_lambda - A_minus_lambda>=0),1,0)

#for total goal 
data = data.frame(data[,1:5],H_level=rep(0,380),A_level=rep(0,380),total_goal=(H_minus_lambda+A_minus_lambda),H_minus_lambda,A_minus_lambda,y=as.factor(total))
data
} 

#function to classify team into 2 level: strong & not strong
#based on whether a team's average goal per match exceeds 1.6 times of average goal for all 20 teams
#input: a data frame contains all games in one season with FTHG & FTAG
#output: return data frame with 
#set four class use two columns
#1-1 Strong vs Strong
#1-0 Strong vs not Strong
#0-1 not Strong vs Strong
#0-0 not Strong vs not Strong
classify.strong = function(data,k=1.3){
#classify team level- strong or not strong
team = unique(data[,"HomeTeam"])
#compute average goal per match for each team
average_goal = rep(0,20)
for (i in 1:20){
  d_H = data[data[,"HomeTeam"]==team[i],]
  d_A = data[data[,"AwayTeam"]==team[i],]
  average_goal[i] = (sum(d_H[,"FTHG"])+sum(d_A[,"FTAG"]))/38
}

c = mean(average_goal)*k #can adjust this threshold
strong = team[average_goal>=c]
data[,"H_level"]=ifelse(data[,"HomeTeam"] %in% strong,1,0)
data[,"A_level"]=ifelse(data[,"AwayTeam"] %in% strong,1,0)
data
}

#manupulate only on total goal based on number of lag
add_previous_performance = function(data,lag=3){
  
dat_copy = data #copy to further copy
temp = rep(0,380)
team = unique(data[,"HomeTeam"])

#add colums to store previous performance
for(i in 1:lag){
#dat_copy = data.frame(dat_copy,assign(paste("Home_p",i,sep=""),temp))
  dat_copy[paste("Home_p",i,sep="")]=temp
}
for(i in 1:lag){
  #dat_copy = data.frame(dat_copy,assign(paste("Home_p",i,sep=""),temp))
  dat_copy[paste("Away_p",i,sep="")]=temp
}

#a for loop for all 20 teams
for (i in 1:20){
d = data[data[,"HomeTeam"]==team[i] | data[,"AwayTeam"]==team[i],] #select all matches for team i  in one season
performance = rep(0,nrow(d)) #store 
performance = ifelse(d[,"HomeTeam"]==team[i],d[,"H_minus_lambda"],d[,"A_minus_lambda"]) #select team i's performance 
d = data.frame(d,Team = team[i],performance) #remove unneeded columns


#attach previous lag matches's performance to dataframe d
#problem!!!
for( k in 38:(lag+1)){
  #m_i to store privious i-th match's performance
  for(n in 1 : lag) assign(paste("m",n,sep="_"),d[k-n,"performance"])
  
  temp = dat_copy[dat_copy[,"Date"]==d[k,"Date"],]
  if (sum(temp[,"HomeTeam"]==d[k,"Team"])) # add to home team columns
  {
    for (n in 1:lag) dat_copy[dat_copy[,"Date"]==d[k,"Date"] & (dat_copy[,"HomeTeam"]==d[k,"Team"]),paste("Home_p",n,sep="")]=get(paste("m",n,sep="_"))
  }
  if (sum(temp[,"AwayTeam"]==d[k,"Team"])) # add to away team columns
  {
    for (n in 1:lag) dat_copy[dat_copy[,"Date"]==d[k,"Date"] & (dat_copy[,"AwayTeam"]==d[k,"Team"]),paste("Away_p",n,sep="")]=get(paste("m",n,sep="_"))
  }
  
}
}

#discard first several matches
dat_copy[(lag*11):nrow(dat_copy),] 
}

#function return best ann model given number of trials
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
#***********************************************************************************#


#process data- 8 season 380*8 = 3040 onservations
d1 = read.csv("PL2012-13.csv",header = T) 
d2 = read.csv("PL2011-12.csv",header = T) 
d3 = read.csv("PL2010-11.csv",header = T) 
d4 = read.csv("PL2009-10.csv",header = T) 
d5 = read.csv("PL2008-09.csv",header = T) 
d6 = read.csv("PL2007-08.csv",header = T) 
d7 = read.csv("PL2006-07.csv",header = T) 
d8 = read.csv("PL2005-06.csv",header = T) 



label = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","Lambda.H.","Lambda.A.")
data1 = d1[,label]
data2 = d2[,label]
data3 = d3[,label]
data4 = d4[,label]
data5 = d5[,label]
data6 = d6[,label]
data7 = d7[,label]
data8 = d8[,label]

#apply functions to process raw data sets
data = data.frame()
label1 = c("Date","FTHG","FTAG","total_goal","H_minus_lambda","A_minus_lambda")
for (i in 1:8){
  assign(paste("data",i,sep=""),process_data(get(paste("data",i,sep=""))))
  assign(paste("data",i,sep=""),classify.strong(get(paste("data",i,sep="")),k=1.3))
  assign(paste("data",i,sep=""),add_previous_performance(get(paste("data",i,sep="")),lag=3))
  assign(paste("data",i,sep=""),get(paste("data",i,sep=""))[ , -which(names(get(paste("data",i,sep=""))) %in% label1)])
  data = rbind(data,get(paste("data",i,sep="")))
}
data = data[c(-5,-6),]
#data store all the observations
#select training set
(n = nrow(data))
set.seed(1)
train=sample(n,n*0.8)

#try ann 
library(nnet)
set.seed(1)

#run on whole data set
#try lag = 3
ann1=ann(data[train,c(3,4,6:11)],data[train,"y"],size =4,maxit =20000,try=100)
summary(ann1)
p1 = ifelse(ann1$fitted.values>0.5,1,0)
table(p1,data[train,"y"]) #training result
table(predict(ann1,data[-train,],type="class"),data[-train,'y']) #approximately 50%....

########################################
#get data for lag = 10
label = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","Lambda.H.","Lambda.A.")
data1 = d1[,label]
data2 = d2[,label]
data3 = d3[,label]
data4 = d4[,label]
data5 = d5[,label]
data6 = d6[,label]
data7 = d7[,label]
data8 = d8[,label]


#lag  =10
data = data.frame()
label1 = c("Date","FTHG","FTAG","total_goal","H_minus_lambda","A_minus_lambda")
for (i in 1:8){
  assign(paste("data",i,sep=""),process_data(get(paste("data",i,sep=""))))
  assign(paste("data",i,sep=""),classify.strong(get(paste("data",i,sep="")),k=1.3))
  assign(paste("data",i,sep=""),add_previous_performance(get(paste("data",i,sep="")),lag=10))
  assign(paste("data",i,sep=""),get(paste("data",i,sep=""))[ , -which(names(get(paste("data",i,sep=""))) %in% label1)])
  data = rbind(data,get(paste("data",i,sep="")))
}

data = data[c(-3,-4),]
#add mean and moving average
n =  nrow(data)
#mean
average_H = rep(0,n)
average_A = rep(0,n)
mv_H = rep(0,n)
mv_A = rep(0,n)
attach(data)
lambda = 0.94
for (i in 1:10){
  average_H = average_H + get(paste("Home_p",i,sep=""))/10
  average_A = average_A + get(paste("Away_p",i,sep=""))/10
  mv_A = mv_A + lambda ^ i * get(paste("Home_p",i,sep=""))/(sum(lambda^(1:10)))
  mv_H = mv_H + lambda ^ i * get(paste("Away_p",i,sep=""))/(sum(lambda^(1:10)))
}


detach(data)
data = data[,c(-9:-15,-19:-25)]
data["average_H"] = average_H
data["average_A"] = average_A
data["mv_H"] = mv_H
data["mv_A"] = mv_A


(n = nrow(data))
set.seed(1)
train=sample(n,n*0.8)

#try ann 
library(nnet)
set.seed(1)
ann2=ann(data[train,c(3,4,6:15)],data[train,"y"],size =6,maxit =20000,try=100)
summary(ann2)
p2 = ifelse(ann2$fitted.values>0.5,1,0)
table(p2,data[train,"y"]) #training result
table(predict(ann2,data[-train,],type="class"),data[-train,'y']) #even lower than 50%.... 



#logisticregression on previous 3 + mean + EWMA
library(aod)
library(ggplot2)
mylogit <- glm(y ~., data = data[train,c(3:15)], family = "binomial")
summary(mylogit) # no significant coef ...
#prediction
p = ifelse(mylogit$fitted.value>0.5,1,0) 
table(p,data[train,"y"]) #training is somehow like random guess
p1=predict(mylogit, data[-train,], type="response") 
p1 = ifelse(p1>0.5,1,0)
table(p1,data[-train,"y"])  #50% no prediction ability...


#AIC model selection
require("MASS")
m1=stepAIC(glm(y ~., data = data[train,c(3:15)], family = "binomial"))
summary(m1)
p_m=predict(m1, data[-train,], type="response") 
p_m = ifelse(p_m>0.5,1,0)
table(p_m,data[-train,"y"])   











