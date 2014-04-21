
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

#input: raw data
#out put: list contains: data, no. of week, three mean for each week
#for weeks with no game, simply remove that week
process_date = function(data){
  data[,"Date"]  = as.Date(data[,"Date"], "%d/%m/%Y")
  data = data[order(data[,"Date"]),]
  data[,"Date"] = as.numeric(data[,"Date"] - data[1,"Date"]) %/% 7
  #for different season, number of week may different
  nweek = max(data[,"Date"])+1
  
  # goal_H + goal_A
  actual_total_goal = data[,"FTHG"]+data[,"FTAG"]
  implied_total_goal = data[,"Lambda.H."]+data[,"Lambda.A."]
  data = data.frame(data,actual_total_goal,implied_total_goal)
  #for each week plot 
  a1 = rep(0,nweek) #store average actual total goal for each week
  a2 = rep(0,nweek)#store average implied total goal for each week
  a3 = rep(0,nweek) #ratio
  
  for (i in 1 : nweek){
    d = data[,"Date"] == (i-1)
    a1[i] = mean(data[d,"actual_total_goal"])
    a2[i] = mean(data[d,"implied_total_goal"])
    a3[i] = a1[i]/a2[i]
    
  }
  mean_actual = a1[!is.na(a1)]
  mean_implied = a2[!is.na(a2)]
  ratio = a3[!is.na(a3)]
  list(data, nweek,mean_actual,mean_implied,ratio)
}

require(ggplot2)
for(i in 1 :8) {
  assign((paste("list",i,sep="")),process_date(get(paste("data",i,sep=""))))
  temp = get(paste("list",i,sep=""))
  #plot(temp[[3]],type="l",main = paste("Average Total Goal for week",i,sep=" "))
  #plot(temp[[4]],type="l",main = paste("Average Implied Goal for week",i,sep=" "))
  #plot(temp[[5]],type="l",main = paste("Ratio of actual over implied for week",i,sep=" "))
  #ts.plot(as.ts(temp[[3]]),as.ts(temp[[4]]))
  
  #r = range(temp[[3]])
  df = data.frame(temp[[3]],temp[[4]],temp[[5]]) 
  ts.plot(df,lty=c(1:3),main = paste("For season",i,sep=" "),ylim = c(0,5.2))
  ts.name = c("Average Total Goal","Average Implied Goal","Ratio of actual over implied")
  legend(0.5,5.2,ts.name,lty=c(1:3),cex=0.5)
}










