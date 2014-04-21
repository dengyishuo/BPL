#EWMA on logistic regression

#first apply logistic regression to obtain coefficients


#for games of each team at home and away
d1 = read.csv("PL2012-13.csv",header = T) 
label = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","HS","AS","HST","AST","HC","AC","HF","AF","HR","AR","Lambda.H.","Lambda.A.")
data = d1[,label]


Arsenal_Home = data[data[2]=='Arsenal',]
ts.plot(Arsenal_Home$HS)
acf(Arsenal_Home$HS)
pacf(Arsenal_Home$HS)
















