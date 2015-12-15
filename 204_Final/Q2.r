###Get the data###
require(quantmod)
require(ggplot2)
Sys.setenv(TZ="GMT")
###Google Stock
getSymbols('GOOG',from='2014-12-01')
Google=data.frame(d=index(Cl(GOOG)),Close=as.numeric(Cl(GOOG)))
###WALMART Stock
getSymbols('WMT',from='2014-12-01')
Walmart=data.frame(d=index(Cl(WMT)),Close=as.numeric(Cl(WMT)))
###WALGREENS Stock
getSymbols('WBA',from='2014-12-01')
Walgreens=data.frame(d=index(Cl(WBA)),Close=as.numeric(Cl(WBA)))
###VERIZON Stock
getSymbols('VZ',from='2014-12-01')
Verizon=data.frame(d=index(Cl(VZ)),Close=as.numeric(Cl(VZ)))
###VERIZON Stock
getSymbols('XOM',from='2014-12-01')
Exxon=data.frame(d=index(Cl(XOM)),Close=as.numeric(Cl(XOM)))
#Remove other data
rm(GOOG,WMT,WBA,VZ,XOM)
####
set.seed(10086)
###########
#Google
##########
Google$U = rep(NA,nrow(Google))
for(i in 1:nrow(Google)-1){
  Google[i,'U'] = log(Google[i+1,'Close']/Google[i,'Close'])
}
Ubar_g = sum(Google$U,na.rm=T)/(nrow(Google)-1)#0.001386795

Google$S2 = (Google$U-Ubar_g)^2
S2_g = sum(Google$S2,na.rm=T)/(nrow(Google)-1)
S2_g #0.0003267567

d_google = (Ubar_g+S2_g/2)/(1/365)
d_google #0.5305304
v_google = sqrt(S2_g)/365/(1/365)
v_google #0.01800293

###########
#Walmart
##########
Walmart$U = rep(NA,nrow(Walmart))
for(i in 1:nrow(Walmart)-1){
  Walmart[i,'U'] = log(Walmart[i+1,'Close']/Walmart[i,'Close'])
}
Ubar_w = sum(Walmart$U,na.rm=T)/(nrow(Walmart)-1)#0.001386795

Walmart$S2 = (Walmart$U-Ubar_w)^2
S2_w = sum(Walmart$S2,na.rm=T)/(nrow(Walmart)-1)
S2_w #0.0001693606

d_Walmart = (Ubar_w+S2_w/2)/(1/365)
d_Walmart #-0.4672016
v_Walmart = sqrt(S2_w)/(1/365)
v_Walmart #4.738866

###########
#Walgreens
##########
Walgreens$U = rep(NA,nrow(Walgreens))
for(i in 1:nrow(Walgreens)-1){
  Walgreens[i,'U'] = log(Walgreens[i+1,'Close']/Walgreens[i,'Close'])
}
Ubar_wa = sum(Walgreens$U,na.rm=T)/(nrow(Walgreens)-1)#0.001386795

Walgreens$S2 = (Walgreens$U-Ubar_wa)^2
S2_wa = sum(Walgreens$S2,na.rm=T)/(nrow(Walgreens)-1)
S2_wa #0.0003168572

d_Walgreens = (Ubar_wa+S2_wa/2)/(1/365)
d_Walgreens #0.3334077
v_Walgreens = sqrt(S2_wa)/(1/365)
v_Walgreens #6.46992


###########
#Verizon
##########
Verizon$U = rep(NA,nrow(Verizon))
for(i in 1:nrow(Verizon)-1){
  Verizon[i,'U'] = log(Verizon[i+1,'Close']/Verizon[i,'Close'])
}
Ubar_v = sum(Verizon$U,na.rm=T)/(nrow(Verizon)-1)#0.001386795

Verizon$S2 = (Verizon$U-Ubar_v)^2
S2_v = sum(Verizon$S2,na.rm=T)/(nrow(Verizon)-1)
S2_v #0.0001047888

d_Verizon = (Ubar_v+S2_v/2)/(1/365)
d_Verizon #-0.1151765
v_Verizon = sqrt(S2_wa)/(1/365)
v_Verizon #6.46992


###########
#Exxon
##########
Exxon$U = rep(NA,nrow(Exxon))
for(i in 1:nrow(Exxon)-1){
  Exxon[i,'U'] = log(Exxon[i+1,'Close']/Exxon[i,'Close'])
}
Ubar_e = sum(Exxon$U,na.rm=T)/(nrow(Exxon)-1)#0.001386795

Exxon$S2 = (Exxon$U-Ubar_e)^2
S2_e = sum(Exxon$S2,na.rm=T)/(nrow(Exxon)-1)
S2_e #0.0002027618

d_Exxon = (Ubar_e+S2_e/2)/(1/365)
d_Exxon #-0.2347795
v_Exxon = sqrt(S2_e)/(1/365)
v_Exxon #5.208565




#########
#Simulation
f = g = rep(0, times=365*5)

###
dt=1/1095
365*5==1825
###

simulation= function(mu,sigma,g1,g,dt){
i=1
g[1]=g1
while(i < 1825){
  i = i+1
  # Represents the stochastic portion
  if(rbinom(1,1,0.5)==1){
    k = 1
  }else{
    k = -1
  }
  # Now calculate the deterministic with the stochastic
  # 3 time for each day, so multiple for 3 time
  g[i] <- g[i-1]*exp(mu*dt+k*sigma*sqrt(dt))
  i = i+1
  if(rbinom(1,1,0.5)==1){
    k = 1
  }else{
    k = -1
  }
  g[i] <- g[i-1]*exp(mu*dt+k*sigma*sqrt(dt))
  i = i +1
  if(rbinom(1,1,0.5)==1){
    k = 1
  }else{
    k = -1
  }
  g[i] <- g[i-1]*exp(mu*dt+k*sigma*sqrt(dt))

}
return (g)
}
##rep function
mean_rep_sim= function(func,sim){
  n=1
  sim_tem =rep(0,length(sim))
  while(n < 10){
    sim_tem =sim_tem+func
    n=n+1
  }
  rep_sim = sim_tem/10
  return(rep_sim)
}
#####Google#####
google1=Google$Close[1]
sim_google = simulation(d_google,v_google,google1,g,dt)
head(sim_google,20)
sim_google_10 = mean_rep_sim(simulation(d_google,v_google,google1,g,dt),sim_google)
ci_google = qexp(0.975)*sd(sim_google_10)/sqrt(length(sim_google_10))
mean(sim_google_10) #755.7168
re_g=mean(sim_google_10)-google1

#####Walmart#####
Walmart1=Walmart$Close[1]
sim_walmart = simulation(d_Walmart,v_Walmart,Walmart1,g,dt)
head(sim_walmart,20)
sim_walmart_10 = mean_rep_sim(simulation(d_Walmart,v_Walmart,Walmart1,g,dt),sim_walmart)
ci_walmart = qexp(0.975)*sd(sim_walmart_10)/sqrt(length(sim_walmart_10))
mean(sim_walmart_10) # 93.11304
re_wmt=mean(sim_walmart_10)-Walmart1

#####Walgreens#####
Walgreens1=Walgreens$Close[1]
sim_walgreens = simulation(d_Walgreens,v_Walgreens,Walgreens1,g,dt)
head(sim_walgreens,20)
sim_walgreens_10 = mean_rep_sim(simulation(d_Walgreens,v_Walgreens,Walgreens1,g,dt),sim_walgreens)
ci_walgreens = qexp(0.975)*sd(sim_walgreens_10)/sqrt(length(sim_walgreens_10))
mean(sim_walgreens_10) #459.8935
re_wg=mean(sim_walgreens_10)-Walgreens1

#####Verizon#####
Verizon1=Verizon$Close[1]
sim_verizon = simulation(d_Verizon,v_Verizon,Verizon1,g,dt)
head(sim_verizon,20)
sim_verizon_10 = mean_rep_sim(simulation(d_Verizon,v_Verizon,Verizon1,g,dt),sim_verizon)
ci_verizon = qexp(0.975)*sd(sim_verizon_10)/sqrt(length(sim_verizon_10))
mean(sim_verizon_10) #120.9025
re_ver=mean(sim_verizon_10)-Verizon1

#####Exxon#####
Exxon1=Exxon$Close[1]
sim_Exxon = simulation(d_Exxon,v_Exxon,Exxon1,g,dt)
head(sim_Exxon,20)
sim_Exxon_10 =  mean_rep_sim(simulation(d_Exxon,v_Exxon,Exxon1,g,dt),sim_Exxon)
ci_Exxon = qexp(0.975)*sd(sim_Exxon_10)/sqrt(length(sim_Exxon_10))
mean(sim_Exxon_10) #132.8888
re_ex=mean(sim_Exxon_10)-Exxon1

###Portfolio 
re_all=re_g+re_wmt+re_wg+re_ver+re_ex # 732.3423
re_g/re_all;re_wmt/re_all;re_wg/re_all;re_ver/re_all;re_ex/re_all


###Visualization###


###Load the sp500 data
sp=read.csv("sp500.csv")
##reverse 
sp = sp[order(sp$Date),]
##let's plot the 262 days of all the datas
sp_price = sp$Adj.Close
plot(sp_price,type="l",ylim=c(50,2150),ylab="S&P_500 Price",xlab="Days",main="5Stocks v.s.S&P500")
lines(sim_walmart_10,col=4,lwd=2)
lines(sim_walgreens_10,col=11,lwd=2)
lines(sim_verizon_10,col=6,lwd=2)
lines(sim_Exxon_10,lwd=2,col=8)
lines(sim_google_10,,lwd=2,col="red")
a=legend("left",legend=c('Google',"Walmart","Walgreens","Verizon","Exxon","S&P500"),
       pt.cex=4,cex=0.75,text.col = c("red",4,11,6,8,"black"),y.intersp=0.8,plot=F)
# box size reduced by factor 0.75
a=a$rect
mid = a$top - 0.5*a$h
reduction = 0.75

# draw new box
rect(xleft=a$left, ytop=mid+0.5*reduction*a$h, xright=a$left+a$w, ybottom=mid-0.5*reduction*a$h)
# add legend items to new box
legend("left",legend=c('Google',"Walmart","Walgreens","Verizon","Exxon","S&P500"),
       pt.cex=4,cex=0.75,text.col = c("red",4,11,6,8,"black"),y.intersp=0.8,bty='n')

