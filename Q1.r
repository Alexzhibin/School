#setwd("~/Documents/Study/Hof/TextBook/IT204/Midterm")
Data = read.csv("DriveThru.csv")
##1.  distributions of arrival times for each of the blocks of time
##6 hours = 6*60 minutes =360 minutes = 1 block 
### 0~360 Late Night; 361~720 Breakfast; 721~1080 Lunch; 1081~1440 Dinner
Data$block = NA
for(i in (1:nrow(Data))){
if  (Data$x[i]>0 & 360>=Data$x[i]){
  Data$block[i] = "Block 1"
}
if  (Data$x[i]>360 & 720>=Data$x[i]){
  Data$block[i] = "Block 2"
}
if  (Data$x[i]>720 & 1080>=Data$x[i]){
  Data$block[i] = "Block 3"
}
if  (Data$x[i]>1080 & 1440>=Data$x[i]){
  Data$block[i] = "Block 4"
}
if  (Data$x[i]>1440){
  Data$block[i] = "Block 1"
}
}
#Calculate the arrival data
Data$arrive =NA
Data$arrive[1]=Data$x[1]
for (i in 2:nrow(Data)){
  Data$arrive[i]= Data$x[i]-Data$x[(i-1)]
}
##Split the data
Block1 = Data[which(Data$block=="Block 1"),]
Block2 = Data[which(Data$block=="Block 2"),]
Block3 = Data[which(Data$block=="Block 3"),]
Block4 = Data[which(Data$block=="Block 4"),]



###Visulization
library(ggplot2)
p1<-ggplot(Data, aes(Data$arrive)) + geom_density(aes(group=block,colour=block))
p1

ggplot(Block1, aes(Block1$arrive)) + geom_density()
ggplot(Block2, aes(Block2$arrive)) + geom_density()
ggplot(Block3, aes(Block3$arrive)) + geom_density()
ggplot(Block4, aes(Block4$arrive)) + geom_density()

###########
#Distribution Check 
###########
#####Normality 
shapiro.test(Block1$arrive)
shapiro.test(Block2$arrive)
shapiro.test(Block3$arrive)
shapiro.test(Block4$arrive)
##All data belong to normal distribution, because all P value are greater than 0.1
mean_sd = function(Block){
  v1 = mean(Block$arrive)
  v2= sd(Block$arrive)
  return (list(mean= v1,sd= v2))
}

mean_sd(Block1)
mean_sd(Block2)
mean_sd(Block3)
mean_sd(Block4)

########
#Q4
########
nrow(Block1)
nrow(Block2)
nrow(Block3)
nrow(Block4)


########
#Q5
########
set.seed(10086)
#plot(x=seq(1,10),y=dexp(seq(1,10)),type="l")
sim_arrive = function(num,block){
  if(block==1){
   arr = rnorm(num,mean=6.12,sd=2.29)
  }
  if(block==2){
    arr = rnorm(num,mean=2.04,sd=0.91)
  }
  if(block==3){
    arr = rnorm(num,mean=1.42,sd=0.52)
  }
  if(block==4){
    arr = rnorm(num,mean=1.10,sd=0.37)
  }
  return(arr)
}

sim_service = function(block){
  if(block==1){
    service = rexp(n=59,rate=0.8)
  }
  if(block==2){
    service = rexp(n=176,rate=0.8)
  }
  if(block==3){
    service = rexp(n=253,rate=0.8)
  }
  if(block==4){
    service = rexp(n=327,rate=0.8)
  }
  return(service)
}

service1 = sim_service(1)
service2 = sim_service(2)
service3 = sim_service(3)
service4 = sim_service(4)
mean(service1)
mean(service2)
mean(service3)
mean(service4)

###Add the service time to each blocks
Block1$service = service1
Block2$service = service2
Block3$service = service3
Block4$service = service4

###Calculate the wait time for each blocks 
#Block1
Block1$wait = rep(NA,nrow(Block1))
for(i in 2:nrow(Block1)){
if(Block1[i-1,"service"]>Block1[i,"arrive"]){
  Block1[i,"wait"] = Block1[i-1,"service"]-Block1[i,"arrive"]
}else {
  Block1[i,"wait"] = 0
}
}
Block1[1,"wait"] = 0
#Block2
Block2$wait = rep(NA,nrow(Block2))
for(i in 2:nrow(Block2)){
  if(Block2[i-1,"service"]>Block2[i,"arrive"]){
    Block2[i,"wait"] = Block2[i-1,"service"]-Block2[i,"arrive"]
  }else {
    Block2[i,"wait"] = 0
  }
}
Block2[1,"wait"] = 0
#Block3
Block3$wait = rep(NA,nrow(Block3))
for(i in 2:nrow(Block3)){
  if(Block3[i-1,"service"]>Block3[i,"arrive"]){
    Block3[i,"wait"] = Block3[i-1,"service"]-Block3[i,"arrive"]
  }else {
    Block3[i,"wait"] = 0
  }
}
Block3[1,"wait"] = 0
#Block4
Block4$wait = rep(NA,nrow(Block4))
for(i in 2:nrow(Block4)){
  if(Block4[i-1,"service"]>Block4[i,"arrive"]){
    Block4[i,"wait"] = Block4[i-1,"service"]-Block4[i,"arrive"]
  }else {
    Block4[i,"wait"] = 0
  }
}
Block4[1,"wait"] = 0

##Mean of wait time for each Block
mean(Block1$wait)
mean(Block2$wait)
mean(Block3$wait)
mean(Block4$wait)

###Plot the wait time
par(mfrow=c(2,2))
plot(Block1$wait,type="h")
plot(Block2$wait,type="h")
plot(Block3$wait,type="h")
plot(Block4$wait,type="h")


####95% confiden interval for the average wait time###
ci1 = qexp(0.975)*sd(Block1$wait)/sqrt(nrow(Block1))
ci2 = qexp(0.975)*sd(Block2$wait)/sqrt(nrow(Block2))
ci3 = qexp(0.975)*sd(Block3$wait)/sqrt(nrow(Block3))
ci4 = qexp(0.975)*sd(Block4$wait)/sqrt(nrow(Block4))

mean(Block1$wait)+ci1
mean(Block1$wait)-ci1
mean(Block2$wait)+ci2
mean(Block2$wait)-ci2
mean(Block3$wait)+ci3
mean(Block3$wait)-ci3
mean(Block4$wait)+ci4
mean(Block4$wait)-ci4




