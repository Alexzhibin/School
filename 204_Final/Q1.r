library(ggplot2)
set.seed(10086)
#setwd("~/Documents/Study/Hof/TextBook/IT204/Final")
Arrival = read.csv('Customer Arrival.csv')
names(Arrival) = 'Arr'
Service = runif(n=nrow(Arrival),min=3,max=12)
Customer = data.frame(arr=Arrival,ser=Service)
###Normal Test####
shapiro.test(Arrival$Arr)
##It failed. 
####Plot####
plot(Arrival$Arr)
#####Create Arrival Column###
Customer$arrive =NA
Customer$arrive[1]=Customer$Arr[1]
for (i in 2:nrow(Customer)){
  Customer$arrive[i]=Customer$Arr[i]-Customer$Arr[i-1]
}
plot(Customer$arrive,type="l")
##9 A.M. to 12 P.M. = 3 hours = 180 minutes 
block1= nrow(Customer[which(Customer$Arr<180),])
shapiro.test(Customer$arrive[1:block1]) ##0.2313, high significant 
ks.test(Customer$arrive[1:block1],"punif",min(Customer$arrive[1:block1]),max(Customer$arrive[1:block1]))
##12 P.M. to 3 P.M. = 3 hours = 180 minutes
block2= nrow(Customer[which(180<Customer$Arr & Customer$Arr<360),])
shapiro.test(Customer$arrive[block1+1:block2]) ##0.3293, high significant 
ks.test(Customer$arrive[block1+1:block2],"punif",min(Customer$arrive[block1+1:block2]),max(Customer$arrive[block1+1:block2]))

##3 P.M. to 5 P.M. = 2 hours = 120 minutes
block3= nrow(Customer[which(360<Customer$Arr),])
shapiro.test(Customer$arrive[block2+1:block3]) ##0.4942, high significant 
ks.test(Customer$arrive[block2+1:block3],"punif",min(Customer$arrive[block2+1:block3]),max(Customer$arrive[block2+1:block3]))



######Create Wait List#####
Customer$wait = rep(NA,nrow(Customer))
for(i in 2:nrow(Customer)){
  if(Customer[i-1,"ser"]>Customer[i,"arrive"]){
    Customer[i,"wait"] = Customer[i-1,"ser"]-Customer[i,"arrive"]
  }else {
    Customer[i,"wait"] = 0
  }
}
Customer[1,"wait"] = 0
###Normal Test####
shapiro.test(Customer$wait[1:block1]) 
shapiro.test(Customer$wait[block1+1:block2]) ##Not significant; 0.003428
shapiro.test(Customer$wait[block2+1:block3])

###Visulization
plot(Customer$wait,type="h")


#####Window Open####
#From plot
#1-- 37 2 windows
#37 -- 69 4 windows
#70--89 3 windows
#90--117 5 windows
#118--150 4 windows
#150--200 3 windows
#200-251 2 windows

##########
#Create Simulation
#########
set.seed(10086)
New_Data = data.frame(Arrive=rep(NA,251),Service=rep(NA,251),
                      Block=c(rep('Block1',block1),rep('Block2',block2),rep('Block3',block3)))
New_Data$Arrive =c(rnorm(block1,mean=mean(Customer$arrive[1:block1]),sd=sd(Customer$arrive[1:block1])),
                   rnorm(block2,mean=mean(Customer$arrive[block1+1:block2]),sd=sd(Customer$arrive[block1+1:block2])),
                   rnorm(block3,mean=mean(Customer$arrive[block2+1:block3]),sd=sd(Customer$arrive[block2+1:block3]))
                   )

New_Data$Service = c(runif(block1,min=min(Customer$ser[1:block1]),max=max(Customer$ser[1:block1])),
                     runif(block2,min=min(Customer$ser[block1+1:block2]),max=max(Customer$ser[block1+1:block2])),
                     runif(block3,min=min(Customer$ser[block2+1:block3]),max=max(Customer$ser[block2+1:block3]))
)
####Make Negative to Zero###
for(i in 1:nrow(New_Data)){
  if(New_Data[i,"Arrive"]<0){
    New_Data[i,"Arrive"] = 0
  }
  if(New_Data[i,"Service"]<0){
    New_Data[i,"Service"] = 0
  }
}


######Create Wait List#####
New_Data$wait = rep(NA,nrow(New_Data))
for(i in 2:nrow(New_Data)){
  if(New_Data[i-1,"Service"]>New_Data[i,"Arrive"]){
    New_Data[i,"wait"] = New_Data[i-1,"Service"]-New_Data[i,"Arrive"]
  }else {
    New_Data[i,"wait"] = 0
  }
}
New_Data[1,"wait"] = 0

####Visulation####
###Density###
##Arrive
p1<-ggplot(New_Data,main="Arrive", aes(New_Data$Arrive)) + geom_density(aes(group=Block,colour=Block))
p1+ggtitle("Density Plot of Arrive")
##Serive
p2<-ggplot(New_Data, aes(New_Data$Service)) + geom_density(aes(group=Block,colour=Block))
p2+ggtitle("Density Plot of Serive")
##Wait
p3<-ggplot(New_Data, aes(New_Data$wait)) + geom_density(aes(group=Block,colour=Block))
p3+ggtitle("Density Plot of wait")

###Time-line###
par(mfrow=c(1,2))
plot(New_Data$Arrive,type="h",ylab="Arrive Time",xlab="Minute",main="Plot of Arrival")
plot(New_Data$Service,type="h",ylab="Service Time",xlab="Minute",main="Plot of Service")
par(mfrow=c(1,1))
plot(New_Data$wait,type="h",ylab="wait Time",xlab="Minute",main="Plot of wait")

#####Window Open####
#From plot
#1-- 37 2 windows
#37 -- 69 4 windows
#70--89 3 windows
#90--117 5 windows
#118--150 4 windows
#150--200 3 windows
#200-251 2 windows

cost_month = (2+4+3+5+4+3+2)*20*5*4
cost_month










