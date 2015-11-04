data = read.csv("Q2.csv")
data$pop_30y = NA

############
##Create simulation function
############
sim_bir=function(Country){
bir_grow = data[which(data$X==Country),"Birth.Rate.Per.Cap"]
bir = log(bir_grow)
bir_rate =rep(NA,30)
for(i in 1:30){
bir_rate[i] = exp(bir)
bir = bir-0.2
}
return(bir_rate)
}
sim_bir("United States")

sim_death=function(Country){
death_grow = data[which(data$X==Country),"Death.Rate.Per.Cap"]
death = log(death_grow)
death_rate =rep(NA,30)
for(i in 1:30){
  death_rate[i] = exp(death)
  death = death-0.2
}
return(death_rate)
}
sim_death("United States")

pop_30 = function(Country){
delta_rate = sim_bir(Country)-sim_death(Country)
pop = data[which(data$X==Country),"Population.2010"]
pop_country =rep(NA,30)
for(i in 1:30){
pop = (pop*1000+pop*delta_rate[i])/1000
pop_country[i] = pop
}
return(pop_country)
}

##########
#Run all the countries
#########
data[which(data$X=="United States"),"pop_30y"] = pop_30("United States")[30]
data[which(data$X=="England"),"pop_30y"] = pop_30("England")[30]
data[which(data$X=="Argentina "),"pop_30y"] = pop_30("Argentina ")[30]
data[which(data$X=="China"),"pop_30y"] = pop_30("China")[30]
data[which(data$X=="India"),"pop_30y"] = pop_30("India")[30]
data[which(data$X=="Lebanon"),"pop_30y"] = pop_30("Lebanon")[30]

#######Simulations#########
par(mfrow=c(3,2))
plot(pop_30("United States"),type="l")
plot(pop_30("England"),type="l")
plot(pop_30("Argentina "),type="l")
plot(pop_30("China"),type="l")
plot(pop_30("India"),type="l")
plot(pop_30("Lebanon"),type="l")


#######disaster#########
#
disaster = function(Country){
  delta_rate = sim_bir(Country)-sim_death(Country)
  pop = data[which(data$X==Country),"Population.2010"]
  pop_country =rep(NA,30)
  for(i in 1:30){
    random = rbinom(n=1, size=1, 0.05)
    if(random==0){
      pop = (pop*1000+pop*delta_rate[i])/1000
    }
    if(random==1){
      pop = (pop*1000*0.95+pop*delta_rate[i])/1000
    }
    pop_country[i] = pop
  }
  return(pop_country)
}

#Plot
par(mfrow=c(3,2))
plot(disaster("United States"),type="l")
plot(disaster("England"),type="l")
plot(disaster("Argentina "),type="l")
plot(disaster("China"),type="l")
plot(disaster("India"),type="l")
plot(disaster("Lebanon"),type="l")

#####################
#Set birth rate == 1/2 of the death rate
####################
###Let's take US as the example
Country ="United States"
delta_rate = 0.5*sim_death(Country)-sim_death(Country)
pop = data[which(data$X==Country),"Population.2010"]
pop_country =rep(NA,30)
for(i in 1:30){
  pop = (pop*1000+pop*delta_rate[i])/1000
  pop_country[i] = pop
}
pop_country
par(mfrow=c(1,1))
plot(pop_country,type="l",ylab="population of U.S",xlab="year")


