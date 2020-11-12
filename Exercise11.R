# Exercise 11

#Set up initial parameters
#When drug is absent:
rN<- 0.1
rM<- 0.1
#When drug is present
rN2<- -0.1
rM2<- 0.05
#Carrying Capacity
K<- 1000000

#Set timesteps
timesteps<-1:500

#Set an output to population
N<-numeric(length = length(1:timesteps))
M<-numeric(length = length(1:timesteps))

#Set N0 and M0
N[1]<-N0
M[1]<-M0

#Use a for loop to simulate in time
for(i in 2:timesteps){
  if(i<100){
    M[i]<-M[i-1]+rM2[i-1]*(1-(N[i-1]+M[i-1])/K)
    N[i]<-N[i-1]+rN2[i-1]*(1-(N[i-1]+M[i-1])/K)
  }else if(i>=100){
    M[i]<-M[i-1]+rM[i-1]*(1-(N[i-1]+M[i-1])/K)
    N[i]<-N[i-1]+rN[i-1]*(1-(N[i-1]+M[i-1])/K)
  }
}

#Set up a drug treatment event to occur sometime at t around 150
























