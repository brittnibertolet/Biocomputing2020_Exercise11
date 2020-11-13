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
timesteps<-500

#Set an output to population
N<-numeric(length = length(timesteps))
M<-numeric(length = length(timesteps))

#Set N0 and M0
N0<-99
M0<-1
N[1]<-N0
M[1]<-M0

#Use a for loop to simulate in time
for(i in 1:timesteps){
  if(i<100){
    M[i+1] = M[i]+rM2*(1-(N[i]+M[i])/K)
    N[i+1] = N[i]+rN2*(1-(N[i]+M[i])/K)
  }else if(i>=100){
    M[i+1] = M[i]+rM*(1-(N[i]+M[i])/K)
    N[i+1] = N[i]+rN*(1-(N[i]+M[i])/K)
  }
}

#Set up a drug treatment event to occur sometime at t around 150
NormalStrain<-data.frame(time=1:timesteps, celltype="normal", cellcount=N)
MutantStrain<-data.frame(time=1:timesteps, celltype="mutant", cellcount=M)
TotalCells<-rbind(NormalStrain, MutantStrain)


library(ggplot2)
ggplot(data = TotalCells, aes(x=time, y=cellcount, col=celltype)) +
  geom_line()
























