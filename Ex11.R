# Exercise 11

# 1. cancer cell model
#Nt+1=Nt+rN*Nt*(1-(Nt+Mt)/K)
#Mt+1=Mt+rM*Mt*(1-(Nt+Mt)/K)

# Set initial conditions
r=0.1 #normal growth rate of cells both rM and rN
K=1000000 #carrying capacity
N0=2 #original population number
time=600 

#output for loop
N=numeric(length=length(1:time))
M=numeric(length=length(1:time))
N[1]=N0 #set initial population of non-mutants

#find at what time there are 100 cells
#for loop with just non-mutant population
for (i in 2:length(1:time)){
  N[i]=N[i-1]+r*N[i-1]*(1-(N[i-1]+M[i-1])/K)
}

#make data frame to find time
find100=data.frame(time=1:time,N=N,Cells="Non-Mutant")

#find time where first reaches 100
head(find100[find100$N>99,])

#time=42 when reaches 100 and mutation occurs
M[42]=1

#for loop to make values for graph
for (i in 2:length(1:time)){
  # at N=100, mutant cells start to grow
  if (i>42 & i<250){
    M[i]=M[i-1]+r*M[i-1]*(1-(N[i-1]+M[i-1])/K) 
    N[i]=N[i-1]+r*N[i-1]*(1-(N[i-1]+M[i-1])/K)
    # after reach equilibrium, introduce drug
  }else if (i>250){
    M[250]=9944.072
    # growth rate of mutants is half
    M[i]=(M[i-1]+0.5*r*M[i-1]*(1-(N[i-1]+M[i-1])/K)) 
    # growth rate of non-mutants is -r
    N[i]=N[i-1]+(-r)*N[i-1]*(1-(N[i-1]+M[i-1])/K) 
  # in the beginning, only non-mutant cells grow 
  }else{
    N[i]=N[i-1]+r*N[i-1]*(1-(N[i-1]+M[i-1])/K)
  }
}

#read in ggplot
library(ggplot2)

#make data frame for graph
simN=data.frame(time=1:time,N=N,Cells="Non-Mutant")
simM=data.frame(time=1:time,N=M,Cells="Mutant")

#combine to one data frame
dat=rbind(simN,simM)

#make line graph
ggplot(data=dat,aes(x=time,y=N))+
  geom_line(aes(color=Cells))+
  geom_vline(xintercept=250,linetype="dotted")+
  annotate("text",x=400,y=1000000,label="Drug Added",size=3)+
  theme_classic()

