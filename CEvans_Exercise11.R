# Exercise 11 - Dynamic Modeling of drug resistance in tumors
# Connor Evans 

library(ggplot2)

# Absent drug treatment:
# rN = 0.1
# rM = 0.1

# Once drug treatment starts:
# rN = -0.1
# rM = 0.05

N0 <- 99 #the mutation occurred when there were 100 total cells in the tumor
M0 <-  1
rN <- 0.1  #growth rate of population N
rM <-  0.1 #growth rate of population M
K <- 1000000 #carrying capacity - 1 million cells
timesteps <- 1000
DrugTreatment = F #starting off growth with no drug treatment

# create vector to store population values and set initial population values
popMatrix = matrix(data = NA, nrow = timesteps, ncol = 2)
popMatrix[1,1] = N0
popMatrix[1,2] = M0

# simulate
for(t in 1:(timesteps-1)){   #for the amount of timesteps...
  if(t >= 160){ 
    # Drug treatment begins at equilibrium (total population reaches carrying capacity, which happens at ~ t = 160)
    rN <- -0.1
    rM <- 0.05 
    DrugTreatment = T
    popMatrix[t+1,1] = popMatrix[t,1] + rN*(popMatrix[t,1])*(1-((popMatrix[t,1] + popMatrix[t,2])/K)) # Update N population (non-mutated)
    popMatrix[t+1,2] = popMatrix[t,2] + rM*(popMatrix[t,2])*(1-((popMatrix[t,1] + popMatrix[t,2])/K)) # Update M population (mutated)
  }
  else {
    popMatrix[t+1,1] = popMatrix[t,1] + rN*(popMatrix[t,1])*(1-((popMatrix[t,1] + popMatrix[t,2])/K)) # Update N population (non-mutated)
    popMatrix[t+1,2] = popMatrix[t,2] + rM*(popMatrix[t,2])*(1-((popMatrix[t,1] + popMatrix[t,2])/K)) # Update M population (mutated)
      
  }
}

# plot simulation
simEvents<-data.frame(time=1:length(popMatrix[,1]),N = popMatrix[,1], M = popMatrix[,2])
ggplot(data=simEvents)+
  geom_line(aes(x=time,y=N,col='Non-mutated'))+
  geom_line(aes(x=time,y=M,col='Mutated'))+
  xlab('Time step')+
  ylab('Poplulation size')+
  theme_classic()+
  theme(legend.title = element_blank())
