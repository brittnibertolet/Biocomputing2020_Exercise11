#Exercise 11
#goal is to model mutant v non-mutant cancer cells treated with drugs
#setting time points to simulate over - 1000 points 
#Also need a place to store population sizes
#Values for parameters:
#Pre-treatment: (rN = rM =.1; K=1000000)
#Post-treatment(rN=-0.1, rM=0.05; K=1000000)
#Start by setting initial values
N0 = 99
rN = 0.1

#Mutant values 
M0 = 1
rM = 0.1

K=1000000
timesteps=1000 #timesteps

#vector to store all N values
Ns = as.numeric(length=timesteps)
Ns[1]= 99 

#same for M 
Ms = as.numeric(length=timesteps)
Ms[1] = 1

#Now simulate to Eq, at t=150
#after 150 cells treated with drug
for(t in 1:(timesteps-1)){
  if(t<300){
    Ns[t+1] <- Ns[t] + (rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
    Ms[t+1] <- Ms[t] + (rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
  }
  else{
    rN= -0.1
    rM= 0.05
    Ns[t+1] <- Ns[t] + (rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
    Ms[t+1] <- Ms[t] + (rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
  }
}


#can plot now 
drugsimulation <- data.frame(time=1:length(Ns), N=Ns, M=Ms)
ggplot(data=drugsimulation) +
  geom_line(aes(x=time, y=N, colour="Non-Mutant")) +
  geom_line(aes(x=time, y=M, colour="Mutant")) +
  theme_bw() +
  xlab("Time") + 
  ylab("Number of Cells")
  
  


