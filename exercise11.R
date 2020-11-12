# N(t+1)=N(t)+r*N(t)*(1-(N+M)/t)
# M(t+1)=M(t)+r*M(t)*(1-(N+M)/t)

### exercise 11
### use population model to investigate evolution of drug resistance in tumors

#normal and mutant cell populations
#simulate population growth before and after drug administration
#parameters before and after
#before (rN=rM=0.1; K = 1000000)
#after (rN=-0.1; rM=0.05; k = 1000000)
#use loop for simulation
#initial start at NO=99; MO=1

#define our parameters

K=1000000
times=400

#normal tumor cell initial values
rN=0.1
NO=99

#mutant tumor cell initial values
rM=0.1
MO=1

#create vector to store N values and set initial N
Ns=numeric(length=times)
Ns[1]=NO

#do the same with M values
Ms=numeric(length=times)
Ms[1]=MO

#simulate after first mutuation until equilibrium is reached
#equilibrium reached at t=150
for(t in 1:(times-1)) {
  if(t<150) {
  Ns[t + 1] <- Ns[t] + (rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
  Ms[t + 1] <- Ms[t] + (rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
  }
#after 150 timesteps, cells are treated with drug
 else {
   rN=-0.1
   rM=0.05
   Ns[t + 1] <- Ns[t] + (rN*Ns[t]*(1-((Ns[t]+Ms[t])/K)))
   Ms[t + 1] <- Ms[t] + (rM*Ms[t]*(1-((Ns[t]+Ms[t])/K)))
 }
}

#plot simulation
library(ggplot2)
sim <- data.frame(time=1:length(Ns),N=Ns,M=Ms)
ggplot(data=sim) +
  geom_line(aes(x=time,y=N,colour="Normal")) +
  geom_line(aes(x=time,y=M,colour="Mutant")) +
  theme_classic() + theme(legend.position="top") +
  scale_colour_manual(values=c("Normal"="blue", "Mutant"="green")) + 
  xlab("Time") + ylab("Number of Cells")