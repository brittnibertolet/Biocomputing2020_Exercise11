# Models a normal and mutant population of cells in an environment with carrying capacity of 1,000,000
k=1000000
# Total experiment is allowed to last 500 timesteps, with a treatment administered part way 
ts=500
# Sets coefficients to reflect various cell populations pre and post treatment
rN1=0.1
rM1=0.1
rN2=-.1
rM2=0.05

#sets dataframe for each population
Ns=data.frame(time=1:ts,sim1=rep(0,ts))
Ms=data.frame(time=1:ts,sim1=rep(0,ts))

#sets total cells 100, with 99 normal cells and 1 mutated cell initially for this example
Ns[1,2]=99
Ms[1,2]=1

#simulates population growth with introduction of treatment at timestep 150 and adds to respective dataframe
for(t in 2:ts){
  if(t<150){
    Ns$sim1[t] <- Ns$sim1[t-1]+rN1*Ns$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/k))
    Ms$sim1[t] <- Ms$sim1[t-1]+rM1*Ms$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/k))}
  else{
    Ns$sim1[t] <- Ns$sim1[t-1]+rN2*Ns$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/k))
    Ms$sim1[t] <- Ms$sim1[t-1]+rM2*Ms$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/k))}
}

#consolidates data to singlular spread sheet for ease of graphing and data management
library(reshape2)
Ns$sim2=Ms$sim1
Ns=melt(Ns,id.vars="time")

# plots simulation
library(ggplot2)
ggplot(data=Ns,aes(x=time,y=value,color=variable)) + geom_line() + theme_classic() + labs(color="cell type") +scale_color_manual(labels = c("normal", "mutant"), values = c("blue", "red"))

