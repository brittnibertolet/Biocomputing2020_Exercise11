#Challenge: Generate a script that stimulates growth of the two 
#cell types in the tumor to equilibrium folloing drug treatment 

# set initial values and parameters for both non-mutant and mutant cells 
#Initial mutation occurred when there were 100 cells total (99 nonmutant cells and 1 mutant cell)
N0= 99
M0=1

r=0.1 #growth rate for both cell types before drug treatment
rN = -0.1 #growth rate in non mutant cells after drug treatment
rM = 0.05 #growth rate in mutant cells after drug treatment

K= 1000000 #carrying capacity for both cell types 
timesteps= 300

# create 2 vectors to store the two state variables (nonmutant and mutant)
Ns=data.frame(time=1:timesteps,nonmutant=rep(0,timesteps),mutant=rep(0,timesteps))
Ns$nonmutant[1]=N0
Ns$mutant[1]=N0

#simulate
for(i in 2:timesteps){
  if(i<100){ #before drug treatment the two cell types grow at the same rate
    Ns$nonmutant[i] <- Ns$nonmutant[i-1]+r*Ns$nonmutant[i-1]*(1-Ns$nonmutant[i-1]/K)
    Ns$mutant[i] <- Ns$mutant[i-1]+r*Ns$mutant[i-1]*(1-Ns$mutant[i-1]/K) 
  }else{ #after drug treatment nonmutant cells decline rapidly and mutant cells grows at 50% of original growth rate
    Ns$nonmutant[i] <- Ns$nonmutant[i-1]+rN*Ns$nonmutant[i-1]*(1-Ns$nonmutant[i-1]/K)
    Ns$mutant[i] <- Ns$mutant[i-1]+rM*Ns$mutant[i-1]*(1-Ns$mutant[i-1]/K) 
  }
}

#convert wide data to long form 
Ns2 <- data.frame(time=c(Ns$time,Ns$time),N=c(Ns$nonmutant,Ns$mutant),cell=rep(c("nonmutant","mutant"),each=timesteps))

# plot simulation as a variale to export
library(ggplot2)
fig1<- ggplot(data=Ns2,aes(x=time,y=N,color=cell)) + 
  geom_line() +
  theme_classic()

#export graph
ggsave(filename = "fig1.pdf",
       plot = fig1,
       width = 8,
       height = 5,
       dpi = 300)





