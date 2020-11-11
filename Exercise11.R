
N0=99 #number of normal cancer cells when mutation happen
M0=1 #first mutated cell 
r=0.1 #cells grow at 0.1 per day
K=1000000 #carrying capacity is 1M cells
timesteps=500 #don't know how long it should be

# create vector to store N's and set initial N
NormCells=numeric(length=timesteps)
NormCells[1]=N0

MutCells=numeric(length=timesteps)
MutCells[1]=M0

#Use for loop to grow sub-populations over time
#Use if/else statement to determine growth rates (if drug or no drug)
for(t in 1:(timesteps-1)){
  if(t<175){ #Drug would be administered at timestep 175
    NormCells[t+1] <- NormCells[t]+0.1*NormCells[t]*(1-(NormCells[t]+MutCells[t])/K)
    MutCells[t+1] <- MutCells[t]+0.1*MutCells[t]*(1-(NormCells[t]+MutCells[t])/K)
  }else{
    NormCells[t+1] <- NormCells[t]+-0.1*NormCells[t]*(1-(NormCells[t]+MutCells[t])/K)
    MutCells[t+1] <- MutCells[t]+0.05*MutCells[t]*(1-(NormCells[t]+MutCells[t])/K)
  } 
}
# Rates: Mutant w/ drug, Mutant w/o drug, 
#rN_nodrug <- 0.1
#rN_drug <- -0.1
#rM_nodrug <- 0.1
#rM_drug <- 0.05


#Need to turn data into a DataFrame before plotting in ggplot
NormPop=data.frame(time=1:timesteps, CellType="Normal", CellCount=NormCells)
MutPop=data.frame(time=1:timesteps, CellType="Mutant", CellCount=MutCells)

TotalPop=rbind(NormPop, MutPop)

library(ggplot2)
ggplot(data = TotalPop, aes(x=time, y=CellCount, col=CellType))+
  geom_line()