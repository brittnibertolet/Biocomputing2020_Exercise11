#Code that shows effect of r and K on population dynamics
#define initial conditions and parameters
N0=99
M0=1
r = 0.1
K = 1000000
timesteps = 500  

#create output
N=numeric(length=length(1:timesteps))
M=numeric(length=length(1:timesteps))

#fill N0 
N[1]=N0

#fill M0
M[1]=M0

for(i in 2:timesteps){
  if(i>=100){
    # 50 additional new individuals due to immigration
    M[i] = M[i-1]+0.5*r*M[i-1]*(1-(N[i-1]+M[i-1])/K)
    N[i] = N[i-1]+(-1)*r*N[i-1]*(1-(N[i-1]+M[i-1])/K)
  }else{
    M[i] = M[i-1]+r*M[i-1]*(1-(N[i-1]+M[i-1])/K)
    N[i] = N[i-1]+r*N[i-1]*(1-(N[i-1]+M[i-1])/K)
  }
}

#create dataframe
normal=data.frame(time=1:timesteps, celltype="normal", countnumberofcells=N)
mutant=data.frame(time=1:timesteps, celltype="mutant", countnumberofcells=M)

library(ggplot2)
allcells=rbind(normal, mutant)
ggplot(data=allcells, aes(x=time, y=countnumberofcells)) +
  geom_line(aes(color=celltype))
