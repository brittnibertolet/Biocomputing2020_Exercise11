##Homework 11
##Use two equations to create a dynamic model on population growth
##Equations being used:
#   - N[t+1] = N[t] + rN*N[t]*(1-(N[t]+M[t])/K)
#   - M[t+1] = M[t] + rM*M[t]*(1-(N[t]+M[t])/K)
# N is the non-mutant cell population while M is the mutant cell population
#Make this a two column matrix instead of just a vector so that we can store this

##Step 1: call in ggplot2
library(ggplot2)

##Step 2: Set up the constant variables
timepts = 1000 #in days
#Normal rates of growth
rN = 0.1
rM = 0.1
#Rate of growth after medicine has been introduced
rNm = -0.1
rMm = rM/0.5
#Carrying capacity
K = 1000000
  
##Step 3: Set up empty vectors to contain the changing cell concentrations
N <- numeric(length=length(1:timepts))
M <- numeric(length=length(1:timepts))

##Step 4: Set the initial values of non-mutant (N) and mutant (M) cells
N[1] <- 100
M[1] <- 1

##Step 5: Set up the for loop to fill in the cancerCells matrix
for (i in 2:length(1:timepts)){
    N[i] = N[i-1] + rN*N[i-1]*(1-(N[i-1]+M[i-1])/K)
    M[i] = M[i-1] + rM*M[i-1]*(1-(M[i-1]+N[i-1])/K)
}

##Step 6: Create a data frame with this info so that it may be plotted
cancerCells <- data.frame(timesteps=1:timepts, Nval=N, Mval=M)

##Step 7: Plot the data in a line graph
ggplot(data=cancerCells, aes(x=timesteps))+
  geom_line(aes(y=Nval, color = "Non-Mutant Cells"))+
  geom_line(aes(y=Mval, color = "Mutant Cells"))+
  labs(color="Cell Type")+
  theme_classic()