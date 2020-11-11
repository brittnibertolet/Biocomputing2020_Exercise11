# Exercise 11
# Chelsea Southworth

# load ggplot for later figures
library(ggplot2)

### we started with this basic model
# time points to simulate over 
# a place to store population sizes (value of state variable through time)
# values for our parameters 
# a loop for simulating
# initial starting place


# Part 1: Simulate to equilibrium
# Then do Part 2: drug treatment, which changes rn to -.1 instead of .1

# set parameter and state variable values
rn = .1
rm = .1
N0 = 99
M0 = 1
K = 1000000
timesteps = 1000 # <- this isn't supposed to be one, change it
# not sure how many timesteps we need to run the model to equilibrium?
# at 10000 it's definitely at equilibrium; looks like it gets there a LOT earlier,
  # looks like it's definitely at equilibrium by 200

# create vector to store N's and set initial N
N = numeric(length=timesteps)
N[1] = N0

# create vector to store M's and set initial M
M = numeric(length=timesteps)
M[1] = M0


# simulate N equation
for(i in 2:timesteps){
  N[i] <- N[i-1]+rn*N[i-1]*(1-(N[i-1]+M[i-1])/K)  
}

# simulate M equation
for(i in 2:timesteps){
  M[i] <- M[i-1]+rm*M[i-1]*(1-(N[i-1]+M[i-1])/K)  
}


simN <- data.frame(time=1:timesteps,N=N)
simM <- data.frame(time=1:timesteps, M=M)

ggplot(data=simM,aes(x=time,y=M))+geom_line()+theme_classic() +
  geom_line(data = simN, aes(x=time,y=N), color = "blue")

# Then do like the tutorial example


# at timestep 200, when it's all definitely at equilibrium, the rn becomes -.1 instead of .1
  # make rn negative
# use the if/else format
# simulate
for(i in 2:timesteps){
 if(i>=200){
    # N becomes -.1
   N[i] <- N[i-1]-rn*N[i-1]*(1-(N[i-1]+M[i-1])/K)
  }else{
    N[i] <- N[i-1]+rn*N[i-1]*(1-(N[i-1]+M[i-1])/K)
  }
}

simN <- data.frame(time=1:timesteps,N=N)

# simulate M equation
for(i in 2:timesteps){
  M[i] <- M[i-1]+rm*M[i-1]*(1-(N[i-1]+M[i-1])/K)  
}
simM <- data.frame(time=1:timesteps, M=M)

ggplot(data=simM,aes(x=time,y=M))+geom_line()+theme_classic() +
  geom_line(data = simN, aes(x=time,y=N), color = "blue")
