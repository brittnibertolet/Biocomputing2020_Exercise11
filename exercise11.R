#model for growth: N(t+1)= N(t)+r(N)*N(t)*(1-(N(t)+M(t))/K)
#second model:M(t+1)= M(t)+r(M)*M(t)*(1-(N(t)+M(t))/K)
#when cancer drug is not present: r(N)=r(M)=0.1
#when drug is present, non-mutant cells: r(N)= -0.1
#when drug is present, mutant cells: r(M)=0.05
#assume K=1,000,000
#mutation occurred when there were 100 cells
#generate script that stimulates growth of two subpopulations in the tumor to equilibrium followed
#   drug treatment. Plot results with line graph

#set inital values
K=1000000
r0=0.1
rN= -0.1
rM= 0.05
N0=99
M0=1
timesteps=600
