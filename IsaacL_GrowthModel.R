#Exercise 11 - modeling growth of populations

#Parameters
K <- 1000000 #carrying capacity
rN<- 0.1 #normal growth for normal and mutant cells w no drug
rNnd<- -0.1 #normal cells decline no drug
rMd<- 0.05 #mutant growth with drug

time <- 1:1000 #time values for graphing

#Initialize sites for outputs

resevoirN <- numeric(length=length(1:time))
resevoirN[1]<- 99 #initial number of normal cells after mutation occurs
resevoirM <- numeric(length = length(1:time))
resevoirM[1] <- 1 #first mutated cell

#For loop to calculate graph values with equations
#Equations to use:
                  # Nt+1 =Nt+rNNt(1−(Nt+Mt)/K)
                  # Mt+1 =Mt+rMMt(1−(Nt+Mt)/K)

for(i in 2:length(time)){
  #start with normal growth
  resevoirM[i] <- resevoirM[i-1] + rN*resevoirM[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  resevoirN[i] <- resevoirN[i-1] + rN*resevoirN[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
}
  







#drug growth
  
  resevoirM[i]<- resevoirM[i-1] + rNnd*resevoirM[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  resevoirN[i]<- resevoirN[i-1] + rNnd*resevoirN[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  
  
  
  
  
  
  
  
  
  
  
  
  
}
  