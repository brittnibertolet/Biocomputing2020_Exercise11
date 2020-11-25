#Exercise 11 - modeling growth of populations

#Parameters
K <- 1000000 #carrying capacity
rN<- 0.1 #normal growth for normal and mutant cells w no drug
rNnd<- -0.1 #normal cells decline 
rMd<- 0.05 #mutant growth with drug

time <- 1:1000 #time values for graphing

#Initialize sites for outputs

resevoirN <- numeric(length=length(time))
resevoirN[1]<- 99 #initial number of normal cells after mutation occurs
resevoirM <- numeric(length = length(time))
resevoirM[1] <- 1 #first mutated cell

#For loop to calculate graph values with equations
#Equations to use:
                  # Nt+1 =Nt+rNNt(1−(Nt+Mt)/K)
                  # Mt+1 =Mt+rMMt(1−(Nt+Mt)/K)

for(i in 2:length(time)){
  #start with normal growth (300 is around equilibrium)
  if(i<=300){ 
  resevoirM[i] <- resevoirM[i-1] + rN*resevoirM[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  resevoirN[i] <- resevoirN[i-1] + rN*resevoirN[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  }else if(i>300){
    resevoirM[100]<-5.755493e+03

    resevoirM[i]<- resevoirM[i-1] + rMd*resevoirM[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
    resevoirN[i]<- resevoirN[i-1] + rNnd*resevoirN[i-1]*(1-(resevoirM[i-1]+resevoirN[i-1])/K)
  }
}
  
#make data readable by ggplot in a dataframe
frame <- data.frame(Time = time, N=resevoirN,M=resevoirM)
library(ggplot2)

ggplot(data=frame, aes(x=time))+
  geom_line(aes(y=M, color = "Cancer Cells"))+
  geom_line(aes(y=N, color = "Normal Cells"))+
  geom_vline(xintercept=300,linetype="dotted")+
  annotate("text", x=300, y=500000, label="Drug Introduced", size = 2)+
  ylab("Number of Cells")+
  xlab("Time")
  theme_classic()



  