# Exercise 11 - Dynamic Modeling

# set initial values and parameters
r=0.1
K=1000000
# set a time horizon
times<-1:50
# place to store output
output<-matrix(data=NA,nrow=length(times),ncol=3)
output[,1]=times

## initial population size
# for our population of non-resistant tumor cells
output[1,2]=100
# for our population of resistant tumors
output[1,3]=1

# equation w/ for loop
for(i in times[-1]){
  output[i,2]=output[(i-1),2]-r*output[(i-1),2]*(1-(output[(i-1),2]+output[(i-1),3])/K)
  output[i,3]=output[(i-1),3]+r*output[(i-1),3]*(1-(output[(i-1),2]+output[(i-1),3])/K)
}

# plotting results (first convert from matrix to dataframe)
outputnonmutant<-data.frame(time=output[,1],nonmutant=output[,2])
outputmutant<-data.frame(time=output[,1],mutant=output[,3])
# plot the results
## load package
library(ggplot2)
ggplot()+
  geom_line(data = outputnonmutant, aes(x=time, y=nonmutant), color='blue')+
  geom_line(data = outputmutant, aes(x=time, y=mutant), color='red')+
  xlab("Time")+
  ylab("# of cells")+
  theme_classic()


