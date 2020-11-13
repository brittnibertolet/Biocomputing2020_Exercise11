# Use the population growth dynamic modeling to model cancerous cell growth
## in terms of mutating and acquiring drug resistance 

# mutation has no effect on growth rate of the mutated sub-population when 
## drug absent
# when the cancer drug is present the mutant sub-population grows at 50% of its 
## growth rate in the absence of the drug and the non-mutant sub-population 
## declines rapidly

# equations to model these phenomena are as follows
## Nt+1=Nt+rN*Nt(1-(Nt+Mt)/K)
## Mt+1=Mt+rM*Mt(1-(Nt+Mt)/K)

# When drug absent rN=rM=0.1 and K=10^6
# mutation of a single cell occurred early in the tumor growth and when it 
## occurred there were 100 total cells in the tumor. Drug treatment of non-mutant 
## cells results in a negative growth rate of -0.1.

# simulate growth of these 2 sub-populations in tumor
## 1.) equilibrium 2.) followed by drug treatment
# Plot results on line graph (2 lines)


# Step 1: define parameters
rMnodrug=0.1
rNnodrug=0.1
rMdrug=-0.1
K=1000000

# step 2: set time span and increments to simulate
times<-1:1000

# Step 3: place to store output need something as long as times we want to simulate
output<-matrix(data=NA, nrow =length(times), ncol=3)
output[,1]=times

# Step 4: initial population size (where we start)
output[1,2]=2

# K=1000000 so expect it to end up at 1000000 at the end
# put a - in front of index, get rid of that entry

# Step 5: for loop, the dynamic part of the dynamic model
for (i in times[-1]) {
  output[i,2]=output[(i-1),2]+rNnodrug*output[(i-1),2]*(1-output[(i-1),2]/K)
}

for (i in times[-1]) {
  output[i,3]=output[(i-1),3]+r*output[(i-1),3]*(1-output[(i-1),3]/K)
}


#can check this worked by putting head(output) or tail(output) in the console
head(output)
tail(output)
# plot the results
## note that ggplot doesn't like matrices, make it a data frame







