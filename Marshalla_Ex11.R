### Biocomputing Exercise 11 - Dan Marshalla
## Create model of tumor subpopulations with drug resistance and drug treatment
# rate of increase prior to drug treatment
rm1 = 0.1
rn1 = 0.1
# rates of increase after drug treatment
rm2 = 0.05
rn2 = -0.1
# carrying capacity
k = 1000000
# amount of time steps
t = 1:600

# Set up a matrix to store values of model for each time step
# column 1 = t, col 2 = M (mutant) cell abundance, col 3 = N (non-mutant) cell abundance
matrix <-matrix(nrow=600,ncol=3)
matrix[,1]=1:600
# initial M
matrix[1,2]=1
# initial N
matrix[1,3]=99

# for loop to input growth rate of subpopulations prior to drugs into matrix
for (i in 2:200){
  matrix[i,2]=matrix[i-1,2]+rm1*matrix[i-1,2]*(1-(matrix[i-1,3]+matrix[i-1,2])/k)
  matrix[i,3]=matrix[i-1,3]+rn1*matrix[i-1,3]*(1-(matrix[i-1,3]+matrix[i-1,2])/k)
}
# for loop to input growth rate of subpopulations after drugs into matrix
for (i in 201:600){
  matrix[i,2]=matrix[i-1,2]+rm2*matrix[i-1,2]*(1-(matrix[i-1,3]+matrix[i-1,2])/k)
  matrix[i,3]=matrix[i-1,3]+rn2*matrix[i-1,3]*(1-(matrix[i-1,3]+matrix[i-1,2])/k)
}

# Plot the subpopulation abundances
# vertical line @ t=200 where drug treatment starts
par(mfrow=c(1,2))
plot(t,matrix[,2], type = 'l', lwd=2, xlab="t (days)", ylab = "M (mutant cells)",col="red")
lines(x=c(200,200),y=(c(0,1000000)))
plot(t,matrix[,3], type = 'l', lwd=2, xlab="t (days)", ylab = "N (non-mutant cells)", col="blue")
lines(x=c(200,200),y=(c(0,1000000)))