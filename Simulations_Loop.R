############ DYNAMIC OCCUPANCY SIMULATION STUDY ##############
########## Ahumada, Beaudrot, O'Brien and Jansen ############# 

# Load functions for simulating data
source(file="dynoFunctions.R")

# Generate list of input values for simulation study where each item in the list has a vector of input values for
# function(points, days, psi,p ,phi ,gamma, years, nsim)
pts <- c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
days <- c(1, 15, 30, 45, 60)
psi1 <- c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0)
p <- c(0.1, 0.2, 0.3, 0.4, 0.5)
phi <- c(0.99, 0.95, 0.9, 0.85)
g <- 0
yrs <- c(10)
nsim <- c(250)

# Create object with all combinations of input parameters
mat <- expand.grid(pts, days, psi1, p, phi, g, yrs, nsim)
colnames(mat) <- c("pts", "days", "psi1", "p", "phi", "g", "yrs", "nsim")

# Create a string of seeds so that exact results can be obtained with parallel processing
# Seed needs to be set for each core to ensure repeatability; write file for records
#seeds <- round(runif(nsim, 0, 10000), 0)
#write.csv(seeds, file="seeds_2016-06-22.csv")

# Setting up the machine for parallel processing
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

# Run loop to generate simulations for each combination of parameters in object "mat"
result <- list()
for(m in 1:dim(mat)[1]){
  print(m)
  test <- simulatetrend(points=mat[m,1], days=mat[m,2], psi1=mat[m,3], p=mat[m,4], phi=mat[m,5], gamma=mat[m,6], years=mat[m,7], nsim=mat[m,8])
  result[[m]] <- test
  names(result)[m] <- paste("input values", paste(mat[m,], collapse=", "))
}

hold <- vector()
for(m in 1:dim(mat)[1]){
  hold[m] <- paste("input values", paste(mat[m,], collapse=", "))
}

stopCluster(cl)
write.csv(melt.list(result), file="result.csv", row.names=TRUE)

# Calculate the first year in which can can be detected (z.first) for 80%, 90% and 95% confidence intervals for each parameter combination
result2 <- list()
z.first80 <- vector()
z.first90 <- vector()
z.first95 <- vector()

for(i in 1:length(result)){
  print(i)
  res <- data.frame(result[[i]])
  slopes <- data.frame(slopes=res[,ncol(res)])
  changes <- data.frame(res[,(ncol(res)/2+1):(ncol(res)-1)])
  res <- res[,-c((ncol(res)/2+1):(ncol(res)))]
  res <- data.frame(res, id = 1:nrow(res))
  names(res) <- c(1:(ncol(res)-1), "id")
  res <- melt(res,c("id"), variable_name = "year")
  result2[[i]] <- res
  names(result2)[i] <- names(result)[i] # only if running loop 2 directly after loop 1, otherwise names order differs
  names(changes) <- c(1:ncol(changes))
  confint80 <- apply(changes,2,quantile,c(0.1,0.9))
  confint90 <- apply(changes,2,quantile,c(0.05,0.95))
  confint95 <- apply(changes,2,quantile,c(0.025,0.975))
  z80 <- which(round(confint80[1,],2) > 0) 
  z90 <- which(round(confint90[1,],2) > 0)
  z95 <- which(round(confint95[1,],2) > 0)
  z.first80[i] <- ifelse(length(z80)==0,0,min(z80))
  z.first90[i] <- ifelse(length(z90)==0,0,min(z90))
  z.first95[i] <- ifelse(length(z95)==0,0,min(z95))
 
}


det.year <- data.frame(z.first80=z.first80, z.first90=z.first90, z.first95=z.first95, mat, input=names(result)) 
write.csv(det.year, file="det.year.csv", row.names=TRUE)
write.csv(melt.list(result2), file="result2.csv", row.names=TRUE)


