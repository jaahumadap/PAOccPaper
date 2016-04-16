############ TEAM CT DYNAMIC OCCUPANCY SIMULATION STUDY ##############

# Load functions for simulating data
source(file="dynoFunctions.R")

# Generate list of input values for simulation study where each item in the list has a vector of input values for
# function(points, days, psi,p ,phi ,gamma, years, nsim)
pts <- c(10, 20, 30, 60, 90, 120)
days <- c(15, 30, 45, 60)
psi <- c(0.5)
p <- c(0.08, 0.2, 0.5)
phi <- c(0.99, 0.95, 0.9, 0.85)
g <- 0
yrs <- c(15)
nsim <- c(500)
# Create object with all combinations of input parameters
mat <- expand.grid(pts, days, psi, p, phi, g, yrs, nsim)
colnames(mat) <- c("pts", "days", "psi", "p", "phi", "g", "yrs", "nsim")

# Setting up the machine for parallel processing
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)
clusterSetRNGStream(cl=cl, iseed=400)

# Run loop to generate simulations for each combination of parameters in object "mat"
result <- list()
for(i in 1:dim(mat)[1]){
    print(i)
    test <- simulatetrend(points=mat[i,1], days=mat[i,2], psi=mat[i,3], p=mat[i,4], phi=mat[i,5], gamma=mat[i,6], years=mat[i,7], nsim=mat[i,8])
    result[[i]] <- test
    names(result)[i] <- paste("input values", paste(mat[i,], collapse=", "))
}

stopCluster(cl)

write.csv(melt.list(result), file="result.csv", row.names=TRUE)

# Run second loop to calculate changes for each set of simulation values

result2 <- list()
y.first <- vector()

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
    names(result2)[i] <- names(result)[i]
    names(changes) <- c(1:ncol(changes))
    confint <- apply(changes,2,quantile,c(0.1,0.9))
    y <- which(round(confint[1,],2) > 0)
    y.first[i] <- ifelse(length(y)==0,0,min(y))
}

det.year <- data.frame(y.first=y.first, mat, input=names(result))
write.csv(melt.list(result2), file="result2.csv", row.names=TRUE)
write.csv(det.year, file="det.year.csv", row.names=TRUE)


