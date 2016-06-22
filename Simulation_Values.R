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
#seeds <- round(runif(nsim, 0, 10000), 0)
#write.csv(seeds, file="seeds.csv")

# Create object with all combinations of input parameters
mat <- expand.grid(pts, days, psi, p, phi, g, yrs, nsim)
colnames(mat) <- c("pts", "days", "psi", "p", "phi", "g", "yrs", "nsim")

# Setting up the machine for parallel processing
no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)
#clusterSetRNGStream(cl=cl, iseed=400)

# Run loop to generate simulations for each combination of parameters in object "mat"
result <- list()
#for(m in 1:dim(mat)[1]){
#for(m in 240:dim(mat)[1]){
for(m in 239:239){
    print(m)
    test <- simulatetrend(points=mat[m,1], days=mat[m,2], psi=mat[m,3], p=mat[m,4], phi=mat[m,5], gamma=mat[m,6], years=mat[m,7], nsim=mat[m,8])
    result[[m]] <- test
    names(result)[m] <- paste("input values", paste(mat[m,], collapse=", "))
}

stopCluster(cl)

write.csv(melt.list(result), file="result.csv", row.names=TRUE)

#test <- read.csv(file="result_1to238.csv") 
#test2 <- read.csv(file="result_239.csv") 
#test3 <- read.csv(file="result_240to288.csv") 
#test4 <- rbind(test, test2, test3)
#abcd <- cast(test4, X1 ~ X2 ~ L1)
#test5 <- alply(.data=abcd, .margins=3, .fun="[")

# Run second loop to calculate changes for each set of simulation values

result2 <- list()
z.first <- vector()
y.first <- vector()
phi.all <- data.frame(mat$phi^1, mat$phi^2, mat$phi^3, mat$phi^4, mat$phi^5, mat$phi^6, mat$phi^7, mat$phi^8, mat$phi^9, mat$phi^10, mat$phi^11, mat$phi^12, mat$phi^13, mat$phi^14)

for(i in 1:length(result)){
#for(i in 240:length(result)){
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
    z <- which(round(confint[1,],2) > 0) 
    z.first[i] <- ifelse(length(z)==0,0,min(z))
    #y <- which(round(confint[1,],2) > phi.all[i,]) # y.first[i] <- the first year that confint contains phi, with phi^n
    y <- which(confint[1,] < phi.all[i,] & confint[2,] > phi.all[i,])
    y.first[i] <- ifelse(length(y)==0,0,min(y))
}

#det.year2 <- det.year[det.year$phi==0.]

#for(n in 1:(dim(changes)[2]-1))
# 
#  for(i in 1:length(result)){
#  confint <- apply(changes,2,quantile,c(0.1,0.9))
#  y <- which(round(confint[1,],2) > phi.all)
  
# Three options for problem solving 1) change to function 2) iseed 
  
#create a data frame that has rows for each set of parameters and columns for phi...phi^n  
#then have ifelse statement refer to each column in the dataframe
  
#quantile(changes[,n+1] - changes[,n], c(0.1, 0.9))
#confint[n] <- quantile((changes[,n+1] - changes[,n]), c(0.1, 0.9))

#quantile(changes[,2] - changes[,1], c(0.1, 0.9))

det.year <- data.frame(y.first=y.first, z.first=z.first, mat, input=names(result))
write.csv(det.year, file="det.year.csv", row.names=TRUE)
write.csv(melt.list(result2), file="result2.csv", row.names=TRUE)


#[1] 93
#Error in { : task 96 failed - "non-finite finite-difference value [4]"
#Called from: e$fun(obj, substitute(ex), parent.frame(), e$data)