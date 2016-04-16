# DynOccFunctions
library(unmarked)
library(ggplot2)
library(parallel)
library(foreach)
library(doParallel)

gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}


data.generator<-function(points,days,psi,p,phi,gamma,years) {
        require(unmarked)
        #first year of data
        y1<-matrix(NA,nr=points,nc=days)
        #generate the expected occupancies
        z<-rbinom(points,1,psi)
        #generate the observations
        for(i in 1:points)
                y1[i,]<-rbinom(days,1,z[i]*p)
        #subsequent years
        #three dimensional matrix to store the results
        yk<-array(NA,dim=c(points,days,years))
        yk[,,1]<-y1
        for(k in 2:years){
        #generate the deterministic part of the model
                occ<-apply(yk[,,k-1],1,max,na.rm=T)
                z<-rbinom(points,1,occ*phi+(1-occ)*gamma)
        #generate the observations
        for(i in 1:points)
                yk[i,,k]<-rbinom(days,1,z[i]*p)
     
        }  
        # convert results to a two dimensional matrix for colext
        yk <- matrix(yk, points, days*years)
        #ny <- matrix(as.character(1:years),nrow(yk), years, byrow=TRUE)
        # store data in an unmarkedMultFrame object so it is ready for analysis
        yUMF <- unmarkedMultFrame(y = yk, numPrimary = years)
        yUMF
} 



simulatetrend <- function(points, days, psi, p, phi, gamma, years, nsim) {
        #store results
        #gm_lambda <- numeric()
        time <- 1:years
        results <- foreach(i = 1:nsim, .combine = rbind, .packages = "unmarked", .export = c("data.generator","gm_mean")) %dopar% {
                #generate data and store projected results
                data <- data.generator(points,days,psi,p,phi,gamma,years)
                model <- colext(~1, ~1, ~1, ~1, data = data, method = "BFGS",se = FALSE)
                timeseries <- as.numeric(smoothed(model)[2,])
                
                #calculate difference between year 1 and year n
                diff1_n <- timeseries[1] - timeseries[-1]
                
                #Calculate geometric mean of lambda
                lambda <- timeseries[2:years]/timeseries[1:(years-1)]
                lambda <- -(gm_mean(lambda) - 1)
                c(timeseries,diff1_n,lambda)
        }
        results
        #list(gmlambda = gm_lambda, results = results)
}


# NOTES

# If we are running the same version of R, set.seed should produce similar output, although if R installation differed, they could be different
# see https://stat.ethz.ch/pipermail/r-help/2005-September/079391.html

# If parallelizing, we should NOT use set.seed, but should instead use the function clusterSetRNGStreatm() from the parallel package
# see http://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/

# It doesn't matter what number we use for the seed
# see http://www.r-bloggers.com/what-are-the-most-common-rng-seeds-used-in-r-scripts-on-github/
# see also http://www.r-bloggers.com/a-look-at-random-seeds-in-r-or-85-why-cant-you-be-more-like-548/

