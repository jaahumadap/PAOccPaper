# DynOccFunctions for power Analysis paper
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


data.generator<-function(points,days,psi1,p,phi,gamma,years) {
        require(unmarked)
        psi <- rep(NA, years)                       # Occupancy probability
        muZ <- z <- array(dim = c(points, years))        # Expected and realized occurrence
        y <- array(NA, dim = c(points, days, years))        # Detection histories
        
        psi[1] <- psi1
        
        # Generate latent states of occurrence
        # First year
        z[,1] <- rbinom(points, 1, psi[1])           # Initial occupancy state
        # Later years
        for(i in 1:points){                          # Loop over sites
                for(k in 2:years){                        # Loop over years
                        muZ[k] <- z[i, k-1]*phi + (1-z[i, k-1])*gamma
                        z[i,k] <- rbinom(1, 1, muZ[k])
                }
        }
        
        # Generate detection/non-detection data
        for(i in 1:points){
                for(k in 1:years){
                        prob <- z[i,k] * p
                        for(j in 1:days){
                                y[i,j,k] <- rbinom(1, 1, prob)
                        }
                }
        }
        # Compute annual population occupancy
        for (k in 2:years){
                psi[k] <- psi[k-1]*phi + (1-psi[k-1])*gamma
        }
        
        yk <- matrix(y, points, days*years)
        
        # store data in an unmarkedMultFrame object so it is ready for analysis
        yUMF <- unmarkedMultFrame(y = yk, numPrimary = years)
        list(yUMF = yUMF,psi = psi)
} 
 

simulatetrend <- function(points, days, psi1, p, phi, gamma, years, nsim) {
        #store results
        #gm_lambda <- numeric()
        time <- 1:years
        results <- foreach(i = 1:nsim, .combine = rbind, .packages = "unmarked", .export = c("data.generator","gm_mean")) %dopar% {
                #generate data and store projected results
                data <- data.generator(points,days,psi1,p,phi,gamma,years)
                model <- colext(~1, ~1, ~1, ~1, data = data$yUMF, method = "Nelder-Mead",se = FALSE)
                timeseries <- as.numeric(smoothed(model)[2,])
                
                #calculate difference between year 1 and year n
                diff1_n <- timeseries[1] - timeseries[-1]
                
                #Calculate geometric mean of lambda
                lambda <- timeseries[2:years]/timeseries[1:(years-1)]
                lambda <- -(gm_mean(lambda) - 1)
                c(timeseries,diff1_n,lambda)
        }
        results
}


# NOTES

# If we are running the same version of R, set.seed should produce similar output, although if R installation differed, they could be different
# see https://stat.ethz.ch/pipermail/r-help/2005-September/079391.html

# If parallelizing, we should NOT use set.seed, but should instead use the function clusterSetRNGStreatm() from the parallel package
# see http://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/

# It doesn't matter what number we use for the seed
# see http://www.r-bloggers.com/what-are-the-most-common-rng-seeds-used-in-r-scripts-on-github/
# see also http://www.r-bloggers.com/a-look-at-random-seeds-in-r-or-85-why-cant-you-be-more-like-548/

