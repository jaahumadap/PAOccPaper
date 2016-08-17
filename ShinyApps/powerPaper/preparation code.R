# Test code for R shiny app for power paper

#load in data
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(png)
library(grid)
powerData <- read.csv("powerPaper/det.year_2016-08-06_FINAL.csv", h=T)

# This app will let you you choose a combo a parameters and give you
# the number of years it will take to detect this change
# The parameters are:
#       Number of camera trap points
#       Number of days sampling within a year
#       The occupancy of the species in year 1
#       The detection probability of the species
#       How much change you want to detect
#
# After selecting these the user will get the number of years required to detect
# that amount of change
# 
# The app will also deliver some graphs so the user can explore how each parameter
# affects the number of years required to detect the change. A vertical line or a point in each graph
# will show the parameter values chosen by the user. The graphs will show a fitted model
#
# Graphs
#       Graph 1: x= time; y = occupancy - average of several simulations. Vertical line - number of years where change is detected with respect to baseline.

# Calculate the number of years



row <- filter(powerData, pts == 10, days == 30, psi1 == 0.5, p == 0.5, decline == 0.05)
is.na(row$year1st)
        

#data generator function
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
#Simulate trend function
simulatetrend <- function(points, days, psi1, p, phi, gamma, years, nsim) {
        #store results
        #gm_lambda <- numeric()
        time <- 1:years
        results <- foreach(i = 1:nsim, .combine = rbind, .packages = "unmarked", .export = c("data.generator")) %dopar% {
                #generate data and store projected results
                data <- data.generator(points,days,psi1,p,phi,gamma,years)
                model <- colext(~1, ~1, ~1, ~1, data = data$yUMF, method = "BFGS",se = FALSE)
                timeseries <- as.numeric(smoothed(model)[2,])
                
                #calculate difference between year 1 and year n
                #diff1_n <- timeseries[1] - timeseries[-1]
                
                #Calculate geometric mean of lambda
                #lambda <- timeseries[2:years]/timeseries[1:(years-1)]
                #lambda <- -(gm_mean(lambda) - 1)
                timeseries
        }
        results
}


no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

res <- simulatetrend(points = 10, days = 20, psi1 = 0.5, gamma = 0, phi = 0.95, nsim = 5, years = 10, p = 0.5)
stopCluster(cl)

res <- data.frame(res)
#slopes <- data.frame(slopes = test[,ncol(res)])
#changes <- data.frame(test[,(ncol(res)/2+1):(ncol(res)-1)])
#res <- res[,-c((ncol(res)/2+1):(ncol(res)))]
res <- data.frame(res, id = 1:nrow(res))
names(res) <- c(1:(ncol(res)-1), "id")

res <- melt(res,c("id"), variable_name = "year")
#names(res)[2] <- "year"
ggplot(res, aes(x=year, y= value)) + geom_line(aes(group=id),alpha=0.2)

head(powerData)

ggplot(filter(powerData, percent == "5%"), aes(x=pts, y = year1st, color = psi1, shape = as.character(p))) + geom_jitter() + geom_vline(xintercept = 10) + geom_hline(yintercept = 11) + xlab("Number of camera trap points") + ylab("Number of years to detect change")  + scale_shape_discrete(guide = guide_legend(title = "Det. probability")) + scale_color_gradient(guide = guide_legend(title = "Initial\noccupancy"))

names(powerData)
