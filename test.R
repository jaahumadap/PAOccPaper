#Test code

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

set.seed(400)
data <- data.generator(points = 100,days = 30, psi = 0.9, p = 0.2, phi = 0.9, 
                       gamma = 0, years = 5)

#Look at the first year of data
summary(data)
model <- colext(psiformula = ~1,gammaformula = ~1,epsilonformula = ~1, 
                pformula = ~1, data = data, method = "BFGS",se = FALSE)
summary(model)
backTransform(model,type=c("psi"))
backTransform(model,type=c("col"))
backTransform(model,type=c("ext"))
backTransform(model,type=c("det"))

year <- 1:5
y <- round(t(smoothed(model))*30)
y <- y[,c(2,1)]
lrmodel <- glm(y ~ year, family = "binomial")
lrmodel$fitted.values
summary(lrmodel)

# do a linear regression
yl <- smoothed(model)[2,]
lmodel <- lm(yl ~ year)
plot(smoothed(model)[2,])
lines(lrmodel$fitted.values)
lines(lmodel$fitted.values,lty=2)

data3d <- array(data@y,dim = c(30,30,5))
obs <- apply(apply(data3d,c(1,3),max),2,sum)/dim(data3d)[1]
mod <- as.numeric(projected(model)[2,])

plot(1:5, obs, type="p"); lines(1:5, mod) ; lines(lmodel,lty=2)

simulatetrend <- function(points = 10,days = 30, psi = 0.5, p = 0.2, phi = 0.2, gamma = 0, years = 5, nsim = 5) {
        #store results
        results <- matrix(NA, nr = nsim, nc = years)
        logisticRes <- matrix(NA, nr = nsim, nc = 2)
        linearRes <- matrix(NA, nr = nsim, nc = 2)
        gm_lambda <- numeric()
        time <- 1:years
        for (i in 1:nsim) {
                #generate data and store projected results
                data <- data.generator(points,days,psi,p,phi,gamma,years)
                model <- colext(~1, ~1, ~1, ~1, data = data, method = "BFGS",se = FALSE)
                results[i,] <- as.numeric(smoothed(model)[2,])
                
                #Calculate geometric mean of lambda
                lambda <- results[i,2:years]/results[i,1:(years-1)]
                gm_lambda[i] <- -(gm_mean(lambda) - 1)
                # Calculate logistic regression
                y <- round(t(smoothed(model))*points)
                #lrmodel <- glm(y ~ time, family = "binomial")
                #logisticRes[i,] <- lrmodel$coeff
                
                # Calculate a linear regression
                #y <- smoothed(model)[2,]
                #lmodel <- lm (y ~ time)
                #linearRes[i,] <- lmodel$coefficients
        }
        list(gmlambda = gm_lambda,logistic = logisticRes, linear = linearRes,results = results)
}

system.time(test <- simulatetrend(points = 50, psi = 0.9, gamma = 0, phi = 0.8, nsim = 100))
require(ggplot2)
res <- data.frame(test$results, id = 1:100)
names(res) <- c(1:(ncol(res)-1),"id")
res <- melt(res,"id")
names(res)[2] <- "year"
ggplot(res, aes(x=year,y=value, group=id))+geom_line(alpha=0.2)

slopes <- as.data.frame(test$gmlambda)
names(slopes) <- c("slope")#ggplot(slopes,aes(x=exp(X2))) + geom_histogram()
ggplot(slopes,aes(x=slope)) + geom_histogram() + geom_density()

gm_mean = function(x, na.rm=TRUE){
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
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
library(parallel)
library(foreach)

library(doParallel)



simulatetrend <- function(points = 10,days = 30, psi = 0.5, p = 0.2, phi = 0.2, gamma = 0, years = 5, nsim = 5) {
        #store results
        #gm_lambda <- numeric()
        time <- 1:years
        results <- foreach(i = 1:nsim, .combine = rbind, .packages = "unmarked", .export = c("data.generator","gm_mean")) %dopar% {
                #generate data and store projected results
                data <- data.generator(points,days,psi,p,phi,gamma,years)
                model <- colext(~1, ~1, ~1, ~1, data = data, method = "BFGS",se = FALSE)
                timeseries <- as.numeric(smoothed(model)[2,])
                
                #Calculate geometric mean of lambda
                lambda <- timeseries[2:years]/timeseries[1:(years-1)]
                lambda <- -(gm_mean(lambda) - 1)
                c(timeseries,lambda)
        }
        results
        #list(gmlambda = gm_lambda, results = results)
}
# Calculate the number of cores
no_cores <- detectCores() - 1

cl<-makeCluster(no_cores)
registerDoParallel(cl)

test <- simulatetrend(points = 60, psi = 0.9, gamma = 0, phi = 0.8, nsim = 100)

res <- data.frame(test)
slopes <- data.frame(slopes = test[,ncol(res)])
res <- res[,-ncol(res)]
res <- data.frame(res, id = 1:nrow(res))
names(res) <- c(1:(ncol(res)-1), "id")

res <- melt(res,c("id"))
names(res)[2] <- "year"
ggplot(res, aes(x=year,y=value, group=id))+geom_line(alpha=0.2)
ggplot(res, aes(x=year, y= value)) + geom_violin() + geom_line(aes(group=id),alpha=0.2)
ggplot(slopes,aes(x=slopes)) + geom_histogram() + geom_density()

#sims <- read.csv("det.year.csv",h=T)
sims <- read.csv("det.year_2016-06-29.csv",h=T)
#levels(sims$pts) <- c(10,120,20,30,60,90)
#sims$pts <- as.numeric(as.character(sims$pts))
#names(sims)
#qwe<-sims[49:(49+47),]
#head(sims)
library(dplyr)
library(ggplot2)

ggplot(sims, aes(x=pts,y=z.first, color = days)) + geom_point()
plot(sims$y.first)
plot(sims$z.first)
sims$y.first <- ifelse(sims$y.first == 0, 15, sims$y.first)
sims$z.first <- ifelse(sims$z.first == 0, 12, sims$z.first)
plot(sims$y.first)

ggplot(sims, aes(x=pts,y=y.first, color = days)) + geom_point()

ggplot(sims, aes(x=pts,y=z.first, colour = days)) + geom_jitter() + facet_grid(p ~  phi)

simsp2 <- filter(sims, p == 0.2)
ggplot(sims, aes(x=pts, y=z.first, color = p)) + geom_jitter(size=2) + facet_grid(days ~  phi)
ggplot(simsp2, aes(x=pts, y=z.first, color = p)) + geom_jitter(size=2) + facet_grid(days ~  phi)

#Correcting mapping problem with number of days
newdays <- numeric()
indx <- which(sims$days == 60)
newdays[indx] <- 15
indx <- which(sims$days == 45)
newdays[indx] <- 30
indx <- which(sims$days == 30)
newdays[indx] <- 45
indx <- which(sims$days == 15)
newdays[indx] <- 60

sims <- mutate(sims, newdays = newdays)

simsp2 <- filter(sims, days < 60)
ggplot(simsp2, aes(x=pts, y=z.first, color = as.character(p))) + geom_jitter(size=2) + facet_grid(days ~  phi) + geom_smooth(span = 1, se = FALSE) + labs(x = "Number of points in deployment", y="Number of years to detect change") + scale_color_discrete(name="Detection prob") + theme(legend.position = "left") + ylim(0,13) + xlim(0,140)
#expand_limits(y=c(1,13))

sims <- read.csv("det.year_2016-06-29.csv",h=T)
levels(sims$pts) <- c(10,120,20,30,60,90)
sims$pts <- as.numeric(as.character(sims$pts))
names(sims)
#qwe<-sims[49:(49+47),]
#head(sims)
library(dplyr)
library(ggplot2)

test <- read.csv("det.year_2016-07-28.csv",h=T)
test$z.first <- ifelse(test$z.first == 10, NA, test$z.first)

ggplot(test, aes(x=pts, y=z.first, color = as.character(p))) + geom_jitter(size=2) + facet_grid(days ~  phi) + geom_smooth(span = 1, se = FALSE) + labs(x = "Number of points in deployment", y="Number of years to detect change") + scale_color_discrete(name="Detection prob") + theme(legend.position = "left") + ylim(0,13) + xlim(0,140)


# Looking at new files

#newdata <- read.csv("result_2016-10-08.csv",h=T)
#row <- filter(newdata, pts == 1, days == 10, psi1 == 0.1, p == 0.1, phi == 0.95)
#row$mode <- as.numeric(as.character(row$mode))
#row <- row[,c(10:19,22)]
#row <- melt(row,id.vars = "transition")
require(tidyverse)
newdata <- readRDS("result_2016-10-08.rds.download/result_2016-10-08.rds")
headers <- read.csv("list_labels.csv", h = T)
headers <- select(.data = headers, pts, days, psi1, p, phi)

summarize_sims <- function (matrix, alpha = c(0.05,0.95)) {
        
        matrix <- matrix[,1:10]
        cl <- apply(matrix, 2, quantile, alpha)
        mean <- apply(matrix, 2, mean, na.rm =T)
        median <- apply(matrix, 2, median, na.rm = T)
        c(cl[1,], cl[2,], mean, median)
        
}

res80 <- sapply(newdata,summarize_sims,alpha = c(0.1,0.9),simplify = T, USE.NAMES = F)
res80 <- t(res80)
res90 <- sapply(newdata,summarize_sims,alpha = c(0.05,0.95),simplify = T, USE.NAMES = F)
res90 <- t(res90)
res95 <- sapply(newdata,summarize_sims,alpha = c(0.025,0.975),simplify = T, USE.NAMES = F)
res95 <- t(res95)
timeseriesData <- data.frame(headers,res80,res90,res95)
rownames(timeseriesData) <- NULL
write.csv(timeseriesData,file = "timeseriesData.csv", row.names = F)

detYear <- read_csv("~/Analyses/PAOccPaper/det.year_2016-10-08.csv")
timeseriesData <- data.frame(detYear[,2:4],timeseriesData)

data <- readRDS("ShinyApps/powerPaper/data")
row <- filter(data, pts == 60, days == 30, psi1 == 0.5, p == 0.5, phi == 0.9)
summary(data[,1:8])
