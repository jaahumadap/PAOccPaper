# Simulating trends and then WPI

#Simulate a trend for one species
library(parallel)
library(foreach)

library(doParallel)


no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

sp1 <- simulatetrend(points = 60, phi = 0.95, years = 15, nsim = 100)

stopCluster(cl)



simulateCommunity <- function(nspecies, points, years, nsim, phi, gamma,p){
        
        #choose phi at random
        set.seed(234)
        phi2 <- runif(nspecies,phi[1],phi[2])
        gamma2 <- runif(nspecies,gamma[1],gamma[2])
        res <- array(NA,c(nsim,years*2,nspecies))
        
        #set up parallel processing stuff
        no_cores <- detectCores() - 1
        cl<-makeCluster(no_cores)
        registerDoParallel(cl)
        for (i in 1:nspecies) {
                
                
                res[,,i] <- simulatetrend(points = points, phi = phi2[i], gamma = gamma2[i], years = years, nsim = nsim, p = p)
                print(paste("Done with species ",i))
                
        }
        stopCluster(cl)
        res
}

# do it for 16 species declining
# for final analysis
spDec <- simulateCommunity(nspecies = 16,points = 60,years = 16,nsim = 100,phi = c(0.925,0.975),gamma = c(0,0), p = 0.4)
control <- simulateCommunity(nspecies = 16,points = 60,years = 16,nsim = 100,phi = c(0.9, 0.95),gamma = c(0.1, 0.05), p=0.4)
# Simulate half the species stable

#spHalfStable <- simulateCommunity(nspecies = 8,points = 60,years = 16,nsim = 100,phi = 0.9, gamma= 0.05)

require(abind)
#for final analysis
spHalf <- abind(spDec[,,1:8],control[,,1:8], along = 3)
spQtr <- abind(spDec[,,1:4],control[,,1:12],along = 3)
spOne <- abind(spDec[,,1],control[,,1:15], along = 3)


# Final communities are:
# spDec = ALL species declining
# spHalf = HALF species declining, half stable
# spQtr = QUARTER species declining, THREE QUARTERS STABLE

# Calculate the WPI for each

#function to generate the WPI from the output simulations in JAGS
# psi is a three dimensional matrix with the psi of each species in each year
f.WPI <-function(psi){
        nsim <- dim(psi)[1]
        nyears <- dim(psi)[2]
        nsp <- dim(psi)[3]
        rel_psi<-numeric()
        wpi<-matrix(NA,nr=nsim,nc=nyears)
        for(i in 1:nsim){
                for(t in 1:nyears){
                        for(s in 1:nsp){
                                rel_psi[s] <- log(psi[i,t,s]/psi[i,1,s])
                        }
                        wpi[i,t]<-exp(1/nsp*sum(rel_psi))
                }
        }
        wpi
        
}

WPIspDec <- f.WPI(spDec[,1:16,])
WPIspHalf <- f.WPI(spHalf[,1:16,])
WPIspQtr <- f.WPI(spQtr[,1:16,])
WPIcontrol <-f.WPI(control[,1:16,])
WPIspOne <- f.WPI(spOne[,1:16,])

#graph it

#graph the WPI through time with 95% confidence limits
#WPI is a matrix of n x t values (n = number of runs, t=number of years)
#calculate with mean, mode or median
graph.WPI <- function(wpi,fun=mean,title,years){
        
        require(ggplot2)
        FUN<-match.fun(fun)
        ct<-apply(wpi,2,FUN)
        lo<-apply(wpi,2,quantile,0.025)
        hi<-apply(wpi,2,quantile,0.975)
        res<-data.frame(year=1:years,ct=ct,lo=lo,hi=hi)
        #res<-melt(res,id.vars=c('year'))
        
        p<-ggplot(data=res, aes(x=year))
        p<-p+geom_line(aes(y=ct),size=2)
        p<-p+geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.2)+xlab("Year")+ylab("Wildlife Picture Index")+labs(title="")+geom_hline(yintercept=1,size=0.5,linetype=2) + ylim(0,2)+labs(title=title)
        p
        
        #ggsave("SpeciesRichness.pdf",p,width=15,height=8,units="cm")
}

graph.WPI(wpi = WPIspDec,fun = mean,title = "All species decreasing", years =16)
graph.WPI(wpi = WPIspHalf,fun = mean,title = "Half species decreasing", years =16)
graph.WPI(wpi = WPIspQtr,fun = mean,title = "One quarter species decreasing", years =16)
graph.WPI(wpi = WPIcontrol,fun = mean,title = "No species decreasing", years =16)
graph.WPI(wpi = WPIspOne,fun = mean,title = "1 species decreasing", years =16)

# do it with Low detection probability

spDec2 <- simulateCommunity(nspecies = 16,points = 60,years = 16,nsim = 100,phi = 0.9,gamma = 0, p=0.08)
control2 <- simulateCommunity(nspecies = 16,points = 60,years = 16,nsim = 100,phi = 0.9,gamma = 0.1, p = 0.08)

spHalf <- abind(spDec2[,,1:8],control2[,,1:8], along = 3)
spQtr <- abind(spDec2[,,1:4],control2[,,1:12],along = 3)

WPIspDec <- f.WPI(spDec2[,1:16,])
WPIspHalf <- f.WPI(spHalf[,1:16,])
WPIspQtr <- f.WPI(spQtr[,1:16,])
WPIcontrol <-f.WPI(control2[,1:16,])

graph.WPI(wpi = WPIspDec,fun = mean,title = "All species decreasing", years =16)
graph.WPI(wpi = WPIspHalf,fun = mean,title = "Half species decreasing", years =16)
graph.WPI(wpi = WPIspQtr,fun = mean,title = "One quarter species decreasing", years =16)
graph.WPI(wpi = WPIcontrol,fun = mean,title = "No species decreasing", years =16)
