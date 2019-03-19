# Revisit the way the number of years required to detect change is calculated
# J. Ahumada Conservation International Jun 22 2018

library(ggplot2)
library(tibble)
#library(fields)
#library(Deducer)
library(plyr)
library(dplyr)

source("dynoFunctions.R")

#Load the file with all the simulation results
det.year <- readRDS(file="AllSimulations_June2018_cleaned.rds")
det.year <- rownames_to_column(det.year)
lower80 <- det.year[,10:19]
higher80 <- det.year[, c(20:29)]
#add a last column of NAs to calculate differences between years
higher80 <- mutate(higher80, NA_c = NA)
mean80 <- det.year[,c(30:39)]
median80 <- det.year[, c(40:49)]




# Find out where are the parmt combinations that show change

diffs <- round(higher80[,-1] - lower80[,1],2) # calculate when change happened
years_change <- diffs < 0
params_change <- apply(years_change, 1, sum, na.rm = T)
params_change <- which(params_change > 0) # which rows have parameters where change was detected
# New calculation for year of change
y80 <- apply(years_change, 1, f_change)
# Does the time series pass condition 1? Confidence intervals are wider than 0
y80cond1 <- condition1(1:dim(det.year)[1], higher80[,1:10], lower80)
# Does the time series pass condition 2? There are no oscillatory dynamics
y80cond2 <- logical()
for(i in 1:dim(det.year)[1]) {
        y80cond2[i] <- condition2(higher80[i,1:10], lower80[i,])
}


temp_year80 <- cbind(y80, y80cond1, y80cond2)

newy80 <- ifelse(temp_year80[,2]+temp_year80[,3] < 2, NA, temp_year80[,1])

det.year_backup <- det.year
det.year$z.first80 <- newy80

# Rows 14 and 31 have oscillations that were not picked up by condition2 code
# Thus I am replacing manually

det.year$z.first80[c(14,31)] <- NA


# Now repeat with z.first90 and z.first95

lower90 <- det.year[,50:59]
higher90 <- det.year[, 60:69]
higher90 <- mutate(higher90, NA_c = NA)

diffs <- round(higher90[,-1] - lower90[,1],1) # calculate when change happened
years_change <- diffs < 0
params_change <- apply(years_change, 1, sum, na.rm = T)
params_change <- which(params_change > 0) # which rows have parameters where change was detected
# New calculation for year of change
y90 <- apply(years_change, 1, f_change)
# Does the time series pass condition 1?
y90cond1 <- condition1(1:dim(det.year)[1], higher90[, 1:10], lower90)
# Does the time series pass condition 2?
#y90cond2 <- condition2(1:dim(det.year)[1], higher90, lower90)
y90cond2 <- logical()
for(i in 1:dim(det.year)[1]) {
        y90cond2[i] <- condition2(higher90[i,1:10], lower90[i,])
}
temp_year90 <- cbind(y90, y90cond1, y90cond2)

newy90 <- ifelse(temp_year90[,2]+temp_year90[,3] < 2, NA, temp_year90[,1])

det.year$z.first90 <- newy90


# for z.first95


lower95 <- det.year[,90:99]
higher95 <- det.year[, c(100:109)]
higher95 <- mutate(higher95, NA_c = NA)

diffs <- round(higher95[,-1] - lower95[,1],1) # calculate when change happened
years_change <- diffs < 0
params_change <- apply(years_change, 1, sum, na.rm = T)
params_change <- which(params_change > 0) # which rows have parameters where change was detected
# New calculation for year of change
y95 <- apply(years_change, 1, f_change)
# Does the time series pass condition 1?
y95cond1 <- condition1(1:dim(det.year)[1], higher95[,1:10], lower95)
# Does the time series pass condition 2?
y95cond2 <- logical()
for(i in 1:dim(det.year)[1]) {
        y95cond2[i] <- condition2(higher95[i,1:10], lower95[i,])
}

temp_year95 <- cbind(y95, y95cond1, y95cond2)

newy95 <- ifelse(temp_year95[,2]+temp_year95[,3] < 2, NA, temp_year95[,1])

det.year$z.first95 <- newy95

# get rid of the first column.. Do not need the row ID for the app

det.year <- det.year[,-1]

saveRDS(det.year, "AllSimulations_Feb8_2019_cleaned.rds")
