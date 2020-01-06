rm(list=ls())
options(stringsAsFactors=F)

library(plyr)
library(tidyr)
library(lubridate)
# library(zoo)
# library(tsoutliers)

############################
# Load convenience functions
############################
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r')

#############
# Import data
#############
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/Temperature/')
files<-list.files('/Volumes/NovakLab/Projects/OR_Intertidal/Temperature/Temp-HoboData', pattern='csv',recursive=TRUE,full.names=TRUE)

##############
# Process data
##############
dat<-dim(0)
for(i in 1:length(files)){
	tlog<-substr(files[i],regexpr('YB_',files[i])[1]+3,nchar(files[i])-4)
	tdat<-read.csv(files[i],skip=2,header=FALSE)
	# The following deals with the fact that Hobo can spit out 
	#   Date-Time combined or in seperate columns
	ncols<-ncol(tdat)
	tdat<-tdat[,c(2:(ncols-4))]
	if(ncol(tdat)==3){
		tdat$DateTime<-paste(tdat[,1],tdat[,2])
		tdat<-tdat[,c(4,3)]
	}
	colnames(tdat)<-c('DateTime','Temp')
	tdat<-tdat[complete.cases(tdat),] # Remove NAs
	tdat$DateTime<-mdy_hms(tdat$DateTime)
	# Remove first 24hrs
	tdat<-tdat[-seq(1,24*4,1),]
	################
	# Scrub outliers
	################
	tdat<-subset(tdat,Temp > -10 & Temp <40) 
	# Do something fancier in the future
	# Convert to time series
# 	TSO<- tso(y = ts(tdat$Temp), types = c("AO", "LS", "TC"), tsmethod = "stsm", args.tsmodel = list(model = "local-level"))	

	tdat<-data.frame(Logger=tlog,tdat)
	dat<-rbind(dat,tdat)	
}

###################################
# Average over various time-scales at site-scale
# NOTE:  Not using a sliding window
###################################
dat$Date<-as.Date(dat$DateTime)
dat$Ywk<-paste0(year(dat$DateTime),'-',week(dat$DateTime))
dat$YM<-paste0(year(dat$DateTime),'-',month(dat$DateTime))

# Logger-specific
# Daily
LDay<-ddply(dat,.(Logger,Date=Date),summarise,Temp=round(mean(Temp),2))
# Weekly
LWeek<-ddply(dat,.(Logger,Week=Ywk),summarise,Temp=round(mean(Temp),2))
# Monthly
LMonth<-ddply(dat,.(Logger,Month=YM),summarise,Temp=round(mean(Temp),2))

# Site-average
# Daily
SDay<-ddply(dat,.(Date=Date),summarise,Temp=round(mean(Temp),2))
# Weekly
SWeek<-ddply(dat,.(Week=Ywk),summarise,Temp=round(mean(Temp),2))
# Monthly
SMonth<-ddply(dat,.(Month=YM),summarise,Temp=round(mean(Temp),2))

#############
# Export data
#############
write.table(dat,'Temp-Data/TempsCleaned_YB_Logger_15min.csv', sep=',',row.names=FALSE)
write.table(LDay,'Temp-Data/TempsCleaned_YB_Logger_Day.csv', sep=',',row.names=FALSE)
write.table(LWeek,'Temp-Data/TempsCleaned_YB_Logger_Week.csv', sep=',',row.names=FALSE)
write.table(LMonth,'Temp-Data/TempsCleaned_YB_Logger_Month.csv', sep=',',row.names=FALSE)
write.table(SDay,'Temp-Data/TempsCleaned_YB_Site_Day.csv', sep=',',row.names=FALSE)
write.table(SWeek,'Temp-Data/TempsCleaned_YB_Site_Week.csv', sep=',',row.names=FALSE)
write.table(SMonth,'Temp-Data/TempsCleaned_YB_Site_Month.csv', sep=',',row.names=FALSE)
###############################################################################
###############################################################################
###############################################################################

