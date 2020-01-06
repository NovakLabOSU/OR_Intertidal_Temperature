rm(list=ls())
options(stringsAsFactors=F)

library(plyr)
library(tidyr)
library(lubridate)
library(scales)

setwd('/Volumes/NovakLab/Projects/OR_Intertidal/Temperature/')
############################
# Load convenience functions
############################
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r')

#############
# Import data
#############
dat<-read.csv('Temp-Data/TempsCleaned_YB_Logger_15min.csv')
LDay<-read.csv('Temp-Data/TempsCleaned_YB_Logger_Day.csv')
LWeek<-read.csv('Temp-Data/TempsCleaned_YB_Logger_Week.csv')
LMonth<-read.csv('Temp-Data/TempsCleaned_YB_Logger_Month.csv')
SDay<-read.csv('Temp-Data/TempsCleaned_YB_Site_Day.csv')
SWeek<-read.csv('Temp-Data/TempsCleaned_YB_Site_Week.csv')
SMonth<-read.csv('Temp-Data/TempsCleaned_YB_Site_Month.csv')

#########################
# Plots - Logger-specific
#########################
#~~~~~~~~~~~~~~~~~~
# Raw data (15 min)
#~~~~~~~~~~~~~~~~~~
loggers<-unique(dat$Logger)
ylims=range(dat$Temp)
dat$DateTime<-ymd_hms(dat$DateTime)
colfoo<-col_numeric(colorRamp(c("Blue", "Yellow","Red"), interpolate="spline"),dat$Temp)

for(l in 1:length(loggers)){
	tdat<-subset(dat,Logger=loggers[l])
	pdf(paste0('Temp-Output/ExpPatch_Temp_15min_',loggers[l],'.pdf'),width=4,height=3)
	par(cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.8,cex.main=0.8)
		plot(tdat$DateTime,tdat$Temp,xlab='',ylab='Temp',pch=21,col='grey30', lwd=0.1,bg=colfoo(tdat$Temp), cex=0.5, main=loggers[l],ylim=ylims,axes=FALSE)
		ax1.at<-pretty(tdat$DateTime,6)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		axis(2)
		box(lwd=1)

	dev.off()
}

######################
# Plots - Site average
######################
#~~~~~
# Date
#~~~~~
SDay$Date<-as.Date(SDay$Date)
colfoo<-col_numeric(colorRamp(c("Blue", "Yellow","Red"), interpolate="spline"),SDay$Temp)

pdf(paste0('Temp-Output/ExpPatch_Temp_Day_Site.pdf'),width=4,height=3)
par(cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.8,cex.main=0.8,las=2)
		plot(SDay$Date,SDay$Temp,xlab='',ylab='Daily mean temperature',pch=21,col='grey30', lwd=0.1,bg=colfoo(SDay$Temp), cex=0.5,axes=FALSE)
		ax1.at<-pretty(SDay$Date,10)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		axis(2)
		box(lwd=1)
dev.off()


