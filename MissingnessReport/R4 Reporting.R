##########################
##QA/QC Taskforce R4 API##
##Author: jirizarry#######
##########################


##Package loading##
#install.packages("REDCapR")#
library(REDCapR)
#install.packages("RCurl")#
library(RCurl)
#install.packages("tidyverse")#
library(tidyverse)
#install.packages("rms")#
library(rms)
#install.packages("Hmisc")#
library(Hmisc)
#install.packages("qreport")#
library(qreport)
#install.packages("plyr")#
library(plyr)

options(digits = 2)
options(na.detail.response=TRUE, na.action = na.keep)

##Variable definitions##
accessKey="Each person must provide their own API key here"
apiUrl="https://redcap.vanderbilt.edu/api/"


##Directory to save latex output##
dir='Change this to the directory you wish to output the LaTeX files to'

##Please change the following site variable to the name of your site according to the RedCap Instrument names##
#The possible values are here:#
#vumc, cchmc, chop, columbia, mgb, mphc, uab, uw, mt_sinai, nu#
#Please note: The name MUST be enclosed by either single or double quotation marks#
#exampleSite = 'uw'
site='uw'

##Pull data##

#First, we pull the list of all forms and then select the ones we need#
forms=redcap_instruments(redcap_uri =  apiUrl, token =  accessKey)
formNames=forms$data
siteForm=paste(site, "_consent_part_2", sep='')
collapsedForms=paste("prescreening_survey,primary_consent,", siteForm ,",baseline_survey_adult,pre_ror_adult", sep = '')
#Using redcap_read we can pull the specific set of forms we just created#
pulledForms=redcap_read(redcap_uri =  apiUrl, token =  accessKey, forms_collapsed=collapsedForms)
data=pulledForms$data


##QC##
#Select only participants who have fully consented#
consented=data[data['primary_consent_complete']==2 & data[paste(siteForm, "_complete", sep = '')]==2, ]

##Variable Selection##
#Select variables of interest from taskforce meeting#

#ID#
id=consented$record_id

#BMI calculated from height & weight#
#Height#
heightFeet=consented$height_feet
heightInches=consented$height_inches

#Impute people who have height in feet but not inches with zero
heightInches[!is.na(heightFeet) & is.na(heightInches)]=0
#Mark as NA person with missing height in feet#
heightInches[is.na(heightFeet) & !is.na(heightInches)]=NA

#Verify NA counts are identical for both variables#
table(is.na(heightFeet), is.na(heightInches))

table(is.na(heightInches))

length(consented[is.na(heightFeet) & !is.na(heightInches),])

#Combine into one variable#
height=as.numeric(heightFeet) + as.numeric(heightInches) / 12

#Weight#
weight=consented$current_weight

#Race/Ethnicity#
#13:80#
#Isolate race variables into main groups#
#American Indian#
amerInd=consented[, 13:16]
amerInd=amerInd %>%
  mutate(sum=rowSums(.))
isAI=amerInd[, ncol(amerInd)]
isAI[isAI>=1]=1

#Asian#
eastAsian=consented[, c(18:23, 25:26)]
eastAsian=eastAsian %>%
  mutate(sum=rowSums(.))
isEA=eastAsian[, ncol(eastAsian)]
isEA[isEA>=1]=1

southAsian=consented[, c(17,24)]
southAsian=southAsian %>%
  mutate(sum=rowSums(.))
isSA=southAsian[, ncol(southAsian)]
isSA[isSA>=1]=1

#African#
african=consented[, 27:38]
african=african %>%
  mutate(sum=rowSums(.))
isAF=african[, ncol(african)]
isAF[isAF>=1]=1

#Hispanic#
hispanic=consented[, 39:48]
hispanic=hispanic %>%
  mutate(sum=rowSums(.))
isHI=hispanic[, ncol(hispanic)]
isHI[isHI>=1]=1

#Middle Eastern#
midEast=consented[, 49:59]
midEast=midEast %>%
  mutate(sum=rowSums(.))
isME=midEast[, ncol(midEast)]
isME[isME>=1]=1

#Hawaiian#
hawaii=consented[, 60:68]
hawaii=hawaii %>%
  mutate(sum=rowSums(.))
isHA=hawaii[, ncol(hawaii)]
isHA[isHA>=1]=1

#White#
white=consented[, 69:80]
white=white %>%
  mutate(sum=rowSums(.))
isWH=white[, ncol(white)]
isWH[isWH>=1]=1

#Combine into one variable#
raceDF=data.frame(isAF, isAI, isEA, isSA, isHA, isHI, isME, isWH)
raceDF=raceDF %>%
  mutate(sum=rowSums(.))

selfReportRace=rep(NA, 2785)
selfReportRace[raceDF$isAF==1]='African'
selfReportRace[raceDF$isAI==1]='American Indian'
selfReportRace[raceDF$isEA==1]='East Asian'
selfReportRace[raceDF$isSA==1]='South Asian'
selfReportRace[raceDF$isHI==1]='Hispanic'
selfReportRace[raceDF$isHA==1]='Hawaiian'
selfReportRace[raceDF$isME==1]='Middle Eastern'
selfReportRace[raceDF$isWH==1]='White'
selfReportRace[raceDF$isWH==1 & raceDF$isAF==1]='African'
selfReportRace[raceDF$isWH==1 & raceDF$isAI==1]='American Indian'
selfReportRace[raceDF$isWH==1 & raceDF$isEA==1]='East Asian'
selfReportRace[raceDF$isWH==1 & raceDF$isSA==1]='South Asian'
selfReportRace[raceDF$isWH==1 & raceDF$isHI==1]='Hispanic'
selfReportRace[raceDF$isWH==1 & raceDF$isHA==1]='Hawaiian'
selfReportRace[raceDF$isWH==1 & raceDF$isME==1]='Middle Eastern'
selfReportRace[raceDF$sum>=2 & is.na(selfReportRace)]='Otherwise diverse'
selfReportRace[raceDF$sum>=3]='Otherwise diverse'
table(selfReportRace)

#Education#
education=consented$highest_grade_level

#Alcohol Use#
alcohol=consented$drink_containing_alcohol

#Physical Activity#
moderateActivities=consented$moderate_activities_such_a



##Report making##
#Combine variables into a single data frame#
dataMatrix=cbind(id, height, weight, selfReportRace, education, alcohol, moderateActivities)
dataFrame=data.frame(dataMatrix)

#Convert variables into usable data types#
dataFrame$height=as.numeric(dataFrame$height)
dataFrame$weight=as.numeric(dataFrame$weight)
dataFrame$selfReportRace=as.factor(dataFrame$selfReportRace)
dataFrame$education=as.factor(dataFrame$education)
dataFrame$alcohol=as.factor(dataFrame$alcohol)
dataFrame$moderateActivities=as.factor(dataFrame$moderateActivities)

#Add labels to factor variables#
dataFrame$education=mapvalues(dataFrame$education, from=levels(dataFrame$education), to=c('Some HS', 'HS/GED', 'Some college', "Bachelor's", 'Advanced degree') )
dataFrame$alcohol=mapvalues(dataFrame$alcohol, from=levels(dataFrame$alcohol), to=c('Never', 'Monthly or less', '2-4 a month', '2-3 a week', '4 or more a week', 'Skip') )
dataFrame$moderateActivities=mapvalues(dataFrame$moderateActivities, from=levels(dataFrame$moderateActivities), to=c('Limited a lot', 'Limited a little', 'Not limited') )

str(dataFrame)

usedData=dataFrame[, -1]

#Summarize all variables against self report race#
tab1=summary(selfReportRace~., data=usedData ,continuous = 2, overall=TRUE, na.action = na.keep, method='reverse', test=TRUE, na.include=TRUE)
#Output summary to LaTeX#
latex(tab1, file = paste(dir,'table1.tex', sep=""),size='tiny',outer.size='tiny',Nsize='tiny',na.action=na.keep,table.env=TRUE,where="!tbp", na.include=TRUE, prn=TRUE,hspace="-10ex", msdsize='tiny',middle.bold=TRUE, ctable=FALSE,long=TRUE, npct.size='tiny',greek=TRUE,exclude1 = FALSE, nomargins=TRUE,units=TRUE)

#Since the missingness for continuous variables isn't shown above,#
#we now use describe only on our continuous variables#
continuous=usedData[, 1:2]

description=describe(continuous)
latex(description, file= paste(dir,'describe.tex', sep=""),size='tiny',outer.size='tiny',Nsize='tiny')
