##------ Geospatial assessment of county-level racial composition  ------##
#        and injustices in public drinking water metal exposures
#        across the United States
#   Updated 8 September 2021
#   Contact:                                                                      
#   Irene Martinez-Morata                                                                  
#   Columbia University Mailman School of Public Health                             
#   Environmental Health Sciences                                                   


#setwd("~/Desktop/PNAS PAPER/PNAS DATASETS/table 2")
#setwd("~/Desktop/PNAS PAPER/RECTA FINAL")
setwd("~/Dropbox/Mac/Documents/Research/Irene_Metals_Justice")
#####----- 0. Some packages and functions -----########  
library(Hmisc)     
library(dplyr)
library(openxlsx)
library(readr)

# Functions
convert.to.ci <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ] , "-" , vector[ 3 ], ")" ) 
  return( vector )
}

convert.to.mean <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ], ")" ) 
  return( vector )
}

to.mean.se <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],") " ) 
  return( vector )
}

mean.se.N <- function( vector )
{
  vector <- paste0( vector[1] , " (" , vector[ 2 ],") ; N= ", vector [3 ] ) 
  return( vector )
}


### end


#####----- 1. Load databases and tidy ######
#####-----    1A. County Health Ranking database and tidy #######

## Load County Health Rankings 2013 analytic data set, downloaded directly from: https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation/national-data-documentation-2010-2019
racedata <- read.csv("analytic_data2013.csv")

# Fix county FIPS codes
library(stringr)
racedata$fipscode<-str_pad(racedata$fipscode, width=5, pad="0")
racedata$fipscode<-as.character(racedata$fipscode)
colnames(racedata)[colnames(racedata)=="fipscode"] <- "CountyFIPS"
colnames(racedata)[colnames(racedata)=="statecode"] <- "STATEFP"

# Fix variable names in County Health Ranking database
describe(racedata$v051_rawvalue) # Population raw value
colnames(racedata)[colnames(racedata)=="v051_rawvalue"] <- "jPopulatio"
describe(racedata$v054_rawvalue) # % Non-Hispanic African American raw value
colnames(racedata)[colnames(racedata)=="v054_rawvalue"] <- "jAfrican.A"
describe(racedata$v055_rawvalue) # % American Indian or Alaskan Native  raw value
colnames(racedata)[colnames(racedata)=="v055_rawvalue"] <- "jAmerican"
describe(racedata$v081_rawvalue) # % Asian  raw value
colnames(racedata)[colnames(racedata)=="v081_rawvalue"] <- "jAsian"
describe(racedata$v080_rawvalue) # % Native Hawaiian  raw value
colnames(racedata)[colnames(racedata)=="v080_rawvalue"] <- "jNatHawaiian"
describe(racedata$v056_rawvalue) # % Hispanic  raw value
colnames(racedata)[colnames(racedata)=="v056_rawvalue"] <- "jHispanic"
describe(racedata$v126_rawvalue) # % Non-Hispanic white  raw value
colnames(racedata)[colnames(racedata)=="v126_rawvalue"] <- "jWhite"
describe(racedata$v063_rawvalue) # Median household income
colnames(racedata)[colnames(racedata)=="v063_rawvalue"] <- "jMedian.ho"
describe(racedata$v021_rawvalue) # % with high school diploma
colnames(racedata)[colnames(racedata)=="v021_rawvalue"] <- "jHigh.scho"
describe(racedata$v058_rawvalue) # % living in rural areas
colnames(racedata)[colnames(racedata)=="v058_rawvalue"] <- "jRural"

## Note: potentially come back and clean up other variables that could be of interest
# Create new variable of number of residents in each racial/ethnic group
racedata$jNafrican <- (racedata$jPopulatio * racedata$jAfrican.A)
racedata$jNafrican <- round(racedata$jNafrican,digits=0)
describe(racedata$jNafrican)

racedata$jNindian <- (racedata$jPopulatio * racedata$jAmerican)
racedata$jNindian <- round(racedata$jNindian,digits=0)
describe(racedata$jNindian)

racedata$jNAsian <- (racedata$jPopulatio * racedata$jAsian)
racedata$jNAsian <- round(racedata$jNAsian,digits=0)
describe(racedata$jNAsian)

racedata$jNNatHawaii <- (racedata$jPopulatio * racedata$jNatHawaiian)
racedata$jNNatHawaii <- round(racedata$jNNatHawaii,digits=0)
describe(racedata$jNNatHawaii)

racedata$jNhispanic <- (racedata$jPopulatio * racedata$jHispanic)
racedata$jNhispanic <- round(racedata$jNhispanic,digits=0)
describe(racedata$jNhispanic)

racedata$jNwhite <- (racedata$jPopulatio * racedata$jWhite)
racedata$jNwhite <- round(racedata$jNwhite,digits=0)
describe(racedata$jNwhite)

# Create % and N BIPOC (eg, non-white non-Hispanic) residents
racedata$jBIPOC <- ( 1- racedata$jWhite)
describe(racedata$jBIPOC)
describe(racedata$jWhite)
racedata$jNBIPOC <- ( racedata$jPopulatio- racedata$jNwhite)
describe(racedata$jNBIPOC)
describe(racedata$jNwhite)
describe(racedata$jPopulatio)


myvars<-c("CountyFIPS","STATEFP","countycode", "jPopulatio", "jAfrican.A","jAmerican","jAsian","jNatHawaiian", "jHispanic","jWhite","jBIPOC",
          "jMedian.ho","jHigh.scho","jRural",
          "jNafrican","jNindian","jNAsian","jNNatHawaii", "jNhispanic","jNwhite","jNBIPOC")
racedata2<-racedata[myvars]


### end
#####-----    1B. Census data and calculate population density #######
# Census data downloaded directly from: https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html#LND
land<-read.xlsx("LND01 2.xlsx")
myv<-c("STCOU","Areaname", "LND110210D")
land<-land[myv]

pop1<-read.xlsx("POP01.xlsx",sheet=1)
pop2<-read.xlsx("POP01.xlsx",sheet=2)
pop3<-read.xlsx("POP01.xlsx",sheet=3)
pop4<-read.xlsx("POP01.xlsx",sheet=4)
pop5<-read.xlsx("POP01.xlsx",sheet=5)
pop6<-read.xlsx("POP01.xlsx",sheet=6)
pop7<-read.xlsx("POP01.xlsx",sheet=7)
pop8<-read.xlsx("POP01.xlsx",sheet=8)
pop9<-read.xlsx("POP01.xlsx",sheet=9)
pop10<-read.xlsx("POP01.xlsx",sheet=10)
pop11<-read.xlsx("POP02.xlsx",sheet=2)

pop<-merge(pop1,pop2,by="STCOU")
pop<-merge(pop,pop3,by="STCOU")
pop<-merge(pop,pop4,by="STCOU")
pop<-merge(pop,pop5,by="STCOU")
pop<-merge(pop,pop6,by="STCOU")
pop<-merge(pop,pop7,by="STCOU")
pop<-merge(pop,pop8,by="STCOU")
pop<-merge(pop,pop9,by="STCOU")
pop<-merge(pop,pop10,by="STCOU")
pop<-merge(pop,pop11,by="STCOU")

myv<-c("STCOU","POP010210D","POP230210D","POP235210D","POP250210D","POP255210D","POP280210D","POP285210D",
       "POP320210D","POP325210D","POP350210D","POP355210D","POP400210D","POP405210D")
pop<-pop[myv]

# POP010210D  Resident population (April 1 - complete count) 2010
# POP230210D  Population of one race - White alone, not Hispanic or Latino 2010 (complete count))
# POP235210D  Population of one race - percent White alone, not Hispanic or Latino 2010 (complete count)
# POP250210D  Population of one race - Black or African American alone 2010 (complete count)
# POP255210D  Population of one race - percent Black or African American alone 2010 (complete count)
# POP280210D 	Population of one race - American Indian and Alaska Native alone 2010 (complete count)
# POP285210D	Population of one race - percent American Indian and Alaska Native alone 2010 (complete count)
# POP320210D	Population of one race - Asian alone 2010 (complete count)
# POP325210D	Population of one race - percent Asian alone 2010 (complete count)
# POP350210D	Population of one race - Native Hawaiian and Other Pacific Islander alone 2010 (complete count)
# POP355210D	Population of one race - percent Native Hawaiian and Other Pacific Islander alone 2010 (complete count)
# POP400210D	Hispanic or Latino population 2010 (complete count)
# POP405210D	Percent Hispanic or Latino population 2010 (complete count)

censusdata<-merge(land,pop,by="STCOU",all=T)

# Create county population density
censusdata$DENSITY2010<- censusdata$POP010210D / censusdata$LND110210D
library(Hmisc)
describe(censusdata$DENSITY2010)
censusdata$DENSITY2010<-round(censusdata$DENSITY2010, digits=2)
censusdata$STCOU<-as.character(censusdata$STCOU)


# Fix variables names
colnames(censusdata)[colnames(censusdata)=="DENSITY2010"] <- "jdensity"
colnames(censusdata)[colnames(censusdata)=="POP230210D"] <- "CenNWhite"
colnames(censusdata)[colnames(censusdata)=="POP235210D"] <- "CenPWhite"
colnames(censusdata)[colnames(censusdata)=="POP250210D"] <- "CenNBlack"
colnames(censusdata)[colnames(censusdata)=="POP255210D"] <- "CenPBlack"
colnames(censusdata)[colnames(censusdata)=="POP280210D"] <- "CenNAmericanI"
colnames(censusdata)[colnames(censusdata)=="POP285210D"] <- "CenPAmericanI"
colnames(censusdata)[colnames(censusdata)=="POP320210D"] <- "CenNAsian"
colnames(censusdata)[colnames(censusdata)=="POP325210D"] <- "CenPAsian"
colnames(censusdata)[colnames(censusdata)=="POP350210D"] <- "CenNNatHawaii"
colnames(censusdata)[colnames(censusdata)=="POP355210D"] <- "CenPNatHawaii"
colnames(censusdata)[colnames(censusdata)=="POP400210D"] <- "CenNHispanic"
colnames(censusdata)[colnames(censusdata)=="POP405210D"] <- "CenPHispanic"

# Change percentile values into proportions
censusdata$CenPWhite<-censusdata$CenPWhite/100
censusdata$CenPBlack<-censusdata$CenPBlack/100
censusdata$CenPAmericanI<-censusdata$CenPAmericanI/100
censusdata$CenPAsian<-censusdata$CenPAsian/100
censusdata$CenPNatHawaii<-censusdata$CenPNatHawaii/100
censusdata$CenPHispanic<-censusdata$CenPHispanic/100

# Create % BIPOC (eg, non-white non-Hispanic) residents
censusdata$CenPBIPOC <- ( 1- censusdata$CenPWhite)
describe(censusdata$CenPBIPOC)
describe(censusdata$CenPWhite)

# Fix countyFIPS
colnames(censusdata)[colnames(censusdata)=="STCOU"] <- "CountyFIPS"

write.xlsx(censusdata, row.names = F, overwrite = T, file ="countypopdensity2010.xlsx")

### end

#####-----    1C. Public water metal exposure estimates #######
## 
metaldata<-read.xlsx("CountyLevelInorganics16June21.xlsx")
#metaldata <- read.csv("finalmetalsfile16.07.csv")
## NOTE: This should be the same as CountyLevelInorganics16June21.xlsx

# Fix county FIPS codes
metaldata$CountyFIPS<-as.character(metaldata$CountyFIPS)

# Values recorded as 999999.00 should be treated as missing

# As:
metaldata$AsWeightedAvg20062011[which(metaldata$AsWeightedAvg20062011==999999.00)]<-NA
metaldata$AsWeightedAvg20082010[which(metaldata$AsWeightedAvg20082010==999999.00)]<-NA
metaldata$AsWeightedp9020062011[which(metaldata$AsWeightedp9020062011==999999.00)]<-NA
metaldata$AsWeightedp9520062011[which(metaldata$AsWeightedp9520062011==999999.00)]<-NA
metaldata$AsWeightedp9020082010[which(metaldata$AsWeightedp9020082010==999999.00)]<-NA
metaldata$AsWeightedp9520082010[which(metaldata$AsWeightedp9520082010==999999.00)]<-NA

# U:
metaldata$UWeightedAvg20002011[which(metaldata$UWeightedAvg20002011==999999.00)]<-NA
metaldata$UWeightedAvg20002007[which(metaldata$UWeightedAvg20002007==999999.00)]<-NA
metaldata$UWeightedAvg20082011[which(metaldata$UWeightedAvg20082011==999999.00)]<-NA
metaldata$UWeightedp9020002007[which(metaldata$UWeightedp9020002007==999999.00)]<-NA
metaldata$UWeightedp9520002007[which(metaldata$UWeightedp9520002007==999999.00)]<-NA
metaldata$UWeightedp9020082011[which(metaldata$UWeightedp9020082011==999999.00)]<-NA
metaldata$UWeightedp9520082011[which(metaldata$UWeightedp9520082011==999999.00)]<-NA
metaldata$UWeightedp9020002011[which(metaldata$UWeightedp9020002011==999999.00)]<-NA
metaldata$UWeightedp9520002011[which(metaldata$UWeightedp9520002011==999999.00)]<-NA

# Ba:
metaldata$BaWeightedAvg20062011[which(metaldata$BaWeightedAvg20062011==999999.00)]<-NA
metaldata$BaWeightedAvg20082010[which(metaldata$BaWeightedAvg20082010==999999.00)]<-NA
metaldata$BaWeightedp9020062011[which(metaldata$BaWeightedp9020062011==999999.00)]<-NA
metaldata$BaWeightedp9520062011[which(metaldata$BaWeightedp9520062011==999999.00)]<-NA
metaldata$BaWeightedp9020082010[which(metaldata$BaWeightedp9020082010==999999.00)]<-NA
metaldata$BaWeightedp9520082010[which(metaldata$BaWeightedp9520082010==999999.00)]<-NA

# Se:
metaldata$SeWeightedAvg20062011[which(metaldata$SeWeightedAvg20062011==999999.00)]<-NA
metaldata$SeWeightedAvg20082010[which(metaldata$SeWeightedAvg20082010==999999.00)]<-NA
metaldata$SeWeightedp9020062011[which(metaldata$SeWeightedp9020062011==999999.00)]<-NA
metaldata$SeWeightedp9520062011[which(metaldata$SeWeightedp9520062011==999999.00)]<-NA
metaldata$SeWeightedp9020082010[which(metaldata$SeWeightedp9020082010==999999.00)]<-NA
metaldata$SeWeightedp9520082010[which(metaldata$SeWeightedp9520082010==999999.00)]<-NA


# Fix mean concentration value column names
colnames(metaldata)[colnames(metaldata)=="AsWeightedAvg20062011"] <- "meanAs"
colnames(metaldata)[colnames(metaldata)=="UWeightedAvg20002011"] <- "meanU"
colnames(metaldata)[colnames(metaldata)=="BaWeightedAvg20062011"] <- "meanBa"
colnames(metaldata)[colnames(metaldata)=="SeWeightedAvg20062011"] <- "meanSe"

# Create log scale 
metaldata$logAs<-log(metaldata$meanAs)
metaldata$logU<-log(metaldata$meanU)
metaldata$logBa<-log(metaldata$meanBa)
metaldata$logSe<-log(metaldata$meanSe)



### end 


#####-----    1D. Public water supply data from USGS #######
# If missing 2010 value, use the 2015 value
# Downloaded directly from: https://water.usgs.gov/watuse/data/
### Load 2010 data
# Downloaded directly from: https://water.usgs.gov/watuse/data/2010/usco2010.xlsx
g2010<-read.xlsx("usco2010.xlsx")
library(stringr)
g2010$FIPS<-as.factor(g2010$FIPS)
g2010$TotalGW10<- g2010$`PS-WGWTo`
g2010$TotalSW10<- g2010$`PS-WSWTo`
g2010$GWPerc10<- g2010$TotalGW10 / (g2010$TotalGW10 + g2010$TotalSW10)
describe(g2010$GWPerc10)
myvar<-c("FIPS", "GWPerc10", "TotalGW10", "TotalSW10")
g2010a<-g2010[myvar]

### Load 2015 data
# Downloaded directly from: https://www.sciencebase.gov/catalog/item/get/5af3311be4b0da30c1b245d8
g2015<-read.xlsx("usco2015v2.0.xlsx")
g2015$FIPS<-str_pad(g2015$FIPS, width=5, pad="0")
g2015$FIPS<-as.factor(g2015$FIPS)
g2015$TotalGW15<- g2015$`PS-WGWTo`
g2015$TotalSW15<- g2015$`PS-WSWTo`
g2015$GWPerc15<- g2015$TotalGW15 / (g2015$TotalGW15 + g2015$TotalSW15)
describe(g2015$GWPerc15)
myvar<-c("FIPS", "GWPerc15", "TotalGW15", "TotalSW15")
g2015a<-g2015[myvar]

### Merge together:
gw<-merge(g2010a, g2015a, by="FIPS",all=T)

### Overall complete obvs correlation
cor(gw$GWPerc10, gw$GWPerc15, use="complete.obs", method="pearson")
# 0.930872

### Those with 0 SW and 0 GW for 2010: replace with 2015
gw$Flag<-0
gw$Flag[which(gw$TotalGW10==0&gw$TotalSW10==0)]<-1
table(gw$Flag) # 100
flag<-gw[which(gw$Flag==1),]
describe(flag$TotalGW15) #86 zero
describe(flag$TotalSW15) #86 zero

## If GW and SW 2015 are both zero, replace with NA
gw$GWPerc10[which(gw$TotalGW10==0&gw$TotalSW10==0)]<-gw$GWPerc15[which(gw$TotalGW10==0&gw$TotalSW10==0)]
gw$GWPerc10[which(is.na(gw$GWPerc10))]<-gw$GWPerc15[which(is.na(gw$GWPerc10))]
gw$GWPerc10[which(gw$GWPerc10=="NaN"&gw$GWPerc15=="NaN")]<-NA
describe(gw$GWPerc10)

# Replace zero values with non-zero min/2
# min(gw$GWPerc10[which(gw$GWPerc10>0)]) #0.0001198969

# How many counties did we replace 2010 (NA) with 2015?
describe(gw$GWPerc10[which(gw$Flag==1)]) # N = 25 times we used the 2015 estimate; for 75, we still have missing (could not rescue)

myvar<-c("FIPS", "GWPerc10","Flag")
gwa<-gw[myvar]

colnames(gwa)[colnames(gwa)=="FIPS"] <- "CountyFIPS"
gwa$CountyFIPS<-as.character(gwa$CountyFIPS)

# Save database
write.xlsx(gwa, overwrite = T, file="groundwaterestimates.xlsx")


### end

#####-----    1E. Merge all cleaned databases together ##########

data<-merge(racedata2,censusdata,by="CountyFIPS",all=T)
data<-merge(data,metaldata,by="CountyFIPS",all=T)
data<-merge(data,gwa,by="CountyFIPS",all=T)

# Merge in state names from fips codes
states<-read.xlsx("statefips.xlsx")
colnames(states)[colnames(states)=="FIPS"] <- "STATEFP"

dat<-merge(data,states,by="STATEFP",all.x=T)
dim(dat)
table(dat$STATEFP)

table(dat$Name)
colnames(dat)[colnames(dat)=="Name"] <- "StateName"
table(dat$StateName)
describe(dat$StateName) # for Washington DC the state name is missing
describe(dat$StateName)

write.xlsx(dat,overwrite = T, "covariates2_sept.07.xlsx")

describe(dat$jNatHawaiian)

#COV2 is the full dataset (including more than the mainland US)

#####----- 2. Exclusion criteria -----#####


#dat1 <- read.xlsx("~/Dropbox/Mac/Documents/Research/Irene_Metals_Justice/covariates2_sept.07.xlsx")
dat1 <- dat
glimpse(dat1)

# First, exclude the row for the entire US, and rows for individual states
dat1<-dat1[which(dat1$CountyFIPS!="00000"),]
nrow(dat1)
dat1<-dat1[which(dat1$countycode!="0"),]
nrow(dat1) #3,141 counties and county equivalents 

# Next, exclude non-US mainland counties
dat<-dat1[which(dat1$STATEFP!="60"&dat1$STATEFP!="66"&dat1$STATEFP!="69"&dat1$STATEFP!="72"&dat1$STATEFP!="78"&dat1$STATEFP!="02"&dat1$STATEFP!="15"),]
nrow(dat) # N = 3,136 counties and county equivalents

# Extract all counties from the original dataset before exclusion criteria:
allcounties<-dat

#-- Exclusion criteria:
dat.a <- dat[!is.na(dat$meanAs) | !is.na(dat$meanU) | !is.na(dat$meanBa) | !is.na(dat$meanSe),]  # exclude those missing all water estimates 
length(dat.a$CountyFIPS)-length(dat$CountyFIPS)                         #  -429 counties excluded 

excluded_miss_metals <- dat[is.na(dat$meanAs) & is.na(dat$meanU) & is.na(dat$meanBa) & is.na(dat$meanSe),]

dat.b <- dat.a[!is.na(dat.a$GWPerc10),]                                 # exclude those missing % of public water from GW supplies
length(dat.b$CountyFIPS)-length(dat.a$CountyFIPS)                         #  -34 counties excluded 
dim(dat.b) #2,770 counties

dat.c <- dat.b[!is.na(dat.b$jHigh.scho),]                                 # exclude those missing % graduating HS
length(dat.c$CountyFIPS)-length(dat.b$CountyFIPS)                         #  -20 counties excluded 
dim(dat.c) # 2,748 counties

dat.d <- dat.c[!is.na(dat.c$jPopulatio),]                                 # exclude those missing total population
length(dat.d$CountyFIPS)-length(dat.c$CountyFIPS)                         #  0 counties excluded 
dim(dat.d) #   counties

dat.e <- dat.d[!is.na(dat.d$jdensity),]                                 # exclude those missing population density
length(dat.e$CountyFIPS)-length(dat.d$CountyFIPS)                         #  0 counties excluded 
dim(dat.e) #   counties

dat.f <- dat.e[!is.na(dat.e$jHigh.scho),]                                 # exclude those missing % with high school diploma
length(dat.f$CountyFIPS)-length(dat.e$CountyFIPS)                         #  0 counties excluded 
dim(dat.f) #   counties

dat.g <- dat.f[!is.na(dat.f$jRural),]                                 # exclude those missing % rural
length(dat.g$CountyFIPS)-length(dat.f$CountyFIPS)                         #  0 counties excluded 
dim(dat.g) #   counties

data<-dat.g
nrow(data) # N= 2,653

# No counties missing either % or N for each racial/ethnic group, from CHR database or Census:
describe(data$jAfrican.A)
describe(data$jAmerican)
describe(data$jHispanic)
describe(data$jWhite)
describe(data$jNafrican)
describe(data$jNindian)
describe(data$jNhispanic)
describe(data$jNwhite)
describe(data$CenNBlack)
describe(data$CenNAmericanI)
describe(data$CenNHispanic)
describe(data$CenNWhite)
describe(data$CenPBlack)
describe(data$CenPAmericanI)
describe(data$CenPHispanic)
describe(data$CenPWhite)

describe(data$meanAs)#2607
describe(data$meanU)#1194
describe(data$meanSe)#2575
describe(data$meanBa)#2575

# Find all counties that were excluded 
allcounties_fips<-as.data.frame(allcounties$CountyFIPS)
colnames(allcounties_fips)[colnames(allcounties_fips)=="allcounties$CountyFIPS"] <- "CountyFIPS"
allcounties_fips$allcounties<-1
m<-merge(data,allcounties_fips,by="CountyFIPS",all=T)
excluded<-m[which(is.na(m$jPopulatio)),];dim(excluded)
excluded1 <- as.data.frame(excluded[,c(1)])
colnames(excluded1)[1] <- "CountyFIPS"
excluded<-merge(excluded1,allcounties,by="CountyFIPS",all.x=T)

### end


#####----- 3. Assess differences between County Health Ranking and Census databases for % in racial/ethnic groups #####
describe(data$jNwhite)
describe(data$CenNWhite)
describe(data$jWhite)
describe(data$CenPWhite)

cor(data$jWhite, data$CenPWhite, use="complete.obs", method="pearson")
cor(data$jAfrican.A, data$CenPBlack, use="complete.obs", method="pearson")
cor(data$jAmerican, data$CenPAmericanI, use="complete.obs", method="pearson")
cor(data$jAsian, data$CenPAsian, use="complete.obs", method="pearson")
cor(data$jHispanic, data$CenPHispanic, use="complete.obs", method="pearson")
cor(data$jBIPOC, data$CenPBIPOC, use="complete.obs", method="pearson")
# All correlations are at 0.999, small differences in computation but otherwise the same


### end
#####----- 4. Create sub dataframes for each racial/ethnic group, create new variables #####
# Create four databases, 1 for each racial/ethnic group (restricting to counties with at least 100 residents)
# Overall:
dataAA<-data[which(data$jNafrican>=100),];dim(dataAA) #1963
dataHis<-data[which(data$jNhispanic>=100),];dim(dataHis) #2446
dataAI<-data[which(data$jNindian>=100),];dim(dataAI) #1731
dataW<-data[which(data$jNwhite>=100),];dim(dataW) #2653
dataBIPOC<-data[which(data$jNBIPOC>=100),];dim(dataBIPOC) #2607

# With arsenic available:
nrow(dataAA[which(!is.na(dataAA$meanAs)),]) #1937
nrow(dataHis[which(!is.na(dataHis$meanAs)),]) #2402
nrow(dataAI[which(!is.na(dataAI$meanAs)),]) #1699
nrow(dataW[which(!is.na(dataW$meanAs)),]) #2607
nrow(dataBIPOC[which(!is.na(dataBIPOC$meanAs)),]) #2562

# With uranium available: 
nrow(dataAA[which(!is.na(dataAA$meanU)),]) #869
nrow(dataHis[which(!is.na(dataHis$meanU)),]) #1102
nrow(dataAI[which(!is.na(dataAI$meanU)),]) #850
nrow(dataW[which(!is.na(dataW$meanU)),]) #1194
nrow(dataBIPOC[which(!is.na(dataBIPOC$meanU)),]) #1168

# With selenium available:  
nrow(dataAA[which(!is.na(dataAA$meanSe)),]) #1927
nrow(dataHis[which(!is.na(dataHis$meanSe)),]) #2378
nrow(dataAI[which(!is.na(dataAI$meanSe)),]) #1686
nrow(dataW[which(!is.na(dataW$meanSe)),]) #2575
nrow(dataBIPOC[which(!is.na(dataBIPOC$meanSe)),]) #2532

# With barium available:
nrow(dataAA[which(!is.na(dataAA$meanBa)),]) #1927
nrow(dataHis[which(!is.na(dataHis$meanBa)),]) #2378
nrow(dataAI[which(!is.na(dataAI$meanBa)),]) #1686
nrow(dataW[which(!is.na(dataW$meanBa)),]) #2575
nrow(dataBIPOC[which(!is.na(dataBIPOC$meanBa)),]) #2532

## Create spline terms for each for the 4 databases (restricting to counties with at least 100 residents)
## non-Hispanic African American:
describe(dataAA$jAfrican.A)
AA.percentiles <- quantile(dataAA$jAfrican.A , probs = c(seq(0,1,0.1), 0.25, 0.75), na.rm=T)

dataAA$AA.rqs2 <- (
  pmax( dataAA$jAfrican.A - AA.percentiles["10%"] , 0 )^2 -
    pmax( dataAA$jAfrican.A - AA.percentiles["90%"] , 0 )^2)
dataAA$AA.rqs3 <- (
  pmax( dataAA$jAfrican.A - AA.percentiles["50%"] , 0 )^2 -
    pmax( dataAA$jAfrican.A - AA.percentiles["90%"] , 0 )^2 )
# log scale:
dataAA$AA.rqs2log <- (
  pmax( log(dataAA$jAfrican.A) - log(AA.percentiles["10%"]) , 0 )^2 -
    pmax( log(dataAA$jAfrican.A) - log(AA.percentiles["90%"]) , 0 )^2)
dataAA$AA.rqs3log <- (
  pmax( log(dataAA$jAfrican.A) - log(AA.percentiles["50%"]) , 0 )^2 -
    pmax( log(dataAA$jAfrican.A) - log(AA.percentiles["90%"]) , 0 )^2 )

#write CSV file to incorporate it into the polygon on QGIS
write.csv(dataAA, row.names=T, file="dataAAsplines.csv")

## Hispanic
describe(dataHis$jHispanic)
Hisp.percentiles <- quantile(dataHis$jHispanic , probs = c(seq(0,1,0.1), 0.25, 0.75), na.rm=T)

dataHis$Hisp.rqs2 <- (
  pmax( dataHis$jHispanic - Hisp.percentiles["10%"] , 0 )^2 -
    pmax( dataHis$jHispanic - Hisp.percentiles["90%"] , 0 )^2)
dataHis$Hisp.rqs3 <- (
  pmax( dataHis$jHispanic - Hisp.percentiles["50%"] , 0 )^2 -
    pmax( dataHis$jHispanic - Hisp.percentiles["90%"] , 0 )^2 )
# log scale
dataHis$Hisp.rqs2log <- (
  pmax( log(dataHis$jHispanic) - log(Hisp.percentiles["10%"]) , 0 )^2 -
    pmax( log(dataHis$jHispanic) - log(Hisp.percentiles["90%"]) , 0 )^2)
dataHis$Hisp.rqs3log <- (
  pmax( log(dataHis$jHispanic) - log(Hisp.percentiles["50%"]) , 0 )^2 -
    pmax( log(dataHis$jHispanic) - log(Hisp.percentiles["90%"]) , 0 )^2 )

#write CSV file to incorporate it into the polygon on QGIS
write.csv(dataHis, row.names=T, file="dataHISsplines.csv")

## American Indian
describe(dataAI$jAmerican)
AI.percentiles <- quantile(dataAI$jAmerican , probs = c(seq(0,1,0.1), 0.25, 0.75), na.rm=T)

dataAI$AI.rqs2log <- (
  pmax( dataAI$jAmerican - AI.percentiles["10%"] , 0 )^2 -
    pmax( dataAI$jAmerican - AI.percentiles["90%"] , 0 )^2)
dataAI$AI.rqs3log <- (
  pmax( dataAI$jAmerican - AI.percentiles["50%"] , 0 )^2 -
    pmax( dataAI$jAmerican - AI.percentiles["90%"] , 0 )^2 )
# log scale:
dataAI$AI.rqs2 <- (
  pmax( log(dataAI$jAmerican) - log(AI.percentiles["10%"]) , 0 )^2 -
    pmax( log(dataAI$jAmerican) - log(AI.percentiles["90%"]) , 0 )^2)
dataAI$AI.rqs3 <- (
  pmax( log(dataAI$jAmerican) - log(AI.percentiles["50%"]) , 0 )^2 -
    pmax( log(dataAI$jAmerican) - log(AI.percentiles["90%"]) , 0 )^2 )

#write CSV file to incorporate it into the polygon on QGIS
write.csv(dataAI, row.names=T, file="dataAIsplines.csv")

## white
describe(dataW$jWhite)
White.percentiles <- quantile(dataW$jWhite , probs = c(seq(0,1,0.1), 0.25, 0.75), na.rm=T)

dataW$White.rqs2 <- (
  pmax( dataW$jWhite - White.percentiles["10%"] , 0 )^2 -
    pmax( dataW$jWhite - White.percentiles["90%"] , 0 )^2)
dataW$White.rqs3log <- (
  pmax( dataW$jWhite - White.percentiles["50%"] , 0 )^2 -
    pmax( dataW$jWhite - White.percentiles["90%"] , 0 )^2 )
# log scale: 
dataW$White.rqs2log <- (
  pmax( log(dataW$jWhite) - log(White.percentiles["10%"]) , 0 )^2 -
    pmax( log(dataW$jWhite) - log(White.percentiles["90%"]) , 0 )^2)
dataW$White.rqs3log <- (
  pmax( log(dataW$jWhite) - log(White.percentiles["50%"]) , 0 )^2 -
    pmax( log(dataW$jWhite) - log(White.percentiles["90%"]) , 0 )^2 )

#write CSV file to incorporate it into the polygon on QGIS
write.csv(dataW, row.names=T, file="dataW2ag.csv")

## BIPOC
describe(dataBIPOC$jBIPOC)
BIPOC.percentiles <- quantile(dataBIPOC$jBIPOC , probs = c(seq(0,1,0.1), 0.25, 0.75), na.rm=T)

dataBIPOC$BIPOC.rqs2 <- (
  pmax( dataBIPOC$jBIPOC - BIPOC.percentiles["10%"] , 0 )^2 -
    pmax( dataBIPOC$jBIPOC - BIPOC.percentiles["90%"] , 0 )^2)
dataBIPOC$BIPOC.rqs3log <- (
  pmax( dataBIPOC$jBIPOC - BIPOC.percentiles["50%"] , 0 )^2 -
    pmax( dataBIPOC$jBIPOC - BIPOC.percentiles["90%"] , 0 )^2 )
# log scale: 
dataBIPOC$BIPOC.rqs2log <- (
  pmax( log(dataBIPOC$jBIPOC) - log(BIPOC.percentiles["10%"]) , 0 )^2 -
    pmax( log(dataBIPOC$jBIPOC) - log(BIPOC.percentiles["90%"]) , 0 )^2)
dataBIPOC$BIPOC.rqs3log <- (
  pmax( log(dataBIPOC$jBIPOC) - log(BIPOC.percentiles["50%"]) , 0 )^2 -
    pmax( log(dataBIPOC$jBIPOC) - log(BIPOC.percentiles["90%"]) , 0 )^2 )

#write CSV file to incorporate it into the polygon on QGIS
write.csv(dataBIPOC, row.names=T, file="dataBIPOC2ag.csv")


### end




#####----- 5. Table 1. Descriptive characteristics -----######

read.xlsx("covariates2_sept.07.xlsx")
# Overall n
nrow(data) #2631

## overall (Column 0)
t.dat<-data
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col0<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col0)[1] <- "Overall"

write.csv(col0, row.names=T, file="Col01.csv")


## non-Hispanic African American  (Column 1):
t.dat<-dataAA
nav<-nrow(t.dat)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))




col1<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool,rural, propA,propAI,propH,propW)
colnames(col1)[1] <- "AfricanAmerican"

## American Indian  (Column 2):
t.dat<-dataAI

nav<-nrow(t.dat)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col2<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool,rural, propA,propAI,propH,propW)
colnames(col2)[1] <- "AmericanIndian"


## Hispanic  (Column 3):
t.dat<-dataHis

nav<-nrow(t.dat)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col3<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool,rural,propA,propAI,propH,propW)
colnames(col3)[1] <- "Hispanic"

## white (Column 4):
t.dat<-dataW

nav<-nrow(t.dat)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col4<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col4)[1] <- "white"


## BIPOC (Column 5):
t.dat<-dataBIPOC

nav<-nrow(t.dat)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col5<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col5)[1] <- "BIPOC"



t.1<-cbind(col0,col1,col2,col3,col4,col5)

write.csv(t.1, row.names=T, file="Table1.09ag.csv")

### end

#####----- 5.5 Supplemental Table 1. Descriptive characteristics for excluded counties #####

# Excluded for missing 100 residents:
dim(excluded_miss_metals) # 429 missing CWS metals
excludedAA<-data[which(data$jNafrican<100),];dim(excludedAA) #690
table(excludedAA$State.Code)
excludedHis<-data[which(data$jNhispanic<100),];dim(excludedHis) #207
excludedAI<-data[which(data$jNindian<100),];dim(excludedAI) #922
excludedW<-data[which(data$jNwhite<100),];dim(excludedW) #0
excludedBIPOC<-data[which(data$jNBIPOC<100),];dim(excludedBIPOC) #46



# All counties excluded from analysis
t.dat<-excluded
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col0<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col0)[1] <- "Excluded"


# All counties at beginning
t.dat<-allcounties
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col1<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col1)[1] <- "All conterminuous US counties"

# Counties excluded from non-Hisp Black analysis
t.dat<-excludedAA
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col2<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col2)[1] <- "Excluded_AA"


# Counties excluded from His
t.dat<-excludedHis
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col3<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col3)[1] <- "Excluded_Hisp"



# Counties excluded from AI
t.dat<-excludedAI
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col4<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col4)[1] <- "Excluded_AI"



# Counties excluded from white
t.dat<-excludedW
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col5<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col5)[1] <- "Excluded_W"


# Counties excluded for missing CWS metals
t.dat<-excluded_miss_metals
nav<-nrow(t.dat)

As.m<-mean(t.dat$meanAs,na.rm=T);As.m<-round(As.m,digits=2)
As.SD<-sd(t.dat$meanAs,na.rm=T);As.SD<-round(As.SD,digits=2)
nAs<-nrow(t.dat[which(!is.na(t.dat$meanAs)),])
arsenic<-mean.se.N(c(As.m,As.SD,nAs))

U.m<-mean(t.dat$meanU,na.rm=T);U.m<-round(U.m,digits=2)
U.SD<-sd(t.dat$meanU,na.rm=T);U.SD<-round(U.SD,digits=2)
nU<-nrow(t.dat[which(!is.na(t.dat$meanU)),])
uranium<-mean.se.N(c(U.m,U.SD,nU))

Se.m<-mean(t.dat$meanSe,na.rm=T);Se.m<-round(Se.m,digits=2)
Se.SD<-sd(t.dat$meanSe,na.rm=T);Se.SD<-round(Se.SD,digits=2)
nSe<-nrow(t.dat[which(!is.na(t.dat$meanSe)),])
selenium<-mean.se.N(c(Se.m,Se.SD,nSe))

Ba.m<-mean(t.dat$meanBa,na.rm=T);Ba.m<-round(Ba.m,digits=2)
Ba.SD<-sd(t.dat$meanBa,na.rm=T);Ba.SD<-round(Ba.SD,digits=2)
nBa<-nrow(t.dat[which(!is.na(t.dat$meanBa)),])
barium<-mean.se.N(c(Ba.m,Ba.SD,nBa))

pop.size.m<-mean(t.dat$jPopulatio,na.rm=T);pop.size.m<-round(pop.size.m,digits=0)
pop.size.SD<-sd(t.dat$jPopulatio,na.rm=T);pop.size.SD<-round(pop.size.SD,digits=0)
pop.size<-to.mean.se(c(pop.size.m,pop.size.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

pop.density.m<-mean(t.dat$jdensity,na.rm=T);pop.density.m<-round(pop.density.m,digits=0)
pop.density.SD<-sd(t.dat$jdensity,na.rm=T);pop.density.SD<-round(pop.density.SD,digits=0)
pop.density<-to.mean.se(c(pop.density.m,pop.density.SD))

per.GW.m<-mean(t.dat$GWPerc10,na.rm=T);per.GW.m<-round(per.GW.m,digits=2)*100
per.GW.SD<-sd(t.dat$GWPerc10,na.rm=T);per.GW.SD<-round(per.GW.SD,digits=2)*100
per.GW<-to.mean.se(c(per.GW.m,per.GW.SD))

income.m<-mean(t.dat$jMedian.ho,na.rm=T);income.m<-round(income.m,digits=0)
income.SD<-sd(t.dat$jMedian.ho,na.rm=T);income.SD<-round(income.SD,digits=0)
income<-to.mean.se(c(income.m,income.SD))

highschool.m<-mean(t.dat$jHigh.scho,na.rm=T);highschool.m<-round(highschool.m,digits=2)*100
highschool.SD<-sd(t.dat$jHigh.scho,na.rm=T);highschool.SD<-round(highschool.SD,digits=2)*100
highschool<-to.mean.se(c(highschool.m,highschool.SD))

rural.m<-mean(t.dat$jRural,na.rm=T);rural.m<-round(rural.m,digits=2)*100
rural.SD<-sd(t.dat$jRural,na.rm=T);rural.SD<-round(rural.SD,digits=2)*100
rural<-to.mean.se(c(rural.m,rural.SD))

propA.m<-mean(t.dat$jAfrican.A,na.rm=T);propA.m<-round(propA.m,digits=2)*100
propA.SD<-sd(t.dat$jAfrican.A,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
propA<-to.mean.se(c(propA.m,propA.SD))

# propA.m<-mean(t.dat$CenPBlack,na.rm=T);propA.m<-round(propA.m,digits=2)*100
# propA.SD<-sd(t.dat$CenPBlack,na.rm=T);propA.SD<-round(propA.SD,digits=2)*100
# propA<-to.mean.se(c(propA.m,propA.SD)) # same estimates when using census data directly

propAI.m<-mean(t.dat$jAmerican,na.rm=T);propAI.m<-round(propAI.m,digits=2)*100
propAI.SD<-sd(t.dat$jAmerican,na.rm=T);propAI.SD<-round(propAI.SD,digits=2)*100
propAI<-to.mean.se(c(propAI.m,propAI.SD))

propH.m<-mean(t.dat$jHispanic,na.rm=T);propH.m<-round(propH.m,digits=2)*100
propH.SD<-sd(t.dat$jHispanic,na.rm=T);propH.SD<-round(propH.SD,digits=2)*100
propH<-to.mean.se(c(propH.m,propH.SD))

propW.m<-mean(t.dat$jWhite,na.rm=T);propW.m<-round(propW.m,digits=2)*100
propW.SD<-sd(t.dat$jWhite,na.rm=T);propW.SD<-round(propW.SD,digits=2)*100
propW<-to.mean.se(c(propW.m,propW.SD))

col6<-rbind(nav,arsenic,uranium,barium,selenium,pop.size,pop.density,per.GW,income,highschool, rural, propA,propAI,propH,propW)
colnames(col6)[1] <- "Excluded_CWSmetals"

t.1<-cbind(col1,col2,col3,col4,col6)

write.csv(t.1, row.names=T, file="SuppTable1.csv")


### end


#####----- 6. Create the shapefiles with the data for the restricted R/E groups ######
#install.packages("sp")
#We use the Shapefile for conterminous US counties with FIPS codes publicly available and downloaded from ESRI. https://community.esri.com/
library(raster)
#for African American
# read data    
pAA <- shapefile("county.shp")
dAA <- read.csv("dataAAsplines.csv")
# merge on common variable
mAA <- merge(pAA, dAA, by='CountyFIPS')
#save as shapefile 
shapefile(mAA, "mergedAA.shp", overwrite = TRUE)
#double check that the group of interest is restricted to >100 people
summary(mAA$jNafrican)

#for American Indian or Alaskan Native
# read data    
pAI <- shapefile("county.shp")
dAI <- read.csv("dataAIsplines.csv")
# merge on common variable
mAI <- merge(pAI, dAI, by='CountyFIPS')
#save as shapefile 
shapefile(mAI, "mergedAI.shp", overwrite = TRUE)
#double check that the group of interest is restricted to >100 people
summary(mAI$jNindian)

#for Hispanic
# read data    
pH <- shapefile("county.shp")
dH <- read.csv("dataHISsplines.csv")
# merge on common variable
mH <- merge(pH, dH, by='CountyFIPS')
#save as shapefile 
shapefile(mH, "mergedHIS.shp", overwrite = TRUE)
#double check that the group of interest is restricted to >100 people
summary(mH$jNhispanic)

#for non-Hispanic white
# read data    
pW <- shapefile("county.shp")
dW <- read.csv("dataW2ag.csv")
# merge on common variable
mW <- merge(pW, dW, by='CountyFIPS')
#save as shapefile 
shapefile(mW, "mergedW.shp", overwrite = TRUE)
#double check that the group of interest is restricted to >100 people
summary(mW$jNwhite)

#for BIPOC
# read data    
pBIPOC <- shapefile("county.shp")
dBIPOC <- read.csv("dataBIPOC2ag.csv")
# merge on common variable
mBIPOC <- merge(pBIPOC, dBIPOC, by='CountyFIPS')
#save as shapefile 
shapefile(mBIPOC, "mergedBIPOC.shp", overwrite = TRUE)
#double check that the group of interest is restricted to >100 people
summary(mBIPOC$jNBIPOC)


## 6.1 double checking GMR before running lag regression

data$IsCaliforniaOrTexas<-"No"

data$IsCaliforniaOrTexas[which(data$StateName=="California")]<-"Yes"

data$IsCaliforniaOrTexas[which(data$StateName=="Texas")]<-"Yes"

table(data$IsCaliforniaOrTexas)

## Get the GMR manually by first getting the geometric mean for both groups and dividing them

exp(mean(data$logAs[which(data$IsCaliforniaOrTexas=="No")],na.rm=T)) #0.7817461

exp(mean(data$logAs[which(data$IsCaliforniaOrTexas=="Yes")],na.rm=T)) #1.285424

1.299173 / 0.7787267 # = 1.66833

## Get the GMR from the GLM model

as.lm <- lm(logAs~IsCaliforniaOrTexas, data=data)

summary(as.lm)

exp(0.51182) # 1.668325

### end

#####----- 7. Table 2. Spatial lag models ######
###end
#####-----     7A. Set up #######

library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)
library(ggplot2)
library(openxlsx)

# Read data in #NOTE: we are using a different shapefile (created in section 6) with the restricted number of people (n>100/county)for each group.

#####-----     7B. Table 2. Per 10% higher proportion of residents ######
working <- getwd()
print("Table2")


### Part 1: Hispanic 
#Specify models
var.x <- "jHspn"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  #here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  TRI_poly.subhis <- readOGR(dsn = working, layer ="mergedHIS")
  sample.size <- "manual"
  summary(TRI_poly.subhis)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqhis <- poly2nb(TRI_poly.subhis)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqhis)
  TRI_nbq_whis <- nb2listw(TRI_nbqhis, zero.policy = TRUE)
  
  
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jAf_A + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subhis, TRI_nbq_whis, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jHspn"]
    se <- summary(fit)$rest.se["jHspn"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  

  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeHispanic <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeHispanic$racegroup<-NA
table.changeHispanic$racegroup<-"Hispanic"

### Part 2: non-Hispanic African American 
#Specify models

var.x <- "jAf_A"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subaf <- readOGR(dsn = working, layer ="mergedAA")
  summary(TRI_poly.subaf)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqaf <- poly2nb(TRI_poly.subaf)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqaf)
  TRI_nbq_waf <- nb2listw(TRI_nbqaf, zero.policy = TRUE)
  summary(TRI_poly.subaf)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAf_A"]
    se <- summary(fit)$rest.se["jAf_A"]
   # result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAA <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAA$racegroup<-NA
table.changeAA$racegroup<-"nonHispanicAfricanAmerican"

### Part 3: American Indian
#Specify models
var.x <- "jAmrc"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subin <- readOGR(dsn = working, layer ="mergedAI")
  
  sample.size <- "manual"
  summary(TRI_poly.subin)
  
  # Create a queen's neighborhood weight matrix using the poly2nb command.
  TRI_nbqin <- poly2nb(TRI_poly.subin)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqin)
  TRI_nbq_win <- nb2listw(TRI_nbqin, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAf_A + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAf_A + jAsin")
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subin, TRI_nbq_win, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAmrc"]
    se <- summary(fit)$rest.se["jAmrc"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAI <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAI$racegroup<-NA
table.changeAI$racegroup<-"AmericanIndian"


### Part 4: non-Hispanic white
#Specify models
var.x <- "jWhit"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subw <- readOGR(dsn = working, layer ="mergedW")
  sample.size <- "manual"
  summary(TRI_poly.subw)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqw <- poly2nb(TRI_poly.subw)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqw)
  TRI_nbq_ww <- nb2listw(TRI_nbqw, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jHspn + jAmrc + jAsin + jAf_A") 
  model3 <- paste( model1, " + jAf_A + jAmrc + jAsin") #leaving hispanic out
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subw, TRI_nbq_ww, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jWhit"]
    se <- summary(fit)$rest.se["jWhit"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changewhite <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changewhite$racegroup<-NA
table.changewhite$racegroup<-"nonHispanicwhite"

### Part 5: BIPOC models 
#Specify models

var.x <- "jBIPO"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subBIPOC <- readOGR(dsn = working, layer ="mergedBIPOC")
  summary(TRI_poly.subBIPOC)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqBIPOC <- poly2nb(TRI_poly.subBIPOC)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqBIPOC)
  TRI_nbq_wBIPOC <- nb2listw(TRI_nbqBIPOC, zero.policy = TRUE)
  summary(TRI_poly.subBIPOC)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jWhit") 
  model3 <- paste( model1) #same as model 1
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jBIPO"]
    se <- summary(fit)$rest.se["jBIPO"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeBP <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeBP$racegroup<-NA
table.changeBP$racegroup<-"BIPOC"

View(table.changeBP)
View(table.changeAA)
View(t.2)

# Rbind all results together 
t.2 <-rbind(table.changeHispanic,table.changeAA,table.changeAI,table.changewhite, table.changeBP)

write.xlsx(t.2, row.names=T, file="Table2.10sept.xlsx", overwrite = TRUE)

library(openxlsx)


#get Ns for previous analyses
describe(TRI_poly.subhis$logAs)
describe(TRI_poly.subhis$logU)
describe(TRI_poly.subhis$logSe)
describe(TRI_poly.subhis$logBa)

describe(TRI_poly.subaf$logAs)
describe(TRI_poly.subaf$logU)
describe(TRI_poly.subaf$logSe)
describe(TRI_poly.subaf$logBa)

describe(TRI_poly.subin$logAs)
describe(TRI_poly.subin$logU)
describe(TRI_poly.subin$logSe)
describe(TRI_poly.subin$logBa)

describe(TRI_poly.subw$logAs)
describe(TRI_poly.subw$logU)
describe(TRI_poly.subw$logSe)
describe(TRI_poly.subw$logBa)

describe(TRI_poly.subBIPOC$logAs)
describe(TRI_poly.subBIPOC$logU)
describe(TRI_poly.subBIPOC$logSe)
describe(TRI_poly.subBIPOC$logBa)


### end




#####-----       Supplemental Table 4. Model adjusting for N of whites ######
### Part 1: Hispanic 
working <- getwd()
#Specify models
var.x <- "jHspn"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  #here is where we specify which dataframe we are using.
  #NOTE: the shapefiles and weights matrix have been created in 7B, no need to run those code sections here as well.
  TRI_poly.subhis <- readOGR(dsn = working, layer ="mergedHIS")
  sample.size <- "manual"
  summary(TRI_poly.subhis)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqhis <- poly2nb(TRI_poly.subhis)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqhis)
  TRI_nbq_whis <- nb2listw(TRI_nbqhis, zero.policy = TRUE)
  
  
  model1 <- "+ GWP10 + jdnst" 
  model2 <- paste( model1, "+ jHgh_ +jMdn_") 
  model3 <- paste( model2, "+ jNwht")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subhis, TRI_nbq_whis, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jHspn"]
    se <- summary(fit)$rest.se["jHspn"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeHispanicN <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeHispanicN$racegroup<-NA
table.changeHispanicN$racegroup<-"Hispanic"

### Part 2: non-Hispanic African American 
#Specify models

var.x <- "jAf_A"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  sample.size <- "manual"
  TRI_poly.subaf <- readOGR(dsn = working, layer ="mergedAA")
  summary(TRI_poly.subaf)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqaf <- poly2nb(TRI_poly.subaf)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqaf)
  TRI_nbq_waf <- nb2listw(TRI_nbqaf, zero.policy = TRUE)
  summary(TRI_poly.subaf)
  
  model1 <- "+ GWP10 + jdnst" 
  model2 <- paste( model1, " + jHgh_ +jMdn_") 
  model3 <- paste( model1, " + jNwht")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAf_A"]
    se <- summary(fit)$rest.se["jAf_A"]
    # result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAAN <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAAN$racegroup<-NA
table.changeAAN$racegroup<-"nonHispanicAfricanAmericanN"

### Part 3: American Indian
#Specify models
var.x <- "jAmrc"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subin <- readOGR(dsn = working, layer ="mergedAI")
  sample.size <- "manual"
  #summary(TRI_poly.subin)
  
  # Create a queen's neighborhood weight matrix using the poly2nb command.
  TRI_nbqin <- poly2nb(TRI_poly.subin)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqin)
  TRI_nbq_win <- nb2listw(TRI_nbqin, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst" 
  model2 <- paste( model1, "+ jHgh_ +jMdn_") 
  model3 <- paste( model2, "+ jNwht")
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subin, TRI_nbq_win, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAmrc"]
    se <- summary(fit)$rest.se["jAmrc"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAIN <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAIN$racegroup<-NA
table.changeAIN$racegroup<-"AmericanIndianN"


### Part 4: non-Hispanic white
#Specify models
var.x <- "jWhit"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subw <- readOGR(dsn = working, layer ="mergedW")
  sample.size <- "manual"
  summary(TRI_poly.subw)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqw <- poly2nb(TRI_poly.subw)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqw)
  TRI_nbq_ww <- nb2listw(TRI_nbqw, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst" 
  model2 <- paste( model1, "+ jHgh_ +jMdn_") 
  model3 <- paste( model2, "+ jNwht") 
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subw, TRI_nbq_ww, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jWhit"]
    se <- summary(fit)$rest.se["jWhit"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changewhiteN <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changewhiteN$racegroup<-NA
table.changewhiteN$racegroup<-"nonHispanicwhiteN"

### Part 5: BIPOC models 
#Specify models

var.x <- "jBIPO"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  sample.size <- "manual"
  TRI_poly.subBIPOC <- readOGR(dsn = working, layer ="mergedBIPOC")
  summary(TRI_poly.subBIPOC)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
TRI_nbqBIPOC <- poly2nb(TRI_poly.subBIPOC)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqBIPOC)
  TRI_nbq_wBIPOC <- nb2listw(TRI_nbqBIPOC, zero.policy = TRUE)
  summary(TRI_poly.subBIPOC)
  
  model1 <- "+ GWP10 + jdnst" 
  model2 <- paste( model1, "+ jHgh_ +jMdn_") 
  model3 <- paste( model2, "+ jNwht") 
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jBIPO"]
    se <- summary(fit)$rest.se["jBIPO"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.1 (10%) increase 
    result.p75.25 <- (0.1) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeBPN <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeBPN$racegroup<-NA
table.changeBPN$racegroup<-"BIPOCN"


# Rbind all results together 
t.3 <-rbind(table.changeHispanicN,table.changeAAN,table.changeAIN,table.changewhiteN, table.changeBPN)

write.xlsx(t.3, row.names=T, file="TableNNwhite.xlsx", overwrite = TRUE)





### end

#####-----       Supplemental Table 4. Western States #######
#Washington, Oregon, Idaho, Montana, Wyoming, North Dakota, South Dakota, Nebraska, Kansas, Oklahoma,
# Texas, New Mexico, Colorado, Utah, Arizona, Nevada, and California

#install.packages("rgeos")
library(rgeos)
working <- getwd()
print("TableW")


### Part 1: Hispanic 
#Specify models
var.x <- "jHspn"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ## ANNIE NOTE TO IRENE: here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  # Also, this code below to determine the total N does not work, I do not know yet how to treat these large spatial polygons!
  
  TRI_poly.subhisWest <- subset(TRI_poly.subhis, STATEFP_x != "01" & STATEFP_x != "05" & STATEFP_x != "09" & STATEFP_x != "10" & STATEFP_x != "11"& STATEFP_x != "12" & STATEFP_x != "13" & STATEFP_x != "17" & STATEFP_x != "18" & STATEFP_x != "19" & STATEFP_x != "21" & STATEFP_x != "22"& STATEFP_x != "23" & STATEFP_x != "24" & STATEFP_x != "25" & STATEFP_x != "26" & STATEFP_x != "27" & STATEFP_x != "28" & STATEFP_x != "29" & STATEFP_x != "33"& STATEFP_x != "34"& STATEFP_x != "36"& STATEFP_x != "37"& STATEFP_x != "39"& STATEFP_x != "42"& STATEFP_x != "44"& STATEFP_x != "45"& STATEFP_x != "47"& STATEFP_x != "50"& STATEFP_x != "51"& STATEFP_x !="54"& STATEFP_x != "55" )
  
  #NOTE from IRENE: when I run this line I get some weird estimates. TRI_poly.subhis <- TRI_poly.subhis[which(!is.na(TRI_poly.subhis[ ,var.y])),]  ; dim(TRI_poly.subhis)
  sample.size <- "manual"
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqhisW <- poly2nb(TRI_poly.subhisWest)
  # Convert the neighborhood matrix into a list 
  TRI_nbq_whisW <- nb2listw(TRI_nbqhisW, zero.policy = TRUE)
  
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jAf_A + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subhisWest, TRI_nbq_whisW, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jHspn"]
    se <- summary(fit)$rest.se["jHspn"]
    result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeHispanic <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeHispanic$racegroup<-NA
table.changeHispanic$racegroup<-"Hispanic"

### Part 2: non-Hispanic African American 
#Specify models

var.x <- "jAf_A"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ## ANNIE NOTE TO IRENE: here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  # Also, this code below to determine the total N does not work, I do not know yet how to treat these large spatial polygons!
  TRI_poly.subAAWest <- subset(TRI_poly.subaf, STATEFP_x != "01" & STATEFP_x != "05" & STATEFP_x != "09" & STATEFP_x != "10" & STATEFP_x != "11"& STATEFP_x != "12" & STATEFP_x != "13" & STATEFP_x != "17" & STATEFP_x != "18" & STATEFP_x != "19" & STATEFP_x != "21" & STATEFP_x != "22"& STATEFP_x != "23" & STATEFP_x != "24" & STATEFP_x != "25" & STATEFP_x != "26" & STATEFP_x != "27" & STATEFP_x != "28" & STATEFP_x != "29" & STATEFP_x != "33"& STATEFP_x != "34"& STATEFP_x != "36"& STATEFP_x != "37"& STATEFP_x != "39"& STATEFP_x != "42"& STATEFP_x != "44"& STATEFP_x != "45"& STATEFP_x != "47"& STATEFP_x != "50"& STATEFP_x != "51"& STATEFP_x !="54"& STATEFP_x != "55" )
  ##NOTE from IRENE: when I run this line I get some weird estimates. TRI_poly.subaf <- TRI_poly.subaf[which(!is.na(TRI_poly.subaf[ ,var.y])),]  ; dim(TRI_poly.subaf)
  sample.size <- "manual"
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqafW <- poly2nb(TRI_poly.subAAWest)
  # Convert the neighborhood matrix into a list 
  TRI_nbq_wafW <- nb2listw(TRI_nbqafW, zero.policy = TRUE)
  
  summary(TRI_poly.subaf)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subAAWest, TRI_nbq_wafW, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAf_A"]
    se <- summary(fit)$rest.se["jAf_A"]
    result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAA <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAA$racegroup<-NA
table.changeAA$racegroup<-"nonHispanicAfricanAmerican"

### Part 3: American Indian
#Specify models
var.x <- "jAmrc"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ## ANNIE NOTE TO IRENE: here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  # Also, this code below to determine the total N does not work, I do not know yet how to treat these large spatial polygons!
  TRI_poly.subinW <- subset(TRI_poly.subin, STATEFP_x != "01" & STATEFP_x != "05" & STATEFP_x != "09" & STATEFP_x != "10" & STATEFP_x != "11"& STATEFP_x != "12" & STATEFP_x != "13" & STATEFP_x != "17" & STATEFP_x != "18" & STATEFP_x != "19" & STATEFP_x != "21" & STATEFP_x != "22"& STATEFP_x != "23" & STATEFP_x != "24" & STATEFP_x != "25" & STATEFP_x != "26" & STATEFP_x != "27" & STATEFP_x != "28" & STATEFP_x != "29" & STATEFP_x != "33"& STATEFP_x != "34"& STATEFP_x != "36"& STATEFP_x != "37"& STATEFP_x != "39"& STATEFP_x != "42"& STATEFP_x != "44"& STATEFP_x != "45"& STATEFP_x != "47"& STATEFP_x != "50"& STATEFP_x != "51"& STATEFP_x !="54"& STATEFP_x != "55" )
  #NOTE FROM IRENE: when I run this line the variable logU, meanU and 95U is read as NA. TRI_poly.subin <- TRI_poly.subin[which(!is.na(TRI_poly.subin[ ,var.y])),]  ; dim(TRI_poly.subin)
  sample.size <- "manual"
  # Create a queen's neighborhood weight matrix using the poly2nb command.
  TRI_nbqinW <- poly2nb(TRI_poly.subinW)
  # Convert the neighborhood matrix into a list 
  TRI_nbq_winW <- nb2listw(TRI_nbqinW, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jHspn + jAsin + jWhit") 
  model3 <- paste( model1, "+ jAf_A + jHspn + jAsin")
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subinW, TRI_nbq_winW, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAmrc"]
    se <- summary(fit)$rest.se["jAmrc"]
    result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAI <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAI$racegroup<-NA
table.changeAI$racegroup<-"AmericanIndian"


### Part 4: non-Hispanic white
#Specify models
var.x <- "jWhit"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ## ANNIE NOTE TO IRENE: here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  # Also, this code below to determine the total N does not work, I do not know yet how to treat these large spatial polygons!
  TRI_poly.subwW <- subset(TRI_poly.subw, STATEFP_x != "01" & STATEFP_x != "05" & STATEFP_x != "09" & STATEFP_x != "10" & STATEFP_x != "11"& STATEFP_x != "12" & STATEFP_x != "13" & STATEFP_x != "17" & STATEFP_x != "18" & STATEFP_x != "19" & STATEFP_x != "21" & STATEFP_x != "22"& STATEFP_x != "23" & STATEFP_x != "24" & STATEFP_x != "25" & STATEFP_x != "26" & STATEFP_x != "27" & STATEFP_x != "28" & STATEFP_x != "29" & STATEFP_x != "33"& STATEFP_x != "34"& STATEFP_x != "36"& STATEFP_x != "37"& STATEFP_x != "39"& STATEFP_x != "42"& STATEFP_x != "44"& STATEFP_x != "45"& STATEFP_x != "47"& STATEFP_x != "50"& STATEFP_x != "51"& STATEFP_x !="54"& STATEFP_x != "55" )
  #NOTE from IRENE: when I run this line I get some weird estimates. TRI_poly.subw <- TRI_poly.subw[which(!is.na(TRI_poly.subw[ ,var.y])),]  ; dim(TRI_poly.subw)
  sample.size <- "manual"
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqwW <- poly2nb(TRI_poly.subwW)
  # Convert the neighborhood matrix into a list 
  TRI_nbq_wwW <- nb2listw(TRI_nbqwW, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jAmrc + jAsin ") 
  model3 <- paste( model1)
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subwW, TRI_nbq_wwW, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jWhit"]
    se <- summary(fit)$rest.se["jWhit"]
    result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changewhite <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changewhite$racegroup<-NA
table.changewhite$racegroup<-"nonHispanicwhite"

### Part 5: BIPOC 
#Specify models
var.x <- "jBIPO"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ## ANNIE NOTE TO IRENE: here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  # Also, this code below to determine the total N does not work, I do not know yet how to treat these large spatial polygons!
  
  TRI_poly.subBIPOCW <- subset(TRI_poly.subBIPOC, STATEFP_x != "01" & STATEFP_x != "05" & STATEFP_x != "09" & STATEFP_x != "10" & STATEFP_x != "11"& STATEFP_x != "12" & STATEFP_x != "13" & STATEFP_x != "17" & STATEFP_x != "18" & STATEFP_x != "19" & STATEFP_x != "21" & STATEFP_x != "22"& STATEFP_x != "23" & STATEFP_x != "24" & STATEFP_x != "25" & STATEFP_x != "26" & STATEFP_x != "27" & STATEFP_x != "28" & STATEFP_x != "29" & STATEFP_x != "33"& STATEFP_x != "34"& STATEFP_x != "36"& STATEFP_x != "37"& STATEFP_x != "39"& STATEFP_x != "42"& STATEFP_x != "44"& STATEFP_x != "45"& STATEFP_x != "47"& STATEFP_x != "50"& STATEFP_x != "51"& STATEFP_x !="54"& STATEFP_x != "55" )
  
  #NOTE from IRENE: when I run this line I get some weird estimates. TRI_poly.subhis <- TRI_poly.subhis[which(!is.na(TRI_poly.subhis[ ,var.y])),]  ; dim(TRI_poly.subhis)
  sample.size <- "manual"
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqBIPOCW <- poly2nb(TRI_poly.subBIPOCW)
  # Convert the neighborhood matrix into a list 
  TRI_nbq_wBIPOCW <- nb2listw(TRI_nbqBIPOCW, zero.policy = TRUE)
  
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jWhit") 
  model3 <- paste( model1 )
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subBIPOCW, TRI_nbq_wBIPOCW, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jBIPO"]
    se <- summary(fit)$rest.se["jBIPO"]
    result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeBIPOC <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeBIPOC$racegroup<-NA
table.changeBIPOC$racegroup<-"BIPOC"



# Rbind all results together 
t.2 <-rbind(table.changeHispanic,table.changeAA,table.changeAI,table.changewhite,table.changeBIPOC)

write.csv(t.2, row.names=T, file="SuppTable4.West.csv")


#get the Ns for these analyses

describe(TRI_poly.subhisWest$logU)
describe(TRI_poly.subhisWest$logAs)
describe(TRI_poly.subhisWest$logSe)
describe(TRI_poly.subhisWest$logBa)

describe(TRI_poly.subAAWest$logAs)
describe(TRI_poly.subAAWest$logU)
describe(TRI_poly.subAAWest$logSe)
describe(TRI_poly.subAAWest$logBa)

describe(TRI_poly.subinW$logAs)
describe(TRI_poly.subinW$logU)
describe(TRI_poly.subinW$logSe)
describe(TRI_poly.subinW$logBa)


describe(TRI_poly.subwW$logAs)
describe(TRI_poly.subwW$logU)
describe(TRI_poly.subwW$logSe)
describe(TRI_poly.subwW$logBa)

describe(TRI_poly.subBIPOCW$logAs)
describe(TRI_poly.subBIPOCW$logU)
describe(TRI_poly.subBIPOCW$logSe)
describe(TRI_poly.subBIPOCW$logBa)

### end

#####-----       Supplemental Table 4. Per 60% increase (0.6) in proportion ######
working <- getwd()
print("Table2")


### Part 1: Hispanic 
#Specify models
var.x <- "jHspn"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  #here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  TRI_poly.subhis <- readOGR(dsn = working, layer ="mergedHIS")
  sample.size <- "manual"
  summary(TRI_poly.subhis)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqhis <- poly2nb(TRI_poly.subhis)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqhis)
  TRI_nbq_whis <- nb2listw(TRI_nbqhis, zero.policy = TRUE)
  
  
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jAf_A + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subhis, TRI_nbq_whis, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jHspn"]
    se <- summary(fit)$rest.se["jHspn"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.6 (10%) increase 
    result.p75.25 <- (0.6) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeHispanic <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeHispanic$racegroup<-NA
table.changeHispanic$racegroup<-"Hispanic"

### Part 2: non-Hispanic African American 
#Specify models

var.x <- "jAf_A"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subaf <- readOGR(dsn = working, layer ="mergedAA")
  summary(TRI_poly.subaf)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqaf <- poly2nb(TRI_poly.subaf)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqaf)
  TRI_nbq_waf <- nb2listw(TRI_nbqaf, zero.policy = TRUE)
  summary(TRI_poly.subaf)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAf_A"]
    se <- summary(fit)$rest.se["jAf_A"]
    # result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.6 (10%) increase 
    result.p75.25 <- (0.6) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAA <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAA$racegroup<-NA
table.changeAA$racegroup<-"nonHispanicAfricanAmerican"

### Part 3: American Indian
#Specify models
var.x <- "jAmrc"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subin <- readOGR(dsn = working, layer ="mergedAI")
  
  sample.size <- "manual"
  summary(TRI_poly.subin)
  
  # Create a queen's neighborhood weight matrix using the poly2nb command.
  TRI_nbqin <- poly2nb(TRI_poly.subin)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqin)
  TRI_nbq_win <- nb2listw(TRI_nbqin, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAf_A + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAf_A + jAsin")
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subin, TRI_nbq_win, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAmrc"]
    se <- summary(fit)$rest.se["jAmrc"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.6 (10%) increase 
    result.p75.25 <- (0.6) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAI <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAI$racegroup<-NA
table.changeAI$racegroup<-"AmericanIndian"


### Part 4: non-Hispanic white
#Specify models
var.x <- "jWhit"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subw <- readOGR(dsn = working, layer ="mergedW")
  sample.size <- "manual"
  summary(TRI_poly.subw)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqw <- poly2nb(TRI_poly.subw)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqw)
  TRI_nbq_ww <- nb2listw(TRI_nbqw, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jHspn + jAmrc + jAsin + jAf_A") 
  model3 <- paste( model1, " + jAf_A + jAmrc + jAsin") #leaving hispanic out
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subw, TRI_nbq_ww, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jWhit"]
    se <- summary(fit)$rest.se["jWhit"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.6 (10%) increase 
    result.p75.25 <- (0.6) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changewhite <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changewhite$racegroup<-NA
table.changewhite$racegroup<-"nonHispanicwhite"

### Part 5: BIPOC models 
#Specify models

var.x <- "jBIPO"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subBIPOC <- readOGR(dsn = working, layer ="mergedBIPOC")
  summary(TRI_poly.subBIPOC)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqBIPOC <- poly2nb(TRI_poly.subBIPOC)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqBIPOC)
  TRI_nbq_wBIPOC <- nb2listw(TRI_nbqBIPOC, zero.policy = TRUE)
  summary(TRI_poly.subBIPOC)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jWhit") 
  model3 <- paste( model1) #same as model 1
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jBIPO"]
    se <- summary(fit)$rest.se["jBIPO"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    # per 0.6 (10%) increase 
    result.p75.25 <- (0.6) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeBP <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeBP$racegroup<-NA
table.changeBP$racegroup<-"BIPOC"

View(table.changeBP)
View(table.changeAA)
View(t.2)

# Rbind all results together 
t.2 <-rbind(table.changeHispanic,table.changeAA,table.changeAI,table.changewhite, table.changeBP)

write.xlsx(t.2, row.names=T, file="SuppTable4.Per60.xlsx", overwrite = TRUE)



 ### end








#####-----       Supplemental Table 4. Per IQR increase in proportion ######
print("Table2")

### Part 1: Hispanic 
#Specify models
p75<-quantile(TRI_poly.subhis$jHspn, probs=c(0.75),na.rm=T)
p25<-quantile(TRI_poly.subhis$jHspn, probs=c(0.25),na.rm=T)
p75
p25
p75-p25 #0.079

#Specify models
var.x <- "jHspn"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  #here is where we specify which dataframe we are using.
  # We can specify a separate one for each racial/ethnic model to make sure it is the dataframe with >=100 for each
  TRI_poly.subhis <- readOGR(dsn = working, layer ="mergedHIS")
  sample.size <- "manual"
  summary(TRI_poly.subhis)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqhis <- poly2nb(TRI_poly.subhis)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqhis)
  TRI_nbq_whis <- nb2listw(TRI_nbqhis, zero.policy = TRUE)
  
  
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jAf_A + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jAf_A + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subhis, TRI_nbq_whis, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jHspn"]
    se <- summary(fit)$rest.se["jHspn"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    result.p75.25 <- (p75-p25) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeHispanic <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeHispanic$racegroup<-NA
table.changeHispanic$racegroup<-"Hispanic"




### Part 2: non-Hispanic African American 
#Specify models
p75<-quantile(TRI_poly.subaf$jAf_A, probs=c(0.75),na.rm=T)
p25<-quantile(TRI_poly.subaf$jAf_A, probs=c(0.25),na.rm=T)
p75
p25
p75-p25 #0.079

var.x <- "jAf_A"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subaf <- readOGR(dsn = working, layer ="mergedAA")
  summary(TRI_poly.subaf)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqaf <- poly2nb(TRI_poly.subaf)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqaf)
  TRI_nbq_waf <- nb2listw(TRI_nbqaf, zero.policy = TRUE)
  summary(TRI_poly.subaf)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAmrc + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAmrc + jAsin")
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAf_A"]
    se <- summary(fit)$rest.se["jAf_A"]
    # result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    result.p75.25 <- (p75-p25) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAA <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAA$racegroup<-NA
table.changeAA$racegroup<-"nonHispanicAfricanAmerican"

### Part 3: American Indian
#Specify models
p75<-quantile(TRI_poly.subin$jAmrc, probs=c(0.75),na.rm=T)
p25<-quantile(TRI_poly.subin$jAmrc, probs=c(0.25),na.rm=T)
p75
p25
p75-p25  
var.x <- "jAmrc"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subin <- readOGR(dsn = working, layer ="mergedAI")
  
  sample.size <- "manual"
  summary(TRI_poly.subin)
  
  # Create a queen's neighborhood weight matrix using the poly2nb command.
  TRI_nbqin <- poly2nb(TRI_poly.subin)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqin)
  TRI_nbq_win <- nb2listw(TRI_nbqin, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, "+ jHspn + jAf_A + jAsin + jWhit") 
  model3 <- paste( model1, "+ jHspn + jAf_A + jAsin")
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subin, TRI_nbq_win, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jAmrc"]
    se <- summary(fit)$rest.se["jAmrc"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    result.p75.25 <- (p75-p25) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeAI <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeAI$racegroup<-NA
table.changeAI$racegroup<-"AmericanIndian"


### Part 4: non-Hispanic white
#Specify models
p75<-quantile(TRI_poly.subw$jWhit, probs=c(0.75),na.rm=T)
p25<-quantile(TRI_poly.subw$jWhit, probs=c(0.25),na.rm=T)
p75
p25
p75-p25  
var.x <- "jWhit"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##here is where we specify which dataframe we are using.
  TRI_poly.subw <- readOGR(dsn = working, layer ="mergedW")
  sample.size <- "manual"
  summary(TRI_poly.subw)
  # Create a queen's neighborhood weight matrix using the poly2nb command, we will use it in the lag regression.
  TRI_nbqw <- poly2nb(TRI_poly.subw)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqw)
  TRI_nbq_ww <- nb2listw(TRI_nbqw, zero.policy = TRUE)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jHspn + jAmrc + jAsin + jAf_A") 
  model3 <- paste( model1, " + jAf_A + jAmrc + jAsin") #leaving hispanic out
  
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subw, TRI_nbq_ww, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jWhit"]
    se <- summary(fit)$rest.se["jWhit"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    result.p75.25 <- (p75-p25) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changewhite <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changewhite$racegroup<-NA
table.changewhite$racegroup<-"nonHispanicwhite"

### Part 5: BIPOC models 
#Specify models
p75<-quantile(TRI_poly.subBIPOC$jBIPO, probs=c(0.75),na.rm=T)
 p25<-quantile(TRI_poly.subBIPOC$jBIPO, probs=c(0.25),na.rm=T)
p75
p25
p75-p25  
var.x <- "jBIPO"

table.change <- character()

outcomes.change<-c("logAs", "logU", "logBa","logSe")

for (var.y in outcomes.change){
  
  print(var.y)
  
  #------------------------#
  #   Complete data set    #
  #------------------------#
  
  continuous.table <- character()
  result.all <- character()
  
  ##  here is where we specify which dataframe we are using.
  TRI_poly.subBIPOC <- readOGR(dsn = working, layer ="mergedBIPOC")
  summary(TRI_poly.subBIPOC)
  # Create a queen's neighborhood weight matrix using the poly2nb comman, we will use it in the lag regression.
  TRI_nbqBIPOC <- poly2nb(TRI_poly.subBIPOC)
  # Convert the neighborhood matrix into a list 
  summary(TRI_nbqBIPOC)
  TRI_nbq_wBIPOC <- nb2listw(TRI_nbqBIPOC, zero.policy = TRUE)
  summary(TRI_poly.subBIPOC)
  
  model1 <- "+ GWP10 + jdnst + jHgh_ +jMdn_" 
  model2 <- paste( model1, " + jWhit") 
  model3 <- paste( model1) #same as model 1
  
  for( loop.model in c(1:3) ) {
    
    # Get each adjusting model
    model <- get( paste0("model", loop.model) )
    
    # Define the variables  
    variables <- paste0(var.y, " ~ " ,  var.x, model)
    fit <- spatialreg::lagsarlm(variables, data=TRI_poly.subaf, TRI_nbq_waf, tol.solve = 1.0e-15, zero.policy = TRUE)
    beta <- summary(fit)$coefficients["jBIPO"]
    se <- summary(fit)$rest.se["jBIPO"]
    #result <- exp(c(beta, beta-1.96*se, beta+1.96*se))
    
    result.p75.25 <- (p75-p25) * c( beta , beta - 1.96 * se , beta + 1.96 * se )
    result.p75.25 <- round(exp( result.p75.25 ),2)
    result <- result.p75.25
    
    gmr.cont <- paste0(
      sprintf( "%.2f" , result[ 1 ] ) , " (" ,
      sprintf( "%.2f" , result[ 2 ] ) , ", " ,
      sprintf( "%.2f" , result[ 3 ] ) , ")" )
    
    result.model <- c( gmr.cont  ) 
    result.all <- cbind( result.all , result.model ) 
  }
  
  result.all <- cbind( sample.size, result.all  ) 
  
  #------------------------#
  #   Put all together     #
  #------------------------#
  
  result.cross.var <- cbind(casefold(var.y, upper = T), result.all)
  table.change <- rbind(table.change, result.cross.var)
  
}

table.change <- rbind(c("Variable and Model", rep(c("N", "Model 1","Model 2", "Model 3"),1)), table.change)
table.changeBP <- as.data.frame(table.change, row.names = seq(1, nrow(table.change)))
table.changeBP$racegroup<-NA
table.changeBP$racegroup<-"BIPOC"

View(table.changeBP)
View(table.changeAA)
View(t.2)

# Rbind all results together 
t.2 <-rbind(table.changeHispanic,table.changeAA,table.changeAI,table.changewhite, table.changeBP)

write.xlsx(t.2, row.names=T, file="SuppTable4IQRsept.xlsx", overwrite = TRUE)

### end

#####-----     7H. Original, + splines ########



# 6 GWR
#GWR does not run with any NAs, we need to use different shapefiles for every racial group and every metal (created previously in QGIS)

library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)
library(spgwr)


working <- getwd()
#Hispanic and As

GWR_As_hisp <- readOGR(dsn = working, layer ="hispanic_As_GWR") 
summary(GWR_As_hisp) #double check that only As fields are included, total 2385 observations
#create the bandwidth
bwG <- gwr.sel(logAs ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
               data= GWR_As_hisp, 
               gweight=gwr.Gauss, 
               verbose=TRUE, 
               adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HA = gwr(logAs ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_As_hisp,
             adapt=bwG,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.HA

names (gwr.HA$SDF)

writeOGR(gwr.HA$SDF, 
         dsn = working, 
         layer= "hispanic_As_fullGWR",
         driver="ESRI Shapefile")

#then in QGIS, we create the t estimates dividing the betas for the main r/e group by the SE


#below the model with the plot of the t statistic in R

#t = gwr.model$SDF$jHispanic / gwr.model$SDF$jHispanic_se
#sig.map = SpatialPointsDataFrame(GWR_As_hisp, data.frame(t))
#colours=c("green","red","green")
#breaks=c(min(t),-1.96,1.96,max(t))
#spplot(sig.map, cuts=breaks, col.regions=colours, cex=c(0.3,1,0.3))

#Hispanic and U
GWR_U_hisp <- readOGR(dsn = working, layer ="hispanic_u_GWR1") 
summary(GWR_U_hisp)
#create the bandwidth
bwG1 <- gwr.sel(logU ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_U_hisp, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HU = gwr(logU ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_U_hisp,
             adapt=bwG1,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.HU

names (gwr.HU$SDF)

writeOGR(gwr.HU$SDF, 
         dsn = working, 
         layer= "hispanic_U_fullGWR23ag",
         driver="ESRI Shapefile")

#Hispanic and Se
GWR_se_hisp <- readOGR(dsn = working, layer ="hipanic_se_GWR") 
#create the bandwidth
bwG2 <- gwr.sel(logSe ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_se_hisp, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HSE = gwr(logSe ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_se_hisp,
              adapt=bwG2,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.HSE

names (gwr.HSE$SDF)

writeOGR(gwr.HSE$SDF, 
         dsn = working, 
         layer= "hispanic_se_fullGWR",
         driver="ESRI Shapefile")

#Hispanic and Barium
GWR_ba_hisp <- readOGR(dsn = working, layer ="hispanic_ba_GWR") 
#create the bandwidth
bwG3 <- gwr.sel(logBa ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_ba_hisp, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HBA = gwr(logBa ~ jHispanic + jAfrican.A + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_ba_hisp,
              adapt=bwG3,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.HBA

names (gwr.HBA$SDF)

writeOGR(gwr.HBA$SDF, 
         dsn = working, 
         layer= "hispanic_ba_fullGWR",
         driver="ESRI Shapefile")

#African American and Arsenic
GWR_as_AA <- readOGR(dsn = working, layer ="african_as_GWR") 
#create the bandwidth
bwG4 <- gwr.sel(logAs ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_as_AA, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AAS = gwr(logAs ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_as_AA,
              adapt=bwG4,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.AAS

names (gwr.AAS$SDF)

writeOGR(gwr.AAS$SDF, 
         dsn = working, 
         layer= "AA_as_fullGWR",
         driver="ESRI Shapefile")

#African American and Uranium

GWR_u_AA <- readOGR(dsn = working, layer ="african_u_GWR") 
#create the bandwidth
bwG5 <- gwr.sel(logU ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_u_AA, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AU = gwr(logU ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_u_AA,
             adapt=bwG5,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.AU

names (gwr.AU$SDF)

writeOGR(gwr.AU$SDF, 
         dsn = working, 
         layer= "AA_u_fullGWR",
         driver="ESRI Shapefile")

#African American and Selenium

GWR_se_AA <- readOGR(dsn = working, layer ="african_se_GWR") 
#create the bandwidth
bwG6 <- gwr.sel(logSe ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_se_AA, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.ASE = gwr(logSe ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_se_AA,
              adapt=bwG6,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.ASE

names (gwr.AU$SDF)

writeOGR(gwr.ASE$SDF, 
         dsn = working, 
         layer= "AA_se_fullGWR",
         driver="ESRI Shapefile")

#African American and Barium

GWR_ba_AA <- readOGR(dsn = working, layer ="african_ba_GWR") 
#create the bandwidth
bwG7 <- gwr.sel(logBa ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_ba_AA, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.ABA = gwr(logBa ~ jAfrican.A + jHispanic + jAmerican + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_ba_AA,
              adapt=bwG7,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.ABA

names (gwr.ABA$SDF)

writeOGR(gwr.ABA$SDF, 
         dsn = working, 
         layer= "AA_ba_fullGWR",
         driver="ESRI Shapefile")


#Native American and Arsenic

GWR_as_N <- readOGR(dsn = working, layer ="native_as_GWR") 
#create the bandwidth
bwG8 <- gwr.sel(logAs ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_as_N, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.NAS = gwr(logAs ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_as_N,
              adapt=bwG8,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.NAS

names (gwr.NAS$SDF)

writeOGR(gwr.NAS$SDF, 
         dsn = working, 
         layer= "NA_as_fullGWR",
         driver="ESRI Shapefile")

#Native American and Uranium

GWR_u_N <- readOGR(dsn = working, layer ="native_u_GWR") 
#create the bandwidth
bwG9 <- gwr.sel(logU ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                data= GWR_u_N, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.NU = gwr(logU ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_u_N,
             adapt=bwG9,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.NU

names (gwr.NU$SDF)

writeOGR(gwr.NU$SDF, 
         dsn = working, 
         layer= "NA_u_fullGWR",
         driver="ESRI Shapefile")

#Native American and Selenium

GWR_se_N <- readOGR(dsn = working, layer ="native_se_GWR") 
#create the bandwidth
bwG10 <- gwr.sel(logSe ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_se_N, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.NSE = gwr(logSe ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_se_N,
              adapt=bwG10,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.NSE

names (gwr.NSE$SDF)

writeOGR(gwr.NSE$SDF, 
         dsn = working, 
         layer= "NA_se_fullGWR",
         driver="ESRI Shapefile")

#Native American and Barium

GWR_ba_N <- readOGR(dsn = working, layer ="native_ba_GWR") 
#create the bandwidth
bwG11 <- gwr.sel(logBa ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_ba_N, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.NBA = gwr(logBa ~ jAmerican + jAfrican.A + jHispanic + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_ba_N,
              adapt=bwG11,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.NBA

names (gwr.NBA$SDF)

writeOGR(gwr.NBA$SDF, 
         dsn = working, 
         layer= "NA_ba_fullGWR",
         driver="ESRI Shapefile")

#Non Hispanic White and Arsenic

GWR_as_W <- readOGR(dsn = working, layer ="white_as_GWR") 
#create the bandwidth
bwG12 <- gwr.sel(logAs ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_as_W, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.WA = gwr(logAs ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_as_W,
             adapt=bwG12,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.WA

names (gwr.WA$SDF)

writeOGR(gwr.WA$SDF, 
         dsn = working, 
         layer= "W_as_fullGWR",
         driver="ESRI Shapefile")

#Non Hispanic White and uranium

GWR_u_W <- readOGR(dsn = working, layer ="W_u_GWR") 
#create the bandwidth
bwG13 <- gwr.sel(logU ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_u_W, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.WU = gwr(logU ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
             data = GWR_u_W,
             adapt=bwG13,
             hatmatrix=TRUE,
             se.fit=TRUE)
gwr.WU

names (gwr.WU$SDF)

writeOGR(gwr.WU$SDF, 
         dsn = working, 
         layer= "W_u_fullGWR23ag",
         driver="ESRI Shapefile")


#Non Hispanic White and Selenium

GWR_se_W <- readOGR(dsn = working, layer ="white_se_GWR") 
#create the bandwidth
bwG14 <- gwr.sel(logSe ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_se_W, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.WSE = gwr(logSe ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_se_W,
              adapt=bwG14,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.WSE

names (gwr.WSE$SDF)

writeOGR(gwr.WSE$SDF, 
         dsn = working, 
         layer= "W_se_fullGWR",
         driver="ESRI Shapefile")

#Non Hispanic White and Barium

GWR_ba_W <- readOGR(dsn = working, layer ="white_ba_GWR") 
#create the bandwidth
bwG15 <- gwr.sel(logBa ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho, 
                 data= GWR_ba_W, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.WBA = gwr(logBa ~ jWhite + jAmerican + jAfrican.A + GWPerc10 + jHigh.scho + jdensity + jMedian.ho,
              data = GWR_ba_W,
              adapt=bwG15,
              hatmatrix=TRUE,
              se.fit=TRUE)
gwr.WBA

names (gwr.WBA$SDF)

writeOGR(gwr.WBA$SDF, 
         dsn = working, 
         layer= "W_ba_fullGWR",
         driver="ESRI Shapefile")

#end


####SUPPLEMENTAL TABLE 2####
library(tibble)
library(olsrr)
library(AICcmodavg)
# Test baseline OLS models and OLS normality diagnostics in fully adjusted model(previously crude models tested in Geoda same results)
#After observation of non normality in the residuals, test spatial error and lag models
#library(readr)
data <- read.csv("full_dataset28.07.csv")


#OLS Hispanic and all metals, fully adjusted model
modelHAS <- lm(logAs~jHispanic + jAfrican.A + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelHAS)
modelHBA <- lm(logBa~jHispanic + jAfrican.A + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelHBA)
modelHSE <- lm(logSe~jHispanic + jAfrican.A + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelHSE)
modelHU <- lm(logU~jHispanic + jAfrican.A + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelHU)

#import the shapefile and create the weights matrix for the spatial regression tests
working <- getwd()
TRI_poly <- readOGR(dsn = working, layer ="Copia de motherfull21.07")
# Create a queen's neighborhood weight matrix using the poly2nb command.
TRI_nbq <- poly2nb(TRI_poly)
# Convert the neighborhood matrix into a list so that the connections between counties can be used
TRI_nbq_w <- nb2listw(TRI_nbq, zero.policy = TRUE)

# Run Langrane Multiplier tests to identify the type of spatial regression model to run for all models
TRI.modelHAS <- lm.LMtests(modelHAS,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelHAS)

TRI.modelHBA <- lm.LMtests(modelHBA,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelHBA)

TRI.modelHSE <- lm.LMtests(modelHSE,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelHSE)

TRI.modelHU <- lm.LMtests(modelHU,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelHU)


#non-Hispanic African American and all metals (fully adjusted)
modelAAS <- lm(logAs~jAfrican.A + jHispanic + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelAAS)

modelAABA <- lm(logBa~jAfrican.A + jHispanic + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelAABA)

modelAASE <- lm(logSe~jAfrican.A + jHispanic + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelAASE)

modelAAU <- lm(logU~jAfrican.A + jHispanic + jAmerican + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelAAU)

# Run Langrane Multiplier tests to identify the type of spatial regression model to run for all models
TRI.modelAAS <- lm.LMtests(modelAAS,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAAS)

TRI.modelAABA <- lm.LMtests(modelAABA,TRI_nbq_w, test=c("LMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAABA)

TRI.modelAASE <- lm.LMtests(modelAASE,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAASE)

TRI.modelAAU <- lm.LMtests(modelAAU,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAAU)

#American Indian or Alaska Natives and and all metals (fully adjusted)
modelAIAS <- lm(logAs~jAmerican + jAfrican.A + jHispanic +  jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelAIAS)
modelAIB <- lm(logBa~jAmerican +jAfrican.A + jHispanic + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelAIB)
modelAISE <- lm(logSe~jAmerican +jAfrican.A + jHispanic  + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelAISE)
modelAIU <- lm(logU~jAmerican +jAfrican.A + jHispanic  + jHigh.scho+ jMedian.ho+ jdensity+ GWPerc10 , data=data)
ols_test_normality(modelAIU)

# Run Lagrane Multiplier tests to identify the type of spatial regression model to run for all models
TRI.modelAIAS <- lm.LMtests(modelAIAS,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAIAS)

TRI.modelAIB <- lm.LMtests(modelAIB,TRI_nbq_w, test=c("LMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAIB)

TRI.modelAISE <- lm.LMtests(modelAISE,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAISE)

TRI.modelAIU <- lm.LMtests(modelAIU,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelAIU)

#non-Hispanic white and and all metals (fully adjusted)
modelWAS <- lm(logAs~jWhite + jAmerican + jAfrican.A  +  jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelWAS)

modelWB <- lm(logBa~jWhite +jAmerican +jAfrican.A + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelWB)

modelWSE <- lm(logSe~jWhite +jAmerican +jAfrican.A  + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10, data=data)
ols_test_normality(modelWSE)

modelWU <- lm(logU~jWhite +jAmerican +jAfrican.A + jHigh.scho+ jMedian.ho+ jdensity + GWPerc10 , data=data)
ols_test_normality(modelWU)

# Run Lagrane Multiplier tests to identify the type of spatial regression model to run for all models
TRI.modelWAS <- lm.LMtests(modelWAS,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelWAS)

TRI.modelWB <- lm.LMtests(modelWB,TRI_nbq_w, test=c("LMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelWB)

TRI.modelWSE <- lm.LMtests(modelWSE,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelWSE)

TRI.modelWU <- lm.LMtests(modelWU,TRI_nbq_w, test=c("LMerr","LMlag","SARMA"), zero.policy = TRUE)
print(TRI.modelWU)

#END. NOTE THAT THE ESTIMATES ARE THE SAME FOR THE3 FIRST MODELS BECAUSE THEY INCLDE THE SAME RACIAL GROUPS.


#Supplemental figure 1. correlogram
install.packages("mctest")
library(mctest)
library(corrplot)
library(readr)
variables <- read_csv("analytic_data2013_clean.9august.csv")

variables <- col_types = cols(Children_poverty = col_number(), 
                              `free lunch`  = col_number(), child_uninsur = col_number(), 
                              adult_uninsur = col_number(), Rural  = col_number(), 
                              `Some college_prop` = col_number(), `Median household income` = col_number(),
                              `High school graduation`= col_number,no_social_support = col_number(),
                              Unemployment = col_number())

library(tidyverse)
variables2 <- variables %>% 
  rename(Child.pov = Children_poverty, C_uni = child_uninsur, No.insur = adult_uninsur, College = `Some college_prop`, MHI = `Median household income`, 
         HS = `High school graduation`, Support = no_social_support, Unemp = Unemployment, Free.lunch = `free lunch`)


variablesnum <- variables2 %>%
  na.omit() %>%
  select(Child.pov, Free.lunch, No.insur, College, MHI, HS, Unemp, Support )


variablescor <- cor(variablesnum)
corrplot(variablescor, method = "number")
corrplot(variablescor, method = "number", type = "lower")

summary(variables)

#VIF graph

modelVIF <-lm(Hispanic~child_uninsur + `free lunch`  + `High school graduation` + `Some college_prop`+ Unemployment + `Children poverty` + `Median household income`, data=dataVIF)

mc.plot(modelVIF, Inter = FALSE, vif = 5, ev = 0.1)

library(corrplot)


### end



####----- 8 GWR ####
library(spdep)
library(maptools)
library(rgdal)
library(spatialreg)
library(spgwr)


working <- getwd()
#Read in the data and create subsets for every R/E group and metal with no NAs
#Hispanic and arsenic
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
HIS <- readOGR(dsn = working, layer ="mergedHIS")
HISas1 <- HIS[!is.na(HIS$logAs) ,]
HISas1 <- na.omit(HISas1)
#we print the clean shapefile and save it
writeOGR(HISas1, 
         dsn = working, 
         layer= "HISas",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG1 <- gwr.sel(logAs ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISas1, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HISas = gwr(logAs ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISas1, 
                adapt=bwG1,
                hatmatrix=TRUE,
                se.fit=TRUE)
#double check the data
gwr.HISas
#save the estimates into a shapefile to plot in QGIS
names (gwr.HISas$SDF)
writeOGR(gwr.HISas$SDF, 
         dsn = working, 
         layer= "HISas_GWR",
         driver="ESRI Shapefile")

#repeat for the other metals and R/E groups
#Read in the data and create subsets for every R/E group and metal with no NAs
#Hispanic and uranium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
HIS <- readOGR(dsn = working, layer ="mergedHIS")
HISu <- HIS[!is.na(HIS$logU) ,]
HISu <- na.omit(HISu)
#we print the clean shapefile and save it
writeOGR(HISu, 
         dsn = working, 
         layer= "HISu",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG2 <- gwr.sel(logU ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISu, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HISu = gwr(logU ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= HISu, 
               adapt=bwG2,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.HISu
#save the estimates into a shapefile to plot in QGIS
names (gwr.HISu$SDF)
writeOGR(gwr.HISu$SDF, 
         dsn = working, 
         layer= "HISu_GWR",
         driver="ESRI Shapefile")

#Hispanic and selenium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
HIS <- readOGR(dsn = working, layer ="mergedHIS")
HISse <- HIS[!is.na(HIS$logSe) ,]
HISse <- na.omit(HISse)
#we print the clean shapefile and save it
writeOGR(HISse, 
         dsn = working, 
         layer= "HISse",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG3 <- gwr.sel(logSe ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISse, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HISse = gwr(logSe ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISse, 
                adapt=bwG3,
                hatmatrix=TRUE,
                se.fit=TRUE)
#double check the data
gwr.HISse
#save the estimates into a shapefile to plot in QGIS
names (gwr.HISse$SDF)
writeOGR(gwr.HISse$SDF, 
         dsn = working, 
         layer= "HISse_GWR",
         driver="ESRI Shapefile")

#Hispanic and barium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
HIS <- readOGR(dsn = working, layer ="mergedHIS")
HISba <- HIS[!is.na(HIS$logBa) ,]
HISba <- na.omit(HISba)
#we print the clean shapefile and save it
writeOGR(HISba, 
         dsn = working, 
         layer= "HISba",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG4 <- gwr.sel(logBa ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISba, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.HISba = gwr(logBa ~ jHspn + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= HISba, 
                adapt=bwG4,
                hatmatrix=TRUE,
                se.fit=TRUE)
#double check the data
gwr.HISba
#save the estimates into a shapefile to plot in QGIS
names (gwr.HISba$SDF)
writeOGR(gwr.HISba$SDF, 
         dsn = working, 
         layer= "HISba_GWR",
         driver="ESRI Shapefile")


#### --- 8.1 GWR Non-Hispanic Black ####
#nHBlack and Arsenic
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AA <- readOGR(dsn = working, layer ="mergedAA")
AAas <- AA[!is.na(AA$logAs) ,]
AAas <- na.omit(AAas)
#we print the clean shapefile and save it
writeOGR(AAas, 
         dsn = working, 
         layer= "AAas",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG5 <- gwr.sel(logAs ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= AAas, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AAas = gwr(logAs ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AAas, 
               adapt=bwG5,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AAas
#save the estimates into a shapefile to plot in QGIS
names (gwr.AAas$SDF)
writeOGR(gwr.AAas$SDF, 
         dsn = working, 
         layer= "AAas_GWR",
         driver="ESRI Shapefile")

#nHBlack and Uranium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AA <- readOGR(dsn = working, layer ="mergedAA")
AAu <- AA[!is.na(AA$logU) ,]
AAu <- na.omit(AAu)
#we print the clean shapefile and save it
writeOGR(AAu, 
         dsn = working, 
         layer= "AAu",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG6 <- gwr.sel(logU ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= AAu, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AAu = gwr(logU ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
              data= AAu, 
              adapt=bwG6,
              hatmatrix=TRUE,
              se.fit=TRUE)
#double check the data
gwr.AAu
#save the estimates into a shapefile to plot in QGIS
names (gwr.AAu$SDF)
writeOGR(gwr.AAu$SDF, 
         dsn = working, 
         layer= "AAu_GWR",
         driver="ESRI Shapefile")

#nHBlack and Selenium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AA <- readOGR(dsn = working, layer ="mergedAA")
AAse <- AA[!is.na(AA$logSe) ,]
AAse <- na.omit(AAse)
#we print the clean shapefile and save it
writeOGR(AAse, 
         dsn = working, 
         layer= "AAse",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG7 <- gwr.sel(logSe ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= AAse, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AAse = gwr(logSe ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AAse, 
               adapt=bwG7,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AAse
#save the estimates into a shapefile to plot in QGIS
names (gwr.AAse$SDF)
writeOGR(gwr.AAse$SDF, 
         dsn = working, 
         layer= "AAse_GWR",
         driver="ESRI Shapefile")

#nHBlack and Barium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AA <- readOGR(dsn = working, layer ="mergedAA")
AAba <- AA[!is.na(AA$logBa) ,]
AAba <- na.omit(AAba)
#we print the clean shapefile and save it
writeOGR(AAba, 
         dsn = working, 
         layer= "AAba",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG8 <- gwr.sel(logBa ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= AAba, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AAba = gwr(logBa ~ jAf_A + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AAba, 
               adapt=bwG8,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AAba
#save the estimates into a shapefile to plot in QGIS
names (gwr.AAba$SDF)
writeOGR(gwr.AAba$SDF, 
         dsn = working, 
         layer= "AAba_GWR",
         driver="ESRI Shapefile")

####----- 8.2 GWR American Indian/Alaskan Natives ####
#AI and Arsenic
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AI <- readOGR(dsn = working, layer ="mergedAI")
AIas <- AI[!is.na(AI$logAs) ,]
AIas <- na.omit(AIas)
#we print the clean shapefile and save it
writeOGR(AIas, 
         dsn = working, 
         layer= "AIas",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG9 <- gwr.sel(logAs ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
                data= AIas, 
                gweight=gwr.Gauss, 
                verbose=TRUE, 
                adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AIas = gwr(logAs ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AIas, 
               adapt=bwG9,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AIas
#save the estimates into a shapefile to plot in QGIS
names (gwr.AIas$SDF)
writeOGR(gwr.AIas$SDF, 
         dsn = working, 
         layer= "AIas_GWR",
         driver="ESRI Shapefile")

#AI and Uranium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AI <- readOGR(dsn = working, layer ="mergedAI")
AIu <- AI[!is.na(AI$logU) ,]
AIu <- na.omit(AIu)
#we print the clean shapefile and save it
writeOGR(AIu, 
         dsn = working, 
         layer= "AIu",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG10 <- gwr.sel(logU ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= AIu, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AIu = gwr(logU ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
              data= AIu, 
              adapt=bwG10,
              hatmatrix=TRUE,
              se.fit=TRUE)
#double check the data
gwr.AIu
#save the estimates into a shapefile to plot in QGIS
names (gwr.AIu$SDF)
writeOGR(gwr.AIu$SDF, 
         dsn = working, 
         layer= "AIu_GWR",
         driver="ESRI Shapefile", overwrite_layer = TRUE)

#AI and Selenium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AI <- readOGR(dsn = working, layer ="mergedAI")
AIse <- AI[!is.na(AI$logSe) ,]
AIse <- na.omit(AIse)
#we print the clean shapefile and save it
writeOGR(AIse, 
         dsn = working, 
         layer= "AIse",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG11 <- gwr.sel(logSe ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= AIse, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AIse = gwr(logSe ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AIse, 
               adapt=bwG11,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AIse
#save the estimates into a shapefile to plot in QGIS
names (gwr.AIse$SDF)
writeOGR(gwr.AIse$SDF, 
         dsn = working, 
         layer= "AIse_GWR",
         driver="ESRI Shapefile")

#AI and Barium
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
AI <- readOGR(dsn = working, layer ="mergedAI")
AIba <- AI[!is.na(AI$logBa) ,]
AIba <- na.omit(AIba)
#we print the clean shapefile and save it
writeOGR(AIba, 
         dsn = working, 
         layer= "AIba",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG12 <- gwr.sel(logBa ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= AIba, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.AIba = gwr(logBa ~ jAmrc + jMdn_ + jHgh_ + jdnst + GWP10 , 
               data= AIba, 
               adapt=bwG12,
               hatmatrix=TRUE,
               se.fit=TRUE)
#double check the data
gwr.AIba
#save the estimates into a shapefile to plot in QGIS
names (gwr.AIba$SDF)
writeOGR(gwr.AIba$SDF, 
         dsn = working, 
         layer= "AIba_GWR",
         driver="ESRI Shapefile")

#####---- 8.3 non Hispanic white ####
#NH w and Arsenic
#we fist remove the NAs from the selected metal (limiting factor, and then we omit all NAs from the other ones)
W <- readOGR(dsn = working, layer ="mergedW")
Was <- W[!is.na(W$logAs) ,]
Was <- na.omit(Was)
#we print the clean shapefile and save it
writeOGR(Was, 
         dsn = working, 
         layer= "Was",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG13 <- gwr.sel(logAs ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= Was, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.Was = gwr(logAs ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
              data= Was, 
              adapt=bwG13,
              hatmatrix=TRUE,
              se.fit=TRUE)
#double check the data
gwr.Was
#save the estimates into a shapefile to plot in QGIS
names (gwr.Was$SDF)
writeOGR(gwr.Was$SDF, 
         dsn = working, 
         layer= "Was_GWR",
         driver="ESRI Shapefile")


#non Hispanic white and Uranium 
W <- readOGR(dsn = working, layer ="mergedW")
Wu <- W[!is.na(W$logU) ,]
Wu <- na.omit(Wu)
#we print the clean shapefile and save it
writeOGR(Wu, 
         dsn = working, 
         layer= "Wu",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG14 <- gwr.sel(logU ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= Wu, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.Wu = gwr(logU ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
             data= Wu, 
             adapt=bwG14,
             hatmatrix=TRUE,
             se.fit=TRUE)
#double check the data
gwr.Wu
#save the estimates into a shapefile to plot in QGIS
names (gwr.Wu$SDF)
writeOGR(gwr.Wu$SDF, 
         dsn = working, 
         layer= "Wu_GWR",
         driver="ESRI Shapefile")


#non Hispanic white and Selenium
W <- readOGR(dsn = working, layer ="mergedW")
Wse <- W[!is.na(W$logSe) ,]
Wse <- na.omit(Wse)
#we print the clean shapefile and save it
writeOGR(Wse, 
         dsn = working, 
         layer= "Wse",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG15 <- gwr.sel(logSe ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= Wse, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.Wse = gwr(logSe ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
              data= Wse, 
              adapt=bwG15,
              hatmatrix=TRUE,
              se.fit=TRUE)
#double check the data
gwr.Wse
#save the estimates into a shapefile to plot in QGIS
names (gwr.Wse$SDF)
writeOGR(gwr.Wse$SDF, 
         dsn = working, 
         layer= "Wse_GWR",
         driver="ESRI Shapefile")

#non Hispanic white and Barium
W <- readOGR(dsn = working, layer ="mergedW")
Wba <- W[!is.na(W$logBa) ,]
Wba <- na.omit(Wba)
#we print the clean shapefile and save it
writeOGR(Wba, 
         dsn = working, 
         layer= "Wba",
         driver="ESRI Shapefile",overwrite_layer = TRUE)

#create the adaptative bandwidth
bwG16 <- gwr.sel(logBa ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
                 data= Wba, 
                 gweight=gwr.Gauss, 
                 verbose=TRUE, 
                 adapt = TRUE)
#run GWR with adaptative bandwith and with se and residuals, and global R2
gwr.Wba = gwr(logBa ~ jWhit + jMdn_ + jHgh_ + jdnst + GWP10 , 
              data= Wba, 
              adapt=bwG16,
              hatmatrix=TRUE,
              se.fit=TRUE)
#double check the data
gwr.Wba
#save the estimates into a shapefile to plot in QGIS
names (gwr.Wba$SDF)
writeOGR(gwr.Wba$SDF, 
         dsn = working, 
         layer= "Wba_GWR",
         driver="ESRI Shapefile")

