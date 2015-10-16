B19201_county <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19201_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
B19131_county <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19131_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
S1101_county <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_S1101_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
DP03_county <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_DP03_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
S1101_OR <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_S1101_OR_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
B19131_OR <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19131_OR_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
B19201_OR <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19201_OR_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
DP03_OR <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_DP03_OR_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)

# combine county and state data into one dataframe
DP03 <- rbind(DP03_OR[2,],DP03_county[2:37,])
S1101 <- rbind(S1101_OR[2,],S1101_county[2:37,])
B19201 <- rbind(B19201_OR[2,],B19201_county[2:37,])
B19131 <- rbind(B19131_OR[2,],B19131_county[2:37,])

## subset to remove the first and third columns -HO
## S1101 <- subset(S1101, select=-c(GEO.id, GEO.display.label))

# only keep the columns we need.

# S1101: geo.id2 = fips, HC01_EST_VC02 = total households
S1101 <- subset(S1101, select=c(GEO.id2, HC01_EST_VC02))

# B19201: geo.id2 = fips, HD01_VD02,HD01_VD03,HD01_VD04 = lowIncomeSingleAdults
B19201 <- subset(B19201, select=c(GEO.id2,HD01_VD02,HD01_VD03,HD01_VD04))
B19201["lowIncomeSingleAdults"] <- as.numeric(B19201$HD01_VD02)+as.numeric(B19201$HD01_VD03)+as.numeric(B19201$HD01_VD04)
B19201 <- subset(B19201, select=c(GEO.id2,lowIncomeSingleAdults))

# B19131: geo.id2 = fips, HD01_VD40,HD01_VD41,HD01_VD42,HD01_VD75,HD01_VD76,HD01_VD77 = lowIncomeSingleParents, HD01_VD04,HD01_VD05,HD01_VD06,HD01_VD07,HD01_VD08,HD01_VD09,HD01_VD10 = lowIncomeMarriedParents
B19131 <- subset(B19131, select=c(GEO.id2,HD01_VD40,HD01_VD41,HD01_VD42,HD01_VD75,HD01_VD76,HD01_VD77,HD01_VD04,HD01_VD05,HD01_VD06,HD01_VD07,HD01_VD08,HD01_VD09,HD01_VD10))
B19131["lowIncomeSingleParents"] <- as.numeric(B19131$HD01_VD40)+as.numeric(B19131$HD01_VD41)+as.numeric(B19131$HD01_VD42)+as.numeric(B19131$HD01_VD75)+as.numeric(B19131$HD01_VD76)+as.numeric(B19131$HD01_VD77)
B19131["lowIncomeMarriedParents"] <- as.numeric(B19131$HD01_VD04)+as.numeric(B19131$HD01_VD05)+as.numeric(B19131$HD01_VD06)+as.numeric(B19131$HD01_VD07)+as.numeric(B19131$HD01_VD08)+as.numeric(B19131$HD01_VD09)+as.numeric(B19131$HD01_VD10)
B19131 <- subset(B19131,select=c(GEO.id2,lowIncomeSingleParents,lowIncomeMarriedParents))

## then subset again to remove all the margin of error columns -HO
## strng <- "_MOE_"
## S1101 <- S1101[, -grep(strng, colnames(dS1101s))]

## get rid of MoE columns using ID row. -HO
## string <- "Margin of Error"
## B19131 <- B19131[, -grep(string, dB19131s[1,])]

# change the names of the columns
colnames(S1101) <- c("fips","totalHouseholds")
colnames(B19131)[1] <- "fips"
colnames(B19201)[1] <- "fips"

# merge columns into one dataframe
censusData <- merge(S1101,B19131,by="fips")
censusData <- merge(censusData,B19201,by="fips")

# write csv
write.csv(censusData, file = "dbV2/censusData.csv")

