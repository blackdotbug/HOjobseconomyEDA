library(dplyr)

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
S1101s1 <- subset(S1101, select=c(GEO.id2, HC01_EST_VC02, HC02_EST_VC02, HC05_EST_VC02))
colnames(S1101s1) <- c("fips","totalHouseholds","totalMarriedFamilyHouseholds","totalNonFamilyHouseholds")
S1101s1$totalHouseholds <- as.numeric(S1101s1$totalHouseholds)
S1101s1$totalMarriedFamilyHouseholds <- as.numeric(S1101s1$totalMarriedFamilyHouseholds)
S1101s1$totalNonFamilyHouseholds <- as.numeric(S1101s1$totalNonFamilyHouseholds)
S1101s1["totalUnmarriedFamilyHouseholds"] <- S1101s1$totalHouseholds - (S1101s1$totalMarriedFamilyHouseholds + S1101s1$totalNonFamilyHouseholds)

# B19201: geo.id2 = fips, HD01_VD02,HD01_VD03,HD01_VD04 = lowIncomeSingleAdults
B19201s1 <- subset(B19201, select=c(GEO.id2))
B19201s1["lowIncomeSingleAdults"] <- as.numeric(B19201$HD01_VD02)+as.numeric(B19201$HD01_VD03)+as.numeric(B19201$HD01_VD04)

# B19131: geo.id2 = fips, HD01_VD40,HD01_VD41,HD01_VD42,HD01_VD75,HD01_VD76,HD01_VD77 = lowIncomeSingleParents, HD01_VD04,HD01_VD05,HD01_VD06,HD01_VD07,HD01_VD08,HD01_VD09,HD01_VD10 = lowIncomeMarriedParents
B19131s1 <- subset(B19131, select=c(GEO.id2))
B19131s1["lowIncomeSingleParents"] <- as.numeric(B19131$HD01_VD40)+as.numeric(B19131$HD01_VD41)+as.numeric(B19131$HD01_VD42)+as.numeric(B19131$HD01_VD75)+as.numeric(B19131$HD01_VD76)+as.numeric(B19131$HD01_VD77)
B19131s1["lowIncomeMarriedParents"] <- as.numeric(B19131$HD01_VD04)+as.numeric(B19131$HD01_VD05)+as.numeric(B19131$HD01_VD06)+as.numeric(B19131$HD01_VD07)+as.numeric(B19131$HD01_VD08)+as.numeric(B19131$HD01_VD09)+as.numeric(B19131$HD01_VD10)

## then subset again to remove all the margin of error columns -HO
## strng <- "_MOE_"
## S1101 <- S1101[, -grep(strng, colnames(dS1101s))]

## get rid of MoE columns using ID row. -HO
## string <- "Margin of Error"
## B19131 <- B19131[, -grep(string, dB19131s[1,])]

# change the names of the columns
colnames(B19131s1)[1] <- "fips"
colnames(B19201s1)[1] <- "fips"

# merge columns into one dataframe
censusHouseholds <- merge(S1101s1,B19131s1,by="fips")
censusHouseholds <- merge(censusHouseholds,B19201s1,by="fips")

censusHouseholds["marriedAsPercentTotal"] <- censusHouseholds$totalMarriedFamilyHouseholds/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeMarriedAsPercentTotal"] <- censusHouseholds$lowIncomeMarriedParents/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeMarriedAsPercentMarried"] <- censusHouseholds$lowIncomeMarriedParents/censusHouseholds$totalMarriedFamilyHouseholds
censusHouseholds["unmarriedAsPercentTotal"] <- censusHouseholds$totalUnmarriedFamilyHouseholds/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeSingleParentsAsPercentTotal"] <- censusHouseholds$lowIncomeSingleParents/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeSingleParentsAsPercentUnmarried"] <- censusHouseholds$lowIncomeSingleParents/censusHouseholds$totalUnmarriedFamilyHouseholds
censusHouseholds["nonfamilyAsPercentTotal"] <- censusHouseholds$totalNonFamilyHouseholds/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeSingleAdultAsPercentTotal"] <- censusHouseholds$lowIncomeSingleAdults/censusHouseholds$totalHouseholds
censusHouseholds["lowIncomeSingleAdultAsPercentNonfamily"] <- censusHouseholds$lowIncomeSingleAdults/censusHouseholds$totalNonFamilyHouseholds

# write csv
write.csv(censusHouseholds, file = "dbV2/censusHouseholds.csv")

# import dataframe of family codes by fips code
familyCodeByFIPS <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/familyCodeWeights.csv", header=FALSE, stringsAsFactors=FALSE)

# subset S1101 keeping percentages of age of own children stats by family type
S1101s2 <- subset(S1101, select=c(GEO.id2,HC02_EST_VC11,HC02_EST_VC12,HC02_EST_VC13))
S1101s2$HC02_EST_VC11 <- as.numeric(S1101s2$HC02_EST_VC11)/100
S1101s2$HC02_EST_VC12 <- as.numeric(S1101s2$HC02_EST_VC12)/100
S1101s2$HC02_EST_VC13 <- as.numeric(S1101s2$HC02_EST_VC13)/100

# add column names
colnames(familyCodeByFIPS) <- c("familyCode","fips","weightChildAge")
colnames(S1101s2)[1] <- "fips"

# combine census stats with family code file, factor family codes, and split data frame by family code
familyCodeByFIPS <- merge(familyCodeByFIPS, S1101s2, by="fips")
familyCodeByFIPS$familyCode <- as.factor(familyCodeByFIPS$familyCode)
familyTypes <- split(familyCodeByFIPS, familyCodeByFIPS$familyCode)

# calculate weight for given family code

# a1i0p0s0t2
familyType01 <- familyTypes[[1]]
familyType01$weight <- (1-familyType01$HC02_EST_VC13)*familyType01$weightChildAge
familyType01 <- subset(familyType01, select = c(fips, familyCode, weight))

# a1i0p0s1t1
familyType02 <- familyTypes[[2]]
familyType02$weight <- (1-familyType02$HC02_EST_VC13)*familyType02$weightChildAge
familyType02 <- subset(familyType02, select = c(fips, familyCode, weight))

# a1i0p0s2t0
familyType03 <- familyTypes[[3]]
familyType03$weight <- (1-familyType03$HC02_EST_VC13)*familyType03$weightChildAge
familyType03 <- subset(familyType03, select = c(fips, familyCode, weight))

# a1i0p1s0t1
familyType04 <- familyTypes[[4]]
familyType04$weight <- (1-familyType04$HC02_EST_VC13)*familyType04$weightChildAge
familyType04 <- subset(familyType04, select = c(fips, familyCode, weight))

# a1i0p1s1t0
familyType05 <- familyTypes[[5]]
familyType05$weight <- (1-familyType05$HC02_EST_VC13)*familyType05$weightChildAge
familyType05 <- subset(familyType05, select = c(fips, familyCode, weight))

# a1i0p2s0t0
familyType06 <- familyTypes[[6]]
familyType06$weight <- (1-familyType06$HC02_EST_VC13)*familyType06$weightChildAge
familyType06 <- subset(familyType06, select = c(fips, familyCode, weight))

# a1i1p0s0t1
familyType07 <- familyTypes[[7]]
familyType07$weight <- (1-familyType07$HC02_EST_VC13)*familyType07$weightChildAge
familyType07 <- subset(familyType07, select = c(fips, familyCode, weight))

# a1i1p0s1t0
familyType08 <- familyTypes[[8]]
familyType08$weight <- (1-familyType08$HC02_EST_VC13)*familyType08$weightChildAge
familyType08 <- subset(familyType08, select = c(fips, familyCode, weight))

# a1i1p1s0t0
familyType09 <- familyTypes[[9]]
familyType09$weight <- (1-familyType09$HC02_EST_VC13)*familyType09$weightChildAge
familyType09 <- subset(familyType09, select = c(fips, familyCode, weight))

# a1i2p0s0t0
familyType10 <- familyTypes[[10]]
familyType10$weight <- (1-familyType10$HC02_EST_VC13)*familyType10$weightChildAge
familyType10 <- subset(familyType10, select = c(fips, familyCode, weight))

# a2i0p0s0t2
familyType11 <- familyTypes[[11]]
familyType11$weight <- familyType11$HC02_EST_VC11*familyType11$weightChildAge
familyType11 <- subset(familyType11, select = c(fips, familyCode, weight))

# a2i0p0s1t1
familyType12 <- familyTypes[[12]]
familyType12$weight <- familyType12$HC02_EST_VC11*familyType12$weightChildAge
familyType12 <- subset(familyType12, select = c(fips, familyCode, weight))

# a2i0p0s2t0
familyType13 <- familyTypes[[13]]
familyType13$weight <- familyType13$HC02_EST_VC11*familyType13$weightChildAge
familyType13 <- subset(familyType13, select = c(fips, familyCode, weight))

# a2i0p1s0t1
familyType14 <- familyTypes[[14]]
familyType14$weight <- familyType14$HC02_EST_VC11*familyType14$weightChildAge
familyType14 <- subset(familyType14, select = c(fips, familyCode, weight))

# a2i0p1s1t0
familyType15 <- familyTypes[[15]]
familyType15$weight <- familyType15$HC02_EST_VC11*familyType15$weightChildAge
familyType15 <- subset(familyType15, select = c(fips, familyCode, weight))

# a2i0p2s0t0
familyType16 <- familyTypes[[16]]
familyType16$weight <- familyType16$HC02_EST_VC11*familyType16$weightChildAge
familyType16 <- subset(familyType16, select = c(fips, familyCode, weight))

# a2i1p0s0t1
familyType17 <- familyTypes[[17]]
familyType17$weight <- familyType17$HC02_EST_VC11*familyType17$weightChildAge
familyType17 <- subset(familyType17, select = c(fips, familyCode, weight))

# a2i1p0s1t0
familyType18 <- familyTypes[[18]]
familyType18$weight <- familyType18$HC02_EST_VC11*familyType18$weightChildAge
familyType18 <- subset(familyType18, select = c(fips, familyCode, weight))

# a2i1p1s0t0
familyType19 <- familyTypes[[19]]
familyType19$weight <- familyType19$HC02_EST_VC11*familyType19$weightChildAge
familyType19 <- subset(familyType19, select = c(fips, familyCode, weight))

# a2i2p0s0t0
familyType20 <- familyTypes[[20]]
familyType20$weight <- familyType20$HC02_EST_VC11*familyType20$weightChildAge
familyType20 <- subset(familyType20, select = c(fips, familyCode, weight))

familyCodeWeights <- rbind_list(familyType20,familyType19,familyType18,familyType17,familyType16,familyType15,familyType14,familyType13,familyType12,familyType11,familyType10,familyType09,familyType08,familyType07,familyType06,familyType05,familyType04,familyType03,familyType02,familyType01)

familyCodeWeights <- subset(familyCodeWeights, select = c(fips,familyCode,weight))

write.csv(familyCodeWeights, file = "dbV2/familyCodeWeights.csv")
