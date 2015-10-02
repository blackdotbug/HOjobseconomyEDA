##new files in allfam folder
md131 <- (ACS_13_1YR_B05010_metadata)
d131 <- (ACS_13_1YR_B05010_with_ann)

#only import and work with the files that have all counties in the data. Sort first, please. -HO
dS1702 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/haseverycounty/ACS_13_5YR_S1702_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
dS1101 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_S1101_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
dB19131 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19131_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
dB08202 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B08202_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
dB23009 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B23009_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)
dB19126 <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/americanfactfinder/raiseeffectcandidates/ACS_13_5YR_B19126_with_ann.csv", header=TRUE, stringsAsFactors=FALSE)

# not the most useful
summary(d131)

##
library(dplyr)
d131s <- dplyr::slice(d131,2:12)

##this got rid of extra row
d131s
summary(d131s)

##still not useful
##going to try plotting  lables are third column geo.., don't use first two colums
#AreaName <- factor (allfama$AreaName)
#barplot (allfama$a1i0p0s0t0, names.arg = AreaName)

county <- factor(d131s$GEO.display.label)

##plot(d131s$HD01_VD01)
### need to rotate data
library(tidyr)
###tidyr::gather(d131s, county)
### getting closer need to peel off first three columns !!! KEEP ID2! That's the FIPS code! We need that in the DB! -HO !!!
###dplyr::select(d131s,d131s$HD01_VD01,d131s$HD02_VD01,d131s$HD01_VD02,d131s$HD02_VD02,d131s$HD01_VD03,d131s$HD02_VD03,d131s$HD01_VD04,d131s$HD02_VD04,d131s$HD01_VD05,d131s$HD02_VD05,d131s$HD01_VD06,d131s$HD02_VD06,d131s$HD01_VD07,d131s$HD02_VD07,d131s$HD01_VD08,d131s$HD02_VD08,d131s$HD01_VD09,d131s$HD02_VD09,d131s$HD01_VD10,d131s$HD02_VD10,d131s$HD01_VD11,d131s$HD02_VD11,d131s$HD01_VD12,d131s$HD02_VD12,d131s$HD01_VD13,d131s$HD02_VD13,d131s$HD01_VD14,d131s$HD02_VD14,d131s$HD01_VD15,d131s$HD02_VD15,d131s$HD01_VD16,d131s$HD02_VD16,d131s$HD01_VD17,d131s$HD02_VD17,d131s$HD01_VD18,d131s$HD02_VD18,d131s$HD01_VD19,d131s$HD02_VD19,d131s$HD01_VD20,d131s$HD02_VD20,d131s$HD01_VD21,d131s$HD02_VD21,d131s$HD01_VD22,d131s$HD02_VD22,d131s$HD01_VD23,d131s$HD02_VD23,d131s$HD01_VD24,d131s$HD02_VD24,d131s$HD01_VD25,d131s$HD02_VD25)
##figured it out
d131ss <-dplyr::select(d131s, 4:53)

## subset to remove the first and third columns -HO
dS1702s <- subset(dS1702, select=-c(GEO.id, GEO.display.label))
dS1101s <- subset(dS1101, select=-c(GEO.id, GEO.display.label))
dB19131s <- subset(dB19131, select=-c(GEO.id, GEO.display.label))
dB08202s <- subset(dB08202, select=-c(GEO.id, GEO.display.label))
dB23009s <- subset(dB23009, select=-c(GEO.id, GEO.display.label))
dB19126s <- subset(dB19126, select=-c(GEO.id, GEO.display.label))

## then subset again to remove all the margin of error columns -HO
strng <- "_MOE_"
dS1702s <- dS1702s[, -grep(strng, colnames(dS1702s))]
dS1101s <- dS1101s[, -grep(strng, colnames(dS1101s))]

## get rid of MoE columns using ID row. -HO
string <- "Margin of Error"
dB19131s <- dB19131s[, -grep(string, dB19131s[1,])]
dB08202s <- dB08202s[, -grep(string, dB08202s[1,])]
dB23009s <- dB23009s[, -grep(string, dB23009s[1,])]
dB19126s <- dB19126s[, -grep(string, dB19126s[1,])]

## change the name of the first column to FIPS
colnames(dS1101s)[1] <- "fips"
colnames(dS1702s)[1] <- "fips"
colnames(dB19131s)[1] <- "fips"
colnames(dB08202s)[1] <- "fips"
colnames(dB23009s)[1] <- "fips"
colnames(dB19126s)[1] <- "fips"

##tidyr::gather(d131ss)
summary(d131ss)
write.csv(d131ss, file = "d131ss.csv")

write.csv(dS1702s, file = "dbV2/dS1702s.csv")
write.csv(dS1101s, file = "dbV2/dS1101s.csv")
write.csv(dB19131s, file = "dbV2/dB19131s.csv")
write.csv(dB08202s, file = "dbV2/dB08202s.csv")
write.csv(dB23009s, file = "dbV2/dB23009s.csv")
write.csv(dB19126s, file = "dbV2/dB19126s.csv")

##let us try one more
d8202 <-(ACS_13_1YR_B08202_with_ann)
d8202s <-(dplyr::slice(d8202, 2:12))
d8202ss <-dplyr::select(d8202s, contains("H"))
##the order has not changed county names will line up
write.csv(d8202ss, file = "d8202ss.csv")
