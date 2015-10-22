library(dplyr)
sssbudget <- read.csv("~/Documents/hackoregon/csvs/sssbudget.csv")
censusHouseholds <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/dbV2/censusHouseholds.csv", stringsAsFactors=FALSE)
households41 <- as.numeric(censusHouseholds[1,c("totalHouseholds")])
censusHouseholds$countyWeight <- censusHouseholds$totalHouseholds/households41
displayedTypes <- c("a1i0p0s0t0","a1i2p0s0t0","a1i1p1s0t0","a1i1p0s1t0","a1i1p0s0t1","a1i0p2s0t0","a1i0p1s1t0","a1i0p1s0t1","a1i0p0s2t0","a1i0p0s1t1","a1i0p0s0t2","a2i2p0s0t0","a2i1p1s0t0","a2i1p0s1t0","a2i1p0s0t1","a2i0p2s0t0","a2i0p1s1t0","a2i0p1s0t1","a2i0p0s2t0","a2i0p0s1t1","a2i0p0s0t2")
sssbudget$familycode <- as.factor(sssbudget$familycode)
familycodes <- sssbudget$familycode
familyTypes <- split(sssbudget, sssbudget$familycode)
housingWM <- lapply(familyTypes, function(i){
  weighted.mean(i$housing, censusHouseholds[2:37,c("countyWeight")])
})
childcareWM <- lapply(familyTypes, function(i){
  weighted.mean(i$childcare, censusHouseholds[2:37,c("countyWeight")])
})
foodWM <- lapply(familyTypes, function(i){
  weighted.mean(i$food, censusHouseholds[2:37,c("countyWeight")])
})
transportationWM <- lapply(familyTypes, function(i){
  weighted.mean(i$transportation, censusHouseholds[2:37,c("countyWeight")])
})
healthcareWM <- lapply(familyTypes, function(i){
  weighted.mean(i$healthcare, censusHouseholds[2:37,c("countyWeight")])
})
taxesWM <- lapply(familyTypes, function(i){
  weighted.mean(i$taxes, censusHouseholds[2:37,c("countyWeight")])
})
miscellaneousWM <- lapply(familyTypes, function(i){
  weighted.mean(i$miscellaneous, censusHouseholds[2:37,c("countyWeight")])
})

sssbudgetOR <- as.data.frame(levels(familycodes))
colnames(sssbudgetOR) <- "familyCode"
housing <- as.data.frame(housingWM)
housing <- as.data.frame(t(housing))
sssbudgetOR$housing <- unlist(housing)
food <- as.data.frame(foodWM)
food <- as.data.frame(t(food))
sssbudgetOR$food <- unlist(food)
childcare <- as.data.frame(childcareWM)
childcare <- as.data.frame(t(childcare))
sssbudgetOR$childcare <- unlist(childcare)
healthcare <- as.data.frame(healthcareWM)
healthcare <- as.data.frame(t(healthcare))
sssbudgetOR$healthcare <- unlist(healthcare)
transportation <- as.data.frame(transportationWM)
transportation <- as.data.frame(t(transportation))
sssbudgetOR$transportation <- unlist(transportation)
taxes <- as.data.frame(taxesWM)
taxes <- as.data.frame(t(taxes))
sssbudgetOR$taxes <- unlist(taxes)
miscellaneous <- as.data.frame(miscellaneousWM)
miscellaneous <- as.data.frame(t(miscellaneous))
sssbudgetOR$miscellaneous <- unlist(miscellaneous)

sssbudgetORdislayed <- sssbudgetOR[sssbudgetOR$familyCode %in% displayedTypes,]

write.csv(sssbudgetORdislayed, file = "dbV2/sssbudgetORdisplayed.csv")
