library(dplyr)
sssbudget <- read.csv("~/Documents/hackoregon/csvs/sssbudget.csv")
censusHouseholds <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/dbV2/censusHouseholds.csv", stringsAsFactors=FALSE)
households41 <- as.numeric(censusHouseholds[1,c("totalHouseholds")])
censusHouseholds$countyWeight <- censusHouseholds$totalHouseholds/households41
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

singleParentSubtypes <- c("a1i2p0s0t0","a1i1p1s0t0","a1i1p0s1t0","a1i1p0s0t1","a1i0p2s0t0","a1i0p1s1t0","a1i0p1s0t1","a1i0p0s2t0","a1i0p0s1t1","a1i0p0s0t2")
marriedParentSubtypes <- c("a2i2p0s0t0","a2i1p1s0t0","a2i1p0s1t0","a2i1p0s0t1","a2i0p2s0t0","a2i0p1s1t0","a2i0p1s0t1","a2i0p0s2t0","a2i0p0s1t1","a2i0p0s0t2")

sssbudgetSingleAdult <- sssbudgetOR[sssbudgetOR$familyCode == "a1i0p0s0t0", 2:8]
write.csv(sssbudgetSingleAdult, file = "dbV2/sssbudgetSingleAdultOR.csv")

sssbudgetSingleParent <- sssbudgetOR[sssbudgetOR$familyCode %in% singleParentSubtypes,]
singleParentWeights <- data.frame(familyCode = c("a1i2p0s0t0", "a1i1p1s0t0", "a1i1p0s1t0", "a1i1p0s0t1", "a1i0p2s0t0", "a1i0p1s1t0", "a1i0p1s0t1", "a1i0p0s2t0", "a1i0p0s1t1", "a1i0p0s0t2"), weight = c(0.057406744, 0.114813488, 0.04326598, 0.03709431, 0.057406744, 0.04326598, 0.03709431, 0.176738244, 0.302997265, 0.129855971))
sssbudgetSingleParent <- merge(sssbudgetSingleParent, singleParentWeights, by="familyCode")
sssbudgetSingleParentOR <- lapply(sssbudgetSingleParent[,2:8], weighted.mean, w = sssbudgetSingleParent$weight)
write.csv(sssbudgetSingleParentOR, file = "dbV2/sssbudgetSingleParentOr.csv")

sssbudgetMarriedParents <- sssbudgetOR[sssbudgetOR$familyCode %in% marriedParentSubtypes,]
marriedParentWeights <- data.frame(familyCode = c("a2i2p0s0t0", "a2i1p1s0t0", "a2i1p0s1t0", "a2i1p0s0t1", "a2i0p2s0t0", "a2i0p1s1t0", "a2i0p1s0t1", "a2i0p0s2t0", "a2i0p0s1t1", "a2i0p0s0t2"), weight = c(0.05825, 0.1165, 0.0581472, 0.0498528, 0.05825, 0.0581472, 0.0498528, 0.1597349, 0.273847, 0.117363))
sssbudgetMarriedParents <- merge(sssbudgetMarriedParents, marriedParentWeights, by="familyCode")
sssbudgetMarriedParentsOR <- lapply(sssbudgetMarriedParents[,2:8], weighted.mean, w=sssbudgetMarriedParents$weight)
write.csv(sssbudgetMarriedParentsOR, file = "dbV2/sssbudgetMarriedParentsOr.csv")
