y
y###first try using join
###objects 
allfam <- AllFamA
countyname <- counties
pop <- population
popname <- join(countyname, pop )
### it worked 
summary(popname)
head(popname)
summary(popname$county)y
summary(popname$mostcommonfamilytype)
###lets turn mostcommonfamilytype into factor
comfam <- (popname$mostcommonfamilytype)
comfam.f <- factor(comfam)
plot (popname$county,comfam.f)
summary(comfam.f)
library(SceincesPo)
freq(comfam.f)
###
allfama <- AllFamA
### cross table in SceincesPO 
plot(allfama$AreaName, allfama$a1i0p0s0t0)
###on track 
library(plotrix)
##battleship.plot(allfama$AreaName, allfama$a1i0p0s0t0)
##seats(36,3 )
### this is the right form now pretty up 
barplot( allfama$a1i0p0s0t0, names.arg = allfama$AreaName, 
         xlim = c(0,36) ,ylim = c(10000,30000),
         xaxs="i",
yaxs="i",
##axis(1,at=1:length(allfama$AreaName),labels=allfama$AreaName)

### something else for awhile
freq(allfama$a1i0p0s0t0)
freq(allfama$a1i0p0s0t1)

AreaName <- factor (allfama$AreaName)
barplot (allfama$a1i0p0s0t0, names.arg = AreaName)

### lets try ggplot where everything has different names
j <- qplot(allfama$a1i0p0s0t0, aes(x=AreaName) ) + geom_bar()

           