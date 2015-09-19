##new files in allfam folder
md131 <- (ACS_13_1YR_B05010_metadata)
d131 <- (ACS_13_1YR_B05010_with_ann)
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
### getting closer need to peel off first three columns
###dplyr::select(d131s,d131s$HD01_VD01,d131s$HD02_VD01,d131s$HD01_VD02,d131s$HD02_VD02,d131s$HD01_VD03,d131s$HD02_VD03,d131s$HD01_VD04,d131s$HD02_VD04,d131s$HD01_VD05,d131s$HD02_VD05,d131s$HD01_VD06,d131s$HD02_VD06,d131s$HD01_VD07,d131s$HD02_VD07,d131s$HD01_VD08,d131s$HD02_VD08,d131s$HD01_VD09,d131s$HD02_VD09,d131s$HD01_VD10,d131s$HD02_VD10,d131s$HD01_VD11,d131s$HD02_VD11,d131s$HD01_VD12,d131s$HD02_VD12,d131s$HD01_VD13,d131s$HD02_VD13,d131s$HD01_VD14,d131s$HD02_VD14,d131s$HD01_VD15,d131s$HD02_VD15,d131s$HD01_VD16,d131s$HD02_VD16,d131s$HD01_VD17,d131s$HD02_VD17,d131s$HD01_VD18,d131s$HD02_VD18,d131s$HD01_VD19,d131s$HD02_VD19,d131s$HD01_VD20,d131s$HD02_VD20,d131s$HD01_VD21,d131s$HD02_VD21,d131s$HD01_VD22,d131s$HD02_VD22,d131s$HD01_VD23,d131s$HD02_VD23,d131s$HD01_VD24,d131s$HD02_VD24,d131s$HD01_VD25,d131s$HD02_VD25)
##figured it out
d131ss <-dplyr::select(d131s, 4:53)
##tidyr::gather(d131ss)
summary(d131ss)
write.csv(d131ss, file = "d131ss.csv")
##let us try one more
d8202 <-(ACS_13_1YR_B08202_with_ann)
d8202s <-(dplyr::slice(d8202, 2:12))
d8202ss <-dplyr::select(d8202s, contains("H"))
##the order has not changed county names will line up
write.csv(d8202ss, file = "d8202ss.csv")
