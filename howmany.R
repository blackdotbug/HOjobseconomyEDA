library(SciencesPo)
#### code to show how many freq 
####looking for a way to show signicance of low income jobs that stay put and re taken for granted
#data(lowincomeworker)
#long list of varibles
#statcount2 alljobs less than 29; statcount3 all jobs 30 to 54; statcount4 alljobs 55 over
#statcount5 earnless than 1250
#statcount 6 to 25 diff types of jobs
#start by looking at stat count 20 education; statcount21 healthcare; statcount 23 food service;
#and statcount 6 ag
###figure out how to do crosstabs

###using freq does not give useful answers  freq(lowincomeworkers$stat6count)
##ag
summary(lowincomeworkers$stat6count)
##ed
summary(lowincomeworkers$stat20count)
##health care
summary(lowincomeworkers$stat21count)
##food service 
summary(lowincomeworkers$stat23count)
##earn less than 1250
summary(lowincomeworkers$stat5count)
###going to try this 
# options(scipen=999)
