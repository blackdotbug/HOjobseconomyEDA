library(ggplot2)
library(scales)
library(plyr)
library(RColorBrewer)
ORCountyEmploymentEarningsPayroll <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/ORCountyEmploymentEarningsPayroll.csv")
lowincomeworkers <- read.csv("~/Documents/hackoregon/CensusData/HOjobseconomyEDA/lowincomeworkers.csv")

lowincomeworkers$stat5count <- as.numeric(lowincomeworkers$stat5count)
lowincomeworkers$stat35count <- as.numeric(lowincomeworkers$stat25count)
lowincomecounties <- lowincomeworkers[2:36,]

plot1 <- ggplot(lowincomecounties) +
         geom_bar(aes(reorder(lowincomecounties$county,lowincomecounties$stat35share), lowincomecounties$stat5share, fill=lowincomecounties$stat34label), stat="identity") +
         geom_bar(aes(reorder(lowincomecounties$county, lowincomecounties$stat35share), lowincomecounties$stat35share, fill=lowincomecounties$stat35label), stat="identity", position="stack") +
         labs(x="Share of Jobs earning $1,250 per month or less", y="") +
         geom_text(aes(reorder(lowincomecounties$county,lowincomecounties$stat35share), lowincomecounties$stat5share, label=lowincomecounties$county, angle=90, hjust=1.05)) +
         theme(legend.title=element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.background = element_blank()) +
  scale_y_continuous(labels=percent_format())
print(plot1)

ggsave("gendershare.pdf", plot1, scale=2)

plot2 <- ggplot(lowincomecounties) +
  geom_bar(aes(reorder(lowincomecounties$county,lowincomecounties$stat33share), lowincomecounties$stat5share, fill=lowincomecounties$stat32label), stat="identity") +
  geom_bar(aes(reorder(lowincomecounties$county, lowincomecounties$stat33share), lowincomecounties$stat33share, fill=lowincomecounties$stat33label), stat="identity", position="stack") +
  labs(x="Share of Jobs earning $1,250 per month or less", y="") +
  geom_text(aes(reorder(lowincomecounties$county,lowincomecounties$stat33share), lowincomecounties$stat5share, label=lowincomecounties$county, angle=90, hjust=1.05)) +
  theme(legend.title=element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.background = element_blank()) +
  scale_y_continuous(labels=percent_format())
print(plot2)

ggsave("ethnicityshare.pdf", plot2, scale=2)

lowincomecounties$stat36count <- lowincomecounties$stat27count + lowincomecounties$stat28count +lowincomecounties$stat29count +lowincomecounties$stat30count +lowincomecounties$stat31count
lowincomecounties$stat36share <- lowincomecounties$stat27share + lowincomecounties$stat28share +lowincomecounties$stat29share +lowincomecounties$stat30share +lowincomecounties$stat31share
lowincomecounties$stat36label <- "All People of Color"
white <- lowincomecounties[,c("county", "stat26share", "stat26label")]
black <- lowincomecounties[,c("county", "stat27share", "stat27label")]
group3 <- lowincomecounties[,c("county", "stat28share", "stat28label")]
group4 <- lowincomecounties[,c("county", "stat29share", "stat29label")]
group5 <- lowincomecounties[,c("county", "stat30share", "stat30label")]
group6 <- lowincomecounties[,c("county", "stat31share", "stat31label")]
poc <- lowincomecounties[,c("county","stat36share","stat36label")]
names(white)[2]<-"share"
names(white)[3]<-"label"
names(black)[2]<-"share"
names(black)[3]<-"label"
names(group3)[2]<-"share"
names(group3)[3]<-"label"
names(group4)[2]<-"share"
names(group4)[3]<-"label"
names(group5)[2]<-"share"
names(group5)[3]<-"label"
names(group6)[2]<-"share"
names(group6)[3]<-"label"
names(poc)[2]<-"share"
names(poc)[3]<-"label"

allraces <- rbind.fill(white, black, group3, group4, group5, group6)
races <- split(allraces, allraces$label)
racebinary <- rbind.fill(white, poc)

fill_palette <- brewer.pal(6,"Dark2")
palette1 <- brewer.pal(12,"Set3")
palette2 <- brewer.pal(8,"Dark2")

plot3 <- ggplot(allraces,aes(x = county, y = share, fill = label, order = label)) +
  geom_bar(position = "stack",stat = "identity") +
  labs(x="Share of Jobs earning $1,250 per month or less", y="") +
  theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = fill_palette, guide = guide_legend(reverse=TRUE))
print(plot3)
ggsave("raceshare.pdf", plot3, scale=2)

plot3a <- ggplot(races[[1]], aes(x=county, y=share, fill=fill_palette[6])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[1]]$label) + scale_fill_manual(values=fill_palette[6], guide=FALSE)
print(plot3a)
plot3b <- ggplot(races[[2]], aes(x=county, y=share, fill=fill_palette[1])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[2]]$label) + scale_fill_manual(values=fill_palette[1], guide=FALSE)
print(plot3b)
plot3c <- ggplot(races[[3]], aes(x=county, y=share, fill=fill_palette[2])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[3]]$label) + scale_fill_manual(values=fill_palette[2], guide=FALSE)
print(plot3c)
plot3d <- ggplot(races[[4]], aes(x=county, y=share, fill=fill_palette[3])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[4]]$label) + scale_fill_manual(values=fill_palette[3], guide=FALSE)
print(plot3d)
plot3e <- ggplot(races[[5]], aes(x=county, y=share, fill=fill_palette[4])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[5]]$label) + scale_fill_manual(values=fill_palette[4], guide=FALSE)
print(plot3e)
plot3f <- ggplot(races[[6]], aes(x=county, y=share, fill=fill_palette[5])) + geom_bar(stat="identity") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90), axis.text.y = element_text(angle = 90)) + scale_y_continuous(labels = percent_format()) + labs(x="",y=races[[6]]$label) + scale_fill_manual(values=fill_palette[5], guide=FALSE)
print(plot3f)

pdf(file="raceshareindividual.pdf",width = 11,height = 8.5)
print(plot3a)
print(plot3b)
print(plot3c)
print(plot3d)
print(plot3e)
print(plot3f)
dev.off()

plot4 <- ggplot(racebinary,aes(x = county, y = share, fill = label, order = label)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(x="Share of Jobs earning $1,250 per month or less", y="") +
  theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = percent_format())  +
  scale_fill_manual(values = fill_palette[3:4], guide = guide_legend(reverse=TRUE))
print(plot4)
ggsave("racebinaryshare.pdf", plot4, scale=2)

industry01 <- lowincomecounties[,c("county","stat6share", "stat6label")]
industry02 <- lowincomecounties[,c("county","stat7share", "stat7label")]
industry03 <- lowincomecounties[,c("county","stat8share", "stat8label")]
industry04 <- lowincomecounties[,c("county","stat9share", "stat9label")]
industry05 <- lowincomecounties[,c("county","stat10share", "stat10label")]
industry06 <- lowincomecounties[,c("county","stat11share", "stat11label")]
industry07 <- lowincomecounties[,c("county","stat12share", "stat12label")]
industry08 <- lowincomecounties[,c("county","stat13share", "stat13label")]
industry09 <- lowincomecounties[,c("county","stat14share", "stat14label")]
industry10 <- lowincomecounties[,c("county","stat15share", "stat15label")]
industry11 <- lowincomecounties[,c("county","stat16share", "stat16label")]
industry12 <- lowincomecounties[,c("county","stat17share", "stat17label")]
industry13 <- lowincomecounties[,c("county","stat18share", "stat18label")]
industry14 <- lowincomecounties[,c("county","stat19share", "stat19label")]
industry15 <- lowincomecounties[,c("county","stat20share", "stat20label")]
industry16 <- lowincomecounties[,c("county","stat21share", "stat21label")]
industry17 <- lowincomecounties[,c("county","stat22share", "stat22label")]
industry18 <- lowincomecounties[,c("county","stat23share", "stat23label")]
industry19 <- lowincomecounties[,c("county","stat24share", "stat24label")]
industry20 <- lowincomecounties[,c("county","stat25share", "stat25label")]
names(industry01)[2]<-"share"
names(industry01)[3]<-"label"
names(industry02)[2]<-"share"
names(industry02)[3]<-"label"
names(industry03)[2]<-"share"
names(industry03)[3]<-"label"
names(industry04)[2]<-"share"
names(industry04)[3]<-"label"
names(industry05)[2]<-"share"
names(industry05)[3]<-"label"
names(industry06)[2]<-"share"
names(industry06)[3]<-"label"
names(industry07)[2]<-"share"
names(industry07)[3]<-"label"
names(industry08)[2]<-"share"
names(industry08)[3]<-"label"
names(industry09)[2]<-"share"
names(industry09)[3]<-"label"
names(industry10)[2]<-"share"
names(industry10)[3]<-"label"
names(industry11)[2]<-"share"
names(industry11)[3]<-"label"
names(industry12)[2]<-"share"
names(industry12)[3]<-"label"
names(industry13)[2]<-"share"
names(industry13)[3]<-"label"
names(industry14)[2]<-"share"
names(industry14)[3]<-"label"
names(industry15)[2]<-"share"
names(industry15)[3]<-"label"
names(industry16)[2]<-"share"
names(industry16)[3]<-"label"
names(industry17)[2]<-"share"
names(industry17)[3]<-"label"
names(industry18)[2]<-"share"
names(industry18)[3]<-"label"
names(industry19)[2]<-"share"
names(industry19)[3]<-"label"
names(industry20)[2]<-"share"
names(industry20)[3]<-"label"
allindustries <- rbind.fill(industry01, industry02, industry03, industry04, industry05, industry06, industry07, industry08, industry09, industry10, industry11, industry12, industry13, industry14, industry15, industry16, industry17, industry18, industry19, industry20)

palette <- c(palette1, palette2)
plot6 <- ggplot(allindustries,aes(x = county, y = share, fill = label, order = label)) +
  geom_bar(position = "stack",stat = "identity") +
  labs(x="Share of Jobs earning $1,250 per month or less", y="") +
  theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = palette, guide = guide_legend(reverse=TRUE))
print(plot6)
ggsave("industryshare.pdf", plot6, scale=2)

ORCountyEmploymentEarningsPayroll$yearQ <- paste(ORCountyEmploymentEarningsPayroll$year, "Q", ORCountyEmploymentEarningsPayroll$quarter, sep="")
ORCountyEmploymentEarningsPayroll$raceEthnicity <- paste(ORCountyEmploymentEarningsPayroll$race, ORCountyEmploymentEarningsPayroll$ethnicity, sep="")
ORCountyEmploymentEarningsPayroll$raceEthnicity_label <- paste(ORCountyEmploymentEarningsPayroll$race_label.value, ORCountyEmploymentEarningsPayroll$ethnicity_label.value)
splitIndustry <- split(ORCountyEmploymentEarningsPayroll, ORCountyEmploymentEarningsPayroll$industry)
timeindustry1 <- splitIndustry[[1]]
splitAllIndustriesCounty <- split(timeindustry1, timeindustry1$geography)
statewide <- splitAllIndustriesCounty[[1]]
baker <- splitAllIndustriesCounty[[2]]
splitYear <- split(statewide, statewide$year)

year2013 <- splitYear[[9]]
year2013 <- year2013[, c("geography", "geography_label.value", "industry", "industry_label.value", "race", "race_label.value", "ethnicity", "ethnicity_label.value", "raceEthnicity", "raceEthnicity_label", "year", "quarter", "EmpS", "EarnS", "Payroll")]
mean2013 <- ddply(year2013, c("geography", "geography_label.value", "industry", "industry_label.value", "race", "race_label.value", "ethnicity", "ethnicity_label.value", "raceEthnicity", "raceEthnicity_label", "year"), summarise, meanEmpS = mean(EmpS), meanEarnS = mean(EarnS), meanPayroll = mean(Payroll))
mean2013$meanPayroll <- format(mean2013$meanPayroll, scientific=FALSE)
mean2013$StableEmploymentShare <- mean2013$meanEmpS / mean2013[1,"meanEmpS"]
palette21 <- c(palette, "#000000")
plot7 <- ggplot(mean2013) + geom_bar(aes(reorder(raceEthnicity,StableEmploymentShare), StableEmploymentShare, fill=raceEthnicity_label, group=raceEthnicity), stat="identity") + scale_fill_manual(values=palette21) + labs(x="", y="Share of Stable Employment Statewide 2013") + theme(legend.title=element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), axis.text.x = element_blank()) + scale_y_continuous(labels = percent_format()) + geom_text(aes(reorder(raceEthnicity,StableEmploymentShare), StableEmploymentShare, label=sprintf("%1.1f%%", 100*StableEmploymentShare), angle=45, vjust=-.125, hjust=.1))
print(plot7)
ggsave("RaceShareStableEmploymentStatewide.pdf", plot7, scale=2)
