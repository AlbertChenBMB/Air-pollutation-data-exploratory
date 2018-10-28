#some other solutions I think great!
# reading and exploring NEI data - National Emmissions Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# aggregating NEI emmissions by year
yearly_emmissions <- aggregate(Emissions ~ year, NEI, sum)

# plot1.png
png('plot1.png')
cols <- c("aquamarine", "chartreuse", "cyan", "deepskyblue")
barplot(height=yearly_emmissions$Emissions/1000, names.arg=yearly_emmissions$year, xlab="Year", ylab=expression('Aggregated Emission'),main=expression('Aggregated PM'[2.5]*' Emmissions by Year'), col = cols)
dev.off()
#
# forming Baltimore data which will be NEI subset
baltdata <- NEI[NEI$fips=="24510", ]

# Baltimore yearly emmisisons data
baltYearEm <- aggregate(Emissions ~ year, baltdata, sum)

# plot2.png
png('plot2.png')
cols <- c("aquamarine", "chartreuse", "cyan", "deepskyblue")
barplot(height=baltYearEm$Emissions/1000, names.arg=baltYearEm$year, xlab="Year", 
        ylab=expression('Aggregated Emission'),main=expression('Baltimore Aggregated PM'[2.5]*' Emmissions by Year'), 
        col = cols)
dev.off()

#plot 4# have problem
combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
combustion.coal.sources<-SCC[combustion.coal,]
emissions.coal.combustion<-NEI[(NEI$SCC %in% combustion.coal.sources$SCC), ]
library(dplyr)
emissions.coal.related<-summarise(group_by(emissions.coal.combustion, year), Emissions = sum(Emissions))
png(filename = "plot4.png")
ggplot(emissions.coal.related, aes(x=factor(year), y=Emissions/1000,fill=year, label = round(Emissions/1000,2))) 
  +geom_bar(stat="identity")
+ xlab("year") 
  + ylab(expression("total PM"[2.5]*" emissions in kilotons")) 
  + ggtitle("Emissions from coal combustion-related sources in kilotons")
  + geom_label(aes(fill = year),colour = "white", fontface = "bold")
dev.off()
