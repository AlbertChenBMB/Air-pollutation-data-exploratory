NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")
#Q1.Have total emissions from PM2.5 decreased in 
#   the United States from 1999 to 2008? 
#   Using the base plotting system, make a plot 
#   showing the total PM2.5 emission from all sources
#   for each of the years 1999, 2002, 2005, and 2008.
emission<-NEI[,c(4,6)]
PM2.5_emission<-group_by(emission,year) %>%
  summarise(sum(Emissions))
png("plot1.png",width = 680,height = 480)
plot(y=PM2.5_emission$`sum(Emissions)`,
     x=(PM2.5_emission$year),type = "l",
     xlab = "Year",
     ylab = "Emission",
     main = "Emission of PM2.5 change in every year")
dev.off()
#Q2.Have total emissions from PM2.5 decreased in the 
#   Baltimore City, Maryland (fips == "24510") from 1999 
#   to 2008? Use the base plotting system to make a plot 
#   answering this question.
emission_Baltimor<-filter(NEI,fips=="24510")
PM2.5_emission_B<-group_by(emission_Baltimor,year) %>%
  summarise(sum(Emissions))
png("plot2.png",width = 680,height = 480)
plot(y=PM2.5_emission_B$`sum(Emissions)`,
     x=(PM2.5_emission$year),type = "l",
     xlab = "Year",
     ylab = "Emission",
     main = "Emission of PM2.5 change in every year")
dev.off()
#Q3 Of the four types of sources indicated by the type (point, 
#   nonpoint, onroad, nonroad) variable, which of these four sources 
#   have seen decreases in emissions from 1999–2008 for Baltimore City? 
#   Which have seen increases in emissions from 1999–2008? Use the 
#   ggplot2 plotting system to make a plot answer this question.
library(dplyr)
library(ggplot2)
Baltimor<-filter(NEI,fips=="24510")
G_Baltimor<-aggregate(Emissions ~ year + type, Baltimor, sum)
png("plot3.png",width = 680,height = 480)
ggplot(data= G_Baltimor, aes(x=year,y=Emissions,color=type))+
  geom_line()+geom_point()
dev.off()
#   Q4 Across the United States, how have emissions from coal combustion-related
#   sources changed from 1999–2008?
NEISCC<-merge(NEI,SCC,by="SCC")
coal_name<-grepl("coal",NEISCC$Short.Name)
coal<-NEISCC[coal_name,]
G_coal<-aggregate(Emissions~year,coal,sum)
png("plot4.png",width = 680,height = 480)
ggplot(data= G_coal, aes(x=year,y=Emissions))+
  geom_line()+geom_point()+
  ggtitle("Emissions from coal changed from 1999–2008 in United States")
dev.off()
# Q5 How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?

emission_Baltimor<-filter(NEI,fips=="24510")
motor<-emission_Baltimor[emission_Baltimor$type=="ON-ROAD",]

motor_emission_B<-group_by(motor,year) %>%
  summarise(sum(Emissions))
names(motor_emission_B)<-c("year","Emissions")
png("plot5.png",width = 680,height = 480)
ggplot(data= motor_emission_B, aes(x=year,y=Emissions))+
  geom_line()+geom_point()+
  ggtitle("Emissions from motor vehicle changed in Baltimore City")
dev.off()
# Q6 Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == 06037). 
# Which city has seen greater changes over time in motor vehicle emissions?

emission<-filter(NEI,fips=="24510"|fips=="06037")
motor<-emission_Baltimor[emission$type=="ON-ROAD",]
#renames of fips to city name
city<-c("24510"="Baltimore","06037"="California")
motor$fips<-city[motor$fips]
motor_emission<-aggregate(Emissions~ year+fips,motor,sum)

png("plot6.png",width = 680,height = 480)
ggplot(data= motor_emission, aes(x=year,y=Emissions,color=fips))+
  geom_line()+geom_point()+
ggtitle("Compare Total Emissions from motor vehicle between Baltimore City and California")
dev.off()
