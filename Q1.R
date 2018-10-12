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