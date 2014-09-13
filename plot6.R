### Plot 6 for Course Project 2
#
# For the purpose of the project, I interpreted "motor vehicles sources" by the 
# definition found on wikiepdia (http://en.wikipedia.org/wiki/Motor_vehicle)
# and filtered the sources by the "On-Road" sector in the SCC file.  
### 
library(ggplot2)
library(grid)
library(gridExtra)

### Load the data
NEIdata <- readRDS("summarySCC_PM25.rds")
sourceNames <- readRDS("Source_Classification_Code.rds")

### Keep only the necessary variables and clean up the structure
sourceNames <- sourceNames[,c(1,3,4,7:10)]
NEIdata <- NEIdata[,-3]
NEIdata[c(1,2,4)] <- lapply(NEIdata[c(1,2,4)], factor)
NEIdata$year <- factor(NEIdata$year, ordered=TRUE)

### Subset the data for Baltimore, LA County, and motor vehicles
BmoreLA <- NEIdata[NEIdata$fips %in% c("24510","06037"),]
motorcodes <- subset(sourceNames,grepl("On-Road",sourceNames$EI.Sector))
BmoreLA <- subset(BmoreLA,BmoreLA$SCC %in% motorcodes$SCC)
BmoreLA$fips <- factor(BmoreLA$fips)      ##Clean up the factors
levels(BmoreLA$fips) <- c("LA County","Baltimore City")     ##Relabel the levels
names(BmoreLA)[1] <- "city"               ##Relabel variable

###Calculate Data to compare Baltimore and LA
CityTotals <- aggregate(Emissions~year+city,data=BmoreLA,FUN=sum) ##Aggregate by year and location
      ##Calculate Year over Year difference for each city
CityTotals$yoy <- ave(CityTotals$Emissions, CityTotals$city, FUN = function(x) c(NA, diff(x)))     
      ##Calculate the amount of change relative to total emissions
CityTotals$perc_Change <- c(NA,CityTotals$yoy[-1]/CityTotals$Emissions[-length(CityTotals$Emissions)]*100)                                     
CityTotals$perc_Change <- round(as.numeric(CityTotals$perc_Change),2) ##Round for presentation
CityTotals$timespan <- c("NA","'99-'02","'02-'05","'05-'08")      ##Add Timespans
CityTotals$timespan <- factor(CityTotals$timespan,levels=c("'99-'02","'02-'05","'05-'08"))

### Build and Save plot
png(filename = "plot6.png", width = 480, height = 920, units = "px", pointsize = 12)
      
      ## Using 3 different visualizarion to compare the two cities

      ## Total emissions
      plot6.1 <- qplot(year,Emissions,data=CityTotals,facets=.~city,geom="bar",stat="identity",
                  ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
                  xlab="Year",
                  main=expression("PM"[2.5]*" Emissions from Motor Vehicles by Year"))+
            theme(plot.title = element_text(vjust=2,hjust=0),plot.margin = unit(c(3,1,0,1),"lines"))
      
      ## Amount of change in emission 
      plot6.2 <- qplot(timespan,yoy,data=na.omit(CityTotals),facets=.~city,geom="bar",stat="identity",
                  ylab=expression("Change in PM"[2.5]*" Emissions (tons)"),
                  xlab="Time Frame",
                  main=expression("Absolute Change in PM"[2.5]*" Emissions from Motor Vehicles"))+
            theme(plot.title = element_text(vjust=2,hjust=0),plot.margin = unit(c(2,1,0,1),"lines"))
      
      ## Amount of change in percent relative to total emissions
      plot6.3 <- qplot(timespan,perc_Change,data=na.omit(CityTotals),facets=.~city,geom="bar",stat="identity",
                       ylab=expression("% Change in PM"[2.5]*" Emissions"),
                       xlab="Time Frame",
                       main=expression("Relative Change in PM"[2.5]*" Emissions from Motor Vehicles"))+
            theme(plot.title = element_text(vjust=2,hjust=0),plot.margin = unit(c(2,1,1,1),"lines"))

      ## Put it all in one plot
      grid.arrange(plot6.1,plot6.2,plot6.3,ncol=1,main=textGrob(expression(
            "Baltimore/LA Comparison of PM"[2.5]*" Emissions"), gp=gpar(cex=1.5), just="top"))

dev.off()