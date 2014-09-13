### Plot 5 for Course Project 2
#
# For the purpose of the project, I interpreted "motor vehicles sources" by the 
# definition found on wikiepdia (http://en.wikipedia.org/wiki/Motor_vehicle)
# and filtered the sources by the "On-Road" sector in the SCC file.  
### 
library(ggplot2)


### Load the data
NEIdata <- readRDS("summarySCC_PM25.rds")
sourceNames <- readRDS("Source_Classification_Code.rds")

### Keep only the necessary variables and clean up the structure
sourceNames <- sourceNames[,c(1,3,4,7:10)]
NEIdata <- NEIdata[,-3]
NEIdata[c(1,2,4)] <- lapply(NEIdata[c(1,2,4)], factor)
NEIdata$year <- factor(NEIdata$year, ordered=TRUE)

### Subset the data for Baltimore & motor vehicles
BmoreNEI <- NEIdata[NEIdata$fips=="24510",]
motorcodes <- subset(sourceNames,grepl("On-Road",sourceNames$EI.Sector))
BmoreNEI <- subset(BmoreNEI,BmoreNEI$SCC %in% motorcodes$SCC)

### Build and Save plot
png(filename = "plot5.png", width = 600, height = 600, units = "px", pointsize = 12)
      
      options(scipen="12") ##don't want scientifc notification for presentation
      print(qplot(year,Emissions,data=BmoreNEI,geom="bar",stat="identity",
            ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
            main=expression("Baltimore City PM"[2.5]*" Emissions from Motor Vehicles by Year"))
      +theme(plot.title = element_text(vjust=2)))

dev.off()