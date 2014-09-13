### Plot 3 for Course Project 2

### Load the data
library(ggplot2)

NEIdata <- readRDS("summarySCC_PM25.rds")
sourceNames <- readRDS("Source_Classification_Code.rds")

### Keep only the necessary variables and clean up the structure
sourceNames <- sourceNames[,c(1,3,4,7:10)]
NEIdata <- NEIdata[,-3]
NEIdata[c(1,2,4)] <- lapply(NEIdata[c(1,2,4)], factor)
NEIdata$year <- factor(NEIdata$year, ordered=TRUE)

### Subset the data for Baltimore according to fips code
BmoreNEI <- NEIdata[NEIdata$fips=="24510",]

### Build and Save plot
png(filename = "plot3.png", width = 600, height = 600, units = "px", pointsize = 12)
      
      ## Reorder type factors for presentation
      BmoreNEI$type <- factor(BmoreNEI$type, levels=c("ON-ROAD","NON-ROAD","POINT","NONPOINT"))
      
      print(qplot(year, Emissions, data=BmoreNEI,facets= .~type, geom="bar", stat="identity",
            ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
            main=expression("Baltimore City PM"[2.5]*" Emissions by Source Type"))+
      theme(plot.title = element_text(vjust=2)))

dev.off()