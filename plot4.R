### Plot 4 for Course Project 2
#
# For the purpose of the project, I interpreted "coal combustion-related" as all
# sources that included the words coal and combustion in the SCC Short.Name
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

### Find all coal combustion related sources in the SCC file and filter the data apropriately
coalcodes <- subset(sourceNames,grepl("[C|c]oal",sourceNames$Short.Name),select=1:2)
coalcodes <- subset(coalcodes,grepl("(C|c)omb",coalcodes$Short.Name))
coalData <- subset(NEIdata,NEIdata$SCC %in% coalcodes$SCC)

### Build and Save plot
png(filename = "plot4.png", width = 480, height = 480, units = "px", pointsize = 12)
      
      options(scipen="12")
      print(qplot(year,Emissions,data=coalData,geom="bar",stat="identity",
                  ylab=expression("Total PM"[2.5]*" Emissions (tons)"),
                  main=expression("U.S. PM"[2.5]*" Emissions from Coal Combustion Sources by Year"))
            +theme(plot.title = element_text(vjust=2)))

dev.off()