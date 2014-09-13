### Plot 2 for Course Project 2

### Load the data
NEIdata <- readRDS("summarySCC_PM25.rds")
sourceNames <- readRDS("Source_Classification_Code.rds")

### Keep only the necessary variables and clean up the structure
sourceNames <- sourceNames[,c(1,3,4,7:10)]
NEIdata <- NEIdata[,-3]
NEIdata[c(1,2,4)] <- lapply(NEIdata[c(1,2,4)], factor)
NEIdata$year <- factor(NEIdata$year, ordered=TRUE)

### Calculate Emission totals for Baltimore
totals <- aggregate(Emissions~year,data=NEIdata[NEIdata$fips=="24510",],FUN=sum)

### Build and Save plot
png(filename = "plot2.png", width = 480, height = 480, units = "px", pointsize = 12)

      plot(totals,axes=FALSE,ylab=expression("Total PM"[2.5]*" Emissions (kilotons)"),
            main=expression("Baltimore City PM"[2.5]*" Emissions by Year"))
      
      ##Find values for axis tickmarks 
      minEm = round(min(totals$Emissions))
      maxEm = round(max(totals$Emissions))
      midEm = round((minEm+maxEm)/2)
      
      axis(2,at=c(0,minEm,midEm,maxEm), labels=c(0,minEm,midEm,maxEm))
      axis(1,at = totals$year, labels=totals$year,lwd=0,lwd.ticks=1)

dev.off()