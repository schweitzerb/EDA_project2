### Plot 1 for Course Project 2

### Load the data
NEIdata <- readRDS("summarySCC_PM25.rds")
sourceNames <- readRDS("Source_Classification_Code.rds")

### Keep only the necessary variables and clean up the structure
sourceNames <- sourceNames[,c(1,3,4,7:10)]
NEIdata <- NEIdata[,-3]
NEIdata[c(1,2,4)] <- lapply(NEIdata[c(1,2,4)], factor)
NEIdata$year <- factor(NEIdata$year, ordered=TRUE)

### Calculate Emission totals
totals <- aggregate(Emissions~year,data=NEIdata,FUN=sum)

### Build and Save plot
png(filename = "plot1.png", width = 480, height = 480, units = "px", pointsize = 12)

      plot(totals,axes=FALSE,ylab=expression("Total PM"[2.5]*" Emissions (kilotons)"),
           main=expression("U.S. PM"[2.5]*" Emissions by Year"))
      axis(2,at=c(0,3500000,5250000,7000000,8750000), labels=c(0,"3500","5250","7000","8750"))
      axis(1,at = totals$year, labels=totals$year,lwd=0,lwd.ticks=1)

dev.off()