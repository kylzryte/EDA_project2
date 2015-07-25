library(ggplot2)
library(reshape2)

path.data <- file.path(getwd(),"exdata-data-NEI_data")

# note:
# fips - five-digit number indicating US county
# scc - the name of the source as indicated by a digit-string
# pollutant - a string indicating the polluatant
# emmissions - amount of PM2.5 emitted, in tons
# type - the type of source
# year - the year of emissions recoreded

NEI <- readRDS(file.path(path.data,"summarySCC_PM25.rds"))
SCC <- readRDS(file.path(path.data,"Source_Classification_Code.rds"))


# Q1: Have total emissions from PM2.5 decreasd in the US from 1999 to 2008?
# use base plotting system

total.emissions <- with(NEI, aggregate(Emissions, by=list(year), sum))
opt <- options()
options(scipen=10)
plot(total.emissions, type="l", pch=18, col="black", lwd=3, ylab=" Total Emissions",
     xlab="Year", main="Annual US Total Emissions", 
     ylim=c(min(total.emissions$x), max(total.emissions$x)),
     sub="Data from: National Emissions Inventory (NEI)", axes=FALSE)
points(total.emissions, type="b", pch=21, col="black", bg="black")
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2)
box()



# Q2: Have total emissions from PM2.5 decreased in Baltimore City, Maryland from
# 1999 t0 2008. Use base plotting system

baltimore <- NEI[(NEI$fips=="24510"),]
baltimore.total.emissions <- with(baltimore, 
                                  aggregate(Emissions, by=list(year), sum))
plot(baltimore.total.emissions, type="l", pch=18, col="black", lwd=3, 
     ylab=" Total Emissions",
     xlab="Year", main="Annual Baltimore Total Emissions", 
     ylim=c(min(baltimore.total.emissions$x), max(baltimore.total.emissions$x)),
     sub="Data from: National Emissions Inventory (NEI)", axes=FALSE)
points(baltimore.total.emissions, type="b", pch=21, col="black", bg="black")
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2)
box()

# Q3: Of the four tpes of sources indicated by the type variable, which of these
# four sources have seen decreases in emissions from 1999-2008 for Baltimore
# City? Which have seen increases in emissions from 1999-2008.
# Use ggplot.

# Using the melt function from the reshape2 package and dcast to sum emissions
# by year and type

baltimore.2 <- melt(baltimore, id=c("year", "type"), measure.vars="Emissions")
baltimore.2 <- dcast(baltimore.2, type + year ~ variable, sum)

ggplot(data=baltimore.2, aes(x=year, y=Emissions, group=type, color=type)) +
        geom_line() + geom_point(size=4, shape=21, aes(color=type, fill=type)) +
        xlab("Year") + ylab("Emissions") + ggtitle("Baltimore Emissions by Type")

# Across the US has emissions from coal combustion-related sources changed
# from 1999-2008?

subset.SCC <- grep("Coal", SCC$Short.Name, ignore.case=FALSE)
coal.codes <- SCC[subset.SCC, 1]
coal.data <- subset(NEI, NEI$SCC %in% coal.codes)
coal.total.emissions <- with(coal.data, 
                                  aggregate(Emissions, by=list(year), sum))
plot(coal.total.emissions, type="l", pch=18, col="black", lwd=3, 
     ylab=" Total Emissions",
     xlab="Year", main="Annual Coal Total Emissions", 
     ylim=c(min(coal.total.emissions$x), max(coal.total.emissions$x)),
     sub="Data from: National Emissions Inventory (NEI)", axes=FALSE)
points(coal.total.emissions, type="b", pch=21, col="black", bg="black")
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2)
box()

# Have emissions from motor vehicle sources changed in 1999-2008 in 
# baltimore city?

subset.SCC <- grep("Vehicle", SCC$EI.Sector, ignore.case=TRUE)
vehicle.codes <- SCC[subset.SCC, 1]
motor.data.baltimore <- subset(baltimore, baltimore$SCC %in% vehicle.codes)
motor.baltimore.total <- with(motor.data.baltimore, aggregate(Emissions,
                                                              by=list(year), sum))
plot(motor.baltimore.total, type="l", pch=18, col="black", lwd=3, 
     ylab=" Total Emissions",
     xlab="Year", main="Annual Baltimore Total Vehicle Emissions", 
     ylim=c(min(motor.baltimore.total$x), max(motor.baltimore.total$x)),
     sub="Data from: National Emissions Inventory (NEI)", axes=FALSE)
points(motor.baltimore.total, type="b", pch=21, col="black", bg="black")
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2)
box()

# Compare Emission from motor vehical sources in LA county, California

losangeles <- NEI[(NEI$fips=="06037"),]
subset.SCC <- grep("Vehicle", SCC$EI.Sector, ignore.case=TRUE)
vehicle.codes <- SCC[subset.SCC, 1]
motor.data.baltimore <- subset(baltimore, baltimore$SCC %in% vehicle.codes)
motor.data.la <- subset(losangeles, losangeles$SCC %in% vehicle.codes)
motor.baltimore.total <- with(motor.data.baltimore, aggregate(Emissions,
                                                              by=list(year), sum))
motor.losangeles.total <- with(motor.data.la, aggregate(Emissions,
                                                              by=list(year), sum))

colnames(motor.losangeles.total) <- c("year","Emissions")
colnames(motor.baltimore.total) <- c("year", "Emissions")

motor.losangeles.total$city <- rep("Los Angeles", length(motor.losangeles.total$year))
motor.baltimore.total$city <- rep("Baltimore", length(motor.losangeles.total$year))

data <- rbind(motor.losangeles.total,motor.baltimore.total)

data$city <- as.factor(data$city)
str(data)

ggplot(data=data, aes(x=year, y=Emissions, group=city, color=city)) +
        geom_line() + geom_point(size=4, shape=21, aes(color=city, fill=city)) +
        xlab("Year") + ylab("Emissions") + 
        ggtitle("Motor Vehicle Emissions - Los Angeles and Baltimore: 1999-2008")
