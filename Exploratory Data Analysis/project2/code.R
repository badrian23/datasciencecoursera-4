#QUESTION1
setwd("/Users/egorov/datasciencecoursera/Exploratory Data Analysis/project2/data")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(data.table)

NEI = data.table(NEI)
SCC = data.table(SCC)

func <- with(NEI, aggregate(Emissions, by = list(year), sum))
setwd("/Users/egorov/datasciencecoursera/Exploratory Data Analysis/project2/")
png("plot1.png", width = 480, height = 480)
plot(func, type = "b", pch = 25, col = "blue", ylab = "Emissions", xlab = "Year", main = "Annual Emissions")
dev.off()


#QUESTION2
balt1 <- NEI[which(NEI$fips == "24510"), ]
balt2 <- with(balt1, aggregate(Emissions, by = list(year), sum))
colnames(balt2) <- c("year", "Emissions")
png("images/plot2.png", width = 480, height = 480)
plot(balt2$year, balt2$Emissions, type = "b", pch = 25, col = "red",ylab = "Emissions", xlab = "Year", main = "Baltimore Emissions")
dev.off()

#QUESTION3
balt3 <- ddply(balt1, .(type, year), summarize, Emissions = sum(Emissions))
library(plyr)
library(ggplot2)
library(grid)
library(scales)
png("images/plot3.png", width = 480, height = 480)
qplot(year, Emissions, data = balt3, group = type,color = type, geom = c("point", "line"), ylab = expression("Total Emissions of PM"[2.5]),xlab = "Year", main = "Total Emissions in U.S. by Type of Pollutant")
dev.off()

#QUESTION4
coal1 = SCC[grep("Coal", SCC.Level.Three), SCC]
coal2 = NEI[SCC %in% coal1, sum(Emissions), by = "year"]
setnames(coal2, c("year", "Emissions"))
png("images/plot4.png", width = 480, height = 480)
g = ggplot(coal2, aes(year, Emissions))
g + geom_point(color = "black") + geom_line(color = "blue") + labs(x = "Year") +labs(y = expression("Total Emissions of PM"[2.5])) + labs(title = "Emissions from Coal Combustion for the US")
dev.off()

#QUESTION5
motor1 = SCC[grep("[Mm]obile|[Vv]ehicles", EI.Sector), SCC]
motor2 = NEI[SCC %in% motor1, sum(Emissions), by = c("year", "fips")][fips == "24510"]
setnames(motor2, c("year", "fips", "Emissions"))
png("images/plot5.png", width = 480, height = 480)
g = ggplot(motor2, aes(year, Emissions))
g + geom_point(color = "black") + geom_line(color = "red") + labs(x = "Year") +labs(y = expression("Total Emissions of PM"[2.5])) +labs(title = "Total Emissions from Motor Vehicle Sources in Baltimore City")
dev.off()

#QUESTION6
balt4 <- NEI[(NEI$fips=="24510"), ]
balt4 <- aggregate(Emissions ~ year, data = balt4, FUN = sum)
losangeles <- NEI[(NEI$fips=="06037"),]
losangeles <- aggregate(Emissions ~ year, data = losangeles, FUN = sum)
balt4$County <- "Baltimore"
losangeles$County <- "Los Angeles"
ambasciudades <- rbind(balt4, losangeles)
fmt <- function(){
	f <- function(x) as.character(round(x,2))
	f
}
png("images/plot6.png", width = 480, height = 480)
ggplot(ambasciudades, aes(x=factor(year), y=Emissions, fill=County)) +geom_bar(aes(fill = County), position = "dodge", stat="identity") + labs(x = "Year") + labs(y = expression("Total Emissions (in log scale) of PM"[2.5])) + xlab("year") +ggtitle(expression("Motor vehicle emission in Baltimore and Los Angeles")) +scale_y_continuous(trans = log_trans(), labels = fmt())
dev.off()



