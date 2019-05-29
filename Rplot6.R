if(!exists("NEI")){NEI <- readRDS("./data/summarySCC_PM25.rds")}
if(!exists("SCC")){SCC <- readRDS("./data/Source_Classification_Code.rds")}  
baltmary.emissions<-summarise(group_by(filter(NEI, fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
LAcal.emissions<-summarise(group_by(filter(NEI, fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

baltmary.emissions$County <- "Baltimore City, MD"
LAcal.emissions$County <- "Los Angeles County, CA"
bemissions <- rbind(baltmary.emissions, LAcal.emissions)

require(ggplot2)
ggplot(bemissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions, 2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("Total PM 2.5 emissions in tons")) + 
  xlab("Year") +
  ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons"))+
  geom_label(aes(fill = County),colour = "black")
