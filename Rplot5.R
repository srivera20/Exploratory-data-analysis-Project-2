if(!exists("NEI")){NEI <- readRDS("./data/summarySCC_PM25.rds")}
if(!exists("SCC")){SCC <- readRDS("./data/Source_Classification_Code.rds")}  
scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; 
scc.vehicles.list <- unique(scc.vehicles$SCC); 
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list);
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); 
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); 
nei.vehicles <- nei.vehicles %>% group_by(fips, year, SCC.Level.Two) %>% summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% group_by(fips, year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
  geom_bar(stat = "identity") +
  facet_grid(fips ~ SCC.Level.Two) + 
  xlab("Year") +
  ylab(expression("Total Tons of PM 2.5 Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM 2.5 Emissions from Motor Vehicle Sources", paste("in Baltimore City, MD and Los Angeles County, CA")))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)
