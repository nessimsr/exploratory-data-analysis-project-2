# Course: Exploratory Data Analysis

## Week 4
getwd()
setwd(".\\Exploratory_Data_Analysis")

### Course project 2
#### Plot 1

#load the librarys

library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

# read the data tables

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#### Question #1

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


head(NEI)
str(NEI)

head(SCC)
str(SCC)

annual <- NEI %>% group_by(year) %>% 
  filter(year == 1999|2002|2005|2008) %>% 
  summarize(Annual.Total = sum(Emissions));
pts <- pretty(annual$Annual.Total/1000000);
yrs <- c(1999,2002,2005,2008)
plot(annual$year, annual$Annual.Total/1000000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in the United States"));
axis(1, at = yrs, labels = paste(yrs));
axis(2, at = pts, labels = paste(pts, "M", sep = ""));
box()

png("plot1.png", width=480, height=480)