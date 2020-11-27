# Course: Exploratory Data Analysis

## Week 4
getwd()
setwd(".\\Exploratory_Data_Analysis")

### Course project 2
#### Plot 2

#load the librarys

library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

# read the data tables

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#### Question #2

# Have total emissions from PM2.5 decreased in Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question

baltimore <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarize(Annual.Total = sum(Emissions));
baltimore.pts <- pretty(baltimore$Annual.Total/1000);
plot(baltimore$year, baltimore$Annual.Total/1000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore"));
axis(1, at = c(1999,2002,2005,2008))
axis(2, at = baltimore.pts, labels = paste(baltimore.pts, "K", sep = ""));
box()

png("plot2.png", width=480, height=480)|