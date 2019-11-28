setwd("c:/Users/REACH-IRQ-GIS/Documents/201911 WASH RS")
library(ggplot2)
library(plotly)

yearly <- read.csv("data/yearly_whole_timeseries_govs_dist.csv")
yearly$water <- yearly$water * 0.03 ^ 2

yearly_aggr <- yearly %>% group_by(year) %>% summarise(sum(water))
yearly_gov <- yearly %>% group_by(governorate, year) %>% summarise(water = sum(water))

NA_years <- c(1988, 1989, 1993, 1994, 1996, 1997)
yearly_gov$water[which(yearly_gov$year %in% NA_years)] <- NA

p <- ggplot(as.data.frame(yearly_gov), aes(x = year, y = water, group = governorate)) + 
  geom_line(aes(color = governorate)) + geom_point() +
  ggtitle("Iraq yearly surface water area by Governorate") + ylab("Surface water (km2)")
theme(legend.position = "none") 
gp <- ggplotly(p)


Sys.setenv("plotly_username"="boukepieter")
Sys.setenv("plotly_api_key"="???")
api_create(gp, filename = "Iraq yearly surface water area by Governorate")
