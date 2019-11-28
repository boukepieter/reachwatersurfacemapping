setwd("c:/Users/REACH-IRQ-GIS/Documents/201911 WASH RS")
Sys.setenv("plotly_username"="boukepieter")
Sys.setenv("plotly_api_key"="???")
library(ggplot2)
library(plotly)
library(rgdal)
library(raster)
library(trend)
library(forecast)

shapes <- readOGR("data/countries_influence.shp", "countries_influence", stringsAsFactors = F)
monthly <- read.csv("data/precipitation_watershed_whole_timeseries.csv", stringsAsFactors = F)
monthly <- monthly[,-c(1,6)]
areas <- area(shapes)
monthly$water <- apply(monthly, 1, FUN=function(x){
  as.numeric(x["precipitation"]) / 1000 * areas[match(x["name"], shapes$COUNTRY_NA)]
})
monthly$water <- round(monthly$water / 1000000, 1)
yearly_aggr <- monthly %>% group_by(year, name) %>% summarise(precipitation = sum(precipitation), water = sum(water))

monthly$date <- as.Date(sprintf("01-%02d-%d", monthly$month, monthly$year), format="%d-%m-%Y")
monthly_aggr <- monthly %>% group_by(date) %>% summarise(precipitation = sum(precipitation), water = sum(water))
monthly_aggr_ts <- ts(monthly_aggr$precipitation, start = c(monthly$year[1], 1),frequency = 12)
components <- decompose(monthly_aggr_ts, type= "additive")
plot(components)

# about sen's slope for rainfall: https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/asl.932
sub <- monthly_aggr %>% filter(date < as.Date("2018-01-01"))
sub_ts <- ts(sub$precipitation, start = c(monthly$year[1], 1),frequency = 12)
sens.slope(monthly_aggr_ts, conf.level = 0.95)
sens.slope(sub_ts, conf.level = 0.95)
mk.test(monthly_aggr_ts)
mk.test(sub_ts)

# Lineplot per month
p <- ggplot(as.data.frame(monthly_aggr), aes(x = date, y = precipitation)) + 
  geom_line() + geom_point() +
  ggtitle("Average precipitation (in mm) per month in the Euphrates/Tigris watershed") + 
  ylab("Precipitation (mm)")
gp <- ggplotly(p)

api_create(gp, filename = "Precipitation in the Euphrates-Tigris watershed by country")

# Lineplot
p <- ggplot(as.data.frame(yearly_aggr), aes(x = year, y = water, group = name)) + 
  geom_line(aes(color = name)) + geom_point() +
  ggtitle("Precipitation (in billion m3) per contributing country in the Euphrates/Tigris watershed") + 
  ylab("Precipitation (1*10^9 m3)")
theme(legend.position = "none") 
gp <- ggplotly(p)

api_create(gp, filename = "Precipitation in the Euphrates-Tigris watershed by country")

# Lineplot in mm
p <- ggplot(as.data.frame(yearly_aggr), aes(x = year, y = precipitation, group = name)) + 
  geom_line(aes(color = name)) + geom_point() +
  ggtitle("Precipitation (in mm) per contributing country in the Euphrates/Tigris watershed") + 
  ylab("Precipitation (mm)")
theme(legend.position = "none") 
gp <- ggplotly(p)

api_create(gp, filename = "Precipitation in mm in the Euphrates-Tigris watershed by country")

# Barplot
p <- ggplot(as.data.frame(yearly_aggr), aes(x = year, y = water, fill = name)) + 
  geom_bar(stat="identity") +
  ggtitle("Precipitation (in billion m3) per contributing country in the Euphrates/Tigris watershed") + 
  ylab("Precipitation (1*10^9 m3)")
theme(legend.position = "none") 
gp <- ggplotly(p)

api_create(gp, filename = "Precipitation in the Euphrates-Tigris watershed by country - barchart")

# transformed to percentage
yearly_total <- yearly_aggr %>% group_by(year) %>% summarise(total_water = sum(water))
yearly_aggr$percentage <- apply(yearly_aggr, 1, FUN = function(x){
  round(as.numeric(x["water"]) / yearly_total$total_water[match(x["year"], yearly_total$year)] * 100, 1)
})

p <- ggplot(as.data.frame(yearly_aggr), aes(x = year, y = percentage, fill = name)) + 
  geom_bar(stat="identity") +
  ggtitle("Precipitation (percentage of total) per contributing country in the Euphrates/Tigris watershed") + 
  ylab("Percentage of precipitation contribution")
theme(legend.position = "none") 
gp <- ggplotly(p)

api_create(gp, filename = "Percentage of precipitation in the Euphrates-Tigris watershed by country - barchart")
