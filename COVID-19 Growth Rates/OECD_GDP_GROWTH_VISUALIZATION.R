#install.packages(c("plotly", "OECD", "plyr", "tidyverse", "ggplot2", "gganimate", "ggthemes", "transformr", "scales", "gifski", "zoo", "lubridate", "png", "RColorBrewer"))
require(OECD)
library(RMariaDB)
library(tidyverse)
library(plyr)
# library(dplyr)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(transformr)
library(scales)
library(gifski)
library(zoo)
library(lubridate)
library(png)
library(RColorBrewer)
# devtools::install_github('thomasp85/gganimate')
# devtools::install_github("ropensci/plotly")


# Get data
dataset_list <- get_datasets()
search_dataset("Quarterly", data = dataset_list)
dataset <- "QNA"
dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
df <- get_dataset("QNA", 
                  filter = c(list(slice_head(dstruc$LOCATION, n=37)$id),
                             "B1_GE", 
                             "GPSA", 
                             "Q"), 
                  start_time = 2006, 
                  end_time = 2020)

# Clean data
rawdata <- within(df, rm(SUBJECT, MEASURE, FREQUENCY, TIME_FORMAT, UNIT, POWERCODE, OBS_STATUS))
names(rawdata)[names(rawdata) == "LOCATION"] <- "COUNTRY"
names(rawdata)[names(rawdata) == "obsTime"] <- "QUARTER"
names(rawdata)[names(rawdata) == "obsValue"] <- "GROWTH RATE"

rawdata$QUARTER <- as.yearqtr(rawdata$QUARTER, format='%Y-Q%q')
rawdata$YEAR <- year(rawdata$QUARTER)

# Split data into OECD/US
USdata <- subset(rawdata, rawdata$COUNTRY == "USA")
OECDdata <- rawdata[rawdata$COUNTRY != "USA", ]


# Label data by region
OECDdata$REGION <- OECDdata$COUNTRY

OECDdata$REGION[OECDdata$REGION == "JPN"] <- "Asia"
OECDdata$REGION[OECDdata$REGION == "KOR"] <- "Asia"

OECDdata$REGION[OECDdata$REGION == "AUS"] <- "Pacific Islands"
OECDdata$REGION[OECDdata$REGION == "NZL"] <- "Pacific Islands"

OECDdata$REGION[OECDdata$REGION == "ISR"] <- "Middle East"
OECDdata$REGION[OECDdata$REGION == "TUR"] <- "Middle East"

OECDdata$REGION[OECDdata$REGION == "CAN"] <- "North America"
OECDdata$REGION[OECDdata$REGION == "MEX"] <- "North America"

OECDdata$REGION[OECDdata$REGION == "COL"] <- "South America"
OECDdata$REGION[OECDdata$REGION == "CHL"] <- "South America"

OECDdata$REGION[OECDdata$REGION == "CZE"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "HUN"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "LVA"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "LTU"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "POL"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "SVK"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "SVN"] <- "Eastern Europe"
OECDdata$REGION[OECDdata$REGION == "EST"] <- "Eastern Europe"

OECDdata$REGION[OECDdata$REGION == "AUT"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "BEL"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "DNK"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "FIN"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "FRA"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "DEU"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "GRC"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "ISL"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "IRL"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "ITA"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "LUX"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "NLD"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "NOR"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "PRT"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "ESP"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "SWE"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "CHE"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "GBR"] <- "Western Europe"
OECDdata$REGION[OECDdata$REGION == "AUT"] <- "Western Europe"



# Group data by quarter
OECDdata <- ddply(OECDdata,.(QUARTER))
names(OECDdata)[names(OECDdata) == "GROWTH RATE"] <- "GROWTH_RATE"
names(USdata)[names(USdata) == "GROWTH RATE"] <- "GROWTH_RATE"

# Make data frames for different regions
ASIA <- subset(OECDdata, OECDdata$REGION == "Asia")
PACIFIC_ISLANDS <- subset(OECDdata, OECDdata$REGION == "Pacific Islands")
MIDDLE_EAST <- subset(OECDdata, OECDdata$REGION == "Middle East")
NORTH_AMERICA <- subset(OECDdata, OECDdata$REGION == "North America")
SOUTH_AMERICA <- subset(OECDdata, OECDdata$REGION == "South America")
EASTERN_EUROPE <- subset(OECDdata, OECDdata$REGION == "Eastern Europe")
WESTERN_EUROPE <- subset(OECDdata, OECDdata$REGION == "Western Europe")


# Find average growth rate per quarter of different regions

ASIA_MEAN_GROWTH <- ddply(ASIA,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
PACIFIC_ISLANDS_MEAN_GROWTH <- ddply(PACIFIC_ISLANDS,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
MIDDLE_EAST_MEAN_GROWTH <- ddply(MIDDLE_EAST,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
NORTH_AMERICA_MEAN_GROWTH <- ddply(NORTH_AMERICA,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
SOUTH_AMERICA_MEAN_GROWTH <- ddply(SOUTH_AMERICA,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
EASTERN_EUROPE_MEAN_GROWTH <- ddply(EASTERN_EUROPE,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))
WESTERN_EUROPE_MEAN_GROWTH <- ddply(WESTERN_EUROPE,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))

# Add regions to data frames
ASIA_MEAN_GROWTH$REGION <- "Asia"
PACIFIC_ISLANDS_MEAN_GROWTH$REGION <- "Pacific Islands"
MIDDLE_EAST_MEAN_GROWTH$REGION <- "Middle East"
NORTH_AMERICA_MEAN_GROWTH$REGION <- "North America"
SOUTH_AMERICA_MEAN_GROWTH$REGION <- "South America"
EASTERN_EUROPE_MEAN_GROWTH$REGION <- "Eastern Europe"
WESTERN_EUROPE_MEAN_GROWTH$REGION <- "Western Europe"
USdata$REGION <- "United States"

# Add years to data frames
ASIA_MEAN_GROWTH$YEAR <- year(ASIA_MEAN_GROWTH$QUARTER)
PACIFIC_ISLANDS_MEAN_GROWTH$YEAR <- year(PACIFIC_ISLANDS_MEAN_GROWTH$QUARTER)
MIDDLE_EAST_MEAN_GROWTH$YEAR <- year(MIDDLE_EAST_MEAN_GROWTH$QUARTER)
NORTH_AMERICA_MEAN_GROWTH$YEAR <- year(NORTH_AMERICA_MEAN_GROWTH$QUARTER)
SOUTH_AMERICA_MEAN_GROWTH$YEAR <- year(SOUTH_AMERICA_MEAN_GROWTH$QUARTER)
EASTERN_EUROPE_MEAN_GROWTH$YEAR <- year(EASTERN_EUROPE_MEAN_GROWTH$QUARTER)
WESTERN_EUROPE_MEAN_GROWTH$YEAR <- year(WESTERN_EUROPE_MEAN_GROWTH$QUARTER)


# Prepare data for plotly
require(plotly)

ALLOtherOECD <- OECDdata[OECDdata$REGION != "Western Europe", ]
OECDAverageGrowthQuarterly <- ddply(OECDdata,.(QUARTER),summarise,MEAN_GROWTH_RATE=mean(GROWTH_RATE))

ALLOtherOECD$DATE <- as.Date(as.yearqtr(ALLOtherOECD$QUARTER, format = "Q%q/%y"))
WESTERN_EUROPE$DATE <- as.Date(as.yearqtr(WESTERN_EUROPE$QUARTER, format = "Q%q/%y"))
USdata$DATE <- as.Date(as.yearqtr(USdata$QUARTER, format = "Q%q/%y"))
OECDAverageGrowthQuarterly$DATE <- as.Date(as.yearqtr(OECDAverageGrowthQuarterly$QUARTER, format = "Q%q/%y"))

# Add full country name for labels
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "JPN"] <- "Japan"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "KOR"] <- "Korea"

ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "AUS"] <- "Australia"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "NZL"] <- "New Zealand"

ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "ISR"] <- "Israel"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "TUR"] <- "Turkey"

ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "CAN"] <- "Canada"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "MEX"] <- "Mexico"

ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "COL"] <- "Colombia"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "CHL"] <- "Chile"

ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "CZE"] <- "Czech Republic"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "HUN"] <- "Hungary"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "LVA"] <- "Latvia"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "LTU"] <- "Lithuania"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "POL"] <- "Poland"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "SVK"] <- "Slovak Republic"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "SVN"] <- "Slovenia"
ALLOtherOECD$COUNTRYFULL[ALLOtherOECD$COUNTRY == "EST"] <- "Estonia"

WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "AUT"] <- "Austria"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "BEL"] <- "Belgium"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "DNK"] <- "Denmark"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "FIN"] <- "Finland"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "FRA"] <- "France"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "DEU"] <- "Germany"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "GRC"] <- "Greece"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "ISL"] <- "Iceland"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "IRL"] <- "Ireland"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "ITA"] <- "Italy"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "LUX"] <- "Luxembourg"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "NLD"] <- "Netherlands"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "NOR"] <- "Norway"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "PRT"] <- "Portugal"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "ESP"] <- "Spain"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "SWE"] <- "Sweden"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "CHE"] <- "Switzerland"
WESTERN_EUROPE$COUNTRYFULL[WESTERN_EUROPE$COUNTRY == "GBR"] <- "United Kingdom"




#make labels for the countries
margin_GDPGrowthVis <- list(
  l = 100,
  r = 25,
  b = 75,
  t = 125)

GDP_Growth_Visualizer <- plot_ly() %>% 
  add_trace(data=ALLOtherOECD, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", mode="markers", color = ~REGION,
            colors = c("#a483a6",
                       "#C0C0C0",
                       "#f79edb",
                       "#753b69",
                       "#7e6aa6",
                       "#ba3f47")) %>% 
  add_trace(data=WESTERN_EUROPE, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", name = 'Western Europe', mode = "markers", marker = list(color = 'rgb(237, 149, 220)'), opacity = .3) %>% 
  add_trace(data=USdata, x = ~DATE, y = ~GROWTH_RATE, type="scatter", name = 'United States', mode = "lines", line = list(color = '#f55b7a')) %>%
  add_trace(data=OECDAverageGrowthQuarterly, x = ~DATE, y = ~MEAN_GROWTH_RATE, type="scatter", name = 'OECD Average', mode = "lines", line = list(color = '#c58ed4'))
GDP_Growth_Visualizer
GDP_Growth_Visualizer <- GDP_Growth_Visualizer %>% layout(
  xaxis = list(title = " "),
  yaxis = list (title = " "))

### "Year in Quarters"
### "Growth Rate (%)"

GDP_Growth_Visualizer <- GDP_Growth_Visualizer %>%
  layout(
    xaxis = list(nticks = 10,type = 'date',
                 tickformat = "%Y-Q%q"
    ))

GDP_Growth_Visualizer <- GDP_Growth_Visualizer %>% layout(title=list(text='GDP Growth Rates 2006-2020', font = list(color = "grey",size = 20)),
                                                          autosize = F,
                                                          #width = 500, height = 500,
                                                          margin = margin_GDPGrowthVis)
GDP_Growth_Visualizer <- GDP_Growth_Visualizer %>% layout(annotations = list(
  list(x = -0.15 , y = 0.5, text = "GDP Growth Rate (%)",
       font = list(color = "grey",size = 18),
       textangle = 270,
       showarrow = F, xref='paper', yref='paper', size=48)
  
))

GDP_Growth_Visualizer <- GDP_Growth_Visualizer %>%
  add_annotations(
    text = "Year in Quarters",
    x = 0.5,
    y = -0,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    yshift = -75,
    showarrow = FALSE,
    font = list(color = "grey",size = 18)
  ) 

GDP_Growth_Visualizer

# Second visualization -> double x axis to view difference between 2008 recession and COVID-19 recession
## Create data frames considering 2006-2010 and 2018-present
OECDAverageGrowthQuarterly$YEAR <- year(OECDAverageGrowthQuarterly$QUARTER)

housingcrisis_ALLOtherOECD <- subset(ALLOtherOECD, ALLOtherOECD$YEAR < 2010)
housingcrisis_WESTERN_EUROPE <- subset(WESTERN_EUROPE, WESTERN_EUROPE$YEAR < 2010)
housingcrisis_USdata <- subset(USdata, USdata$YEAR < 2010)
housingcrisis_OECDAverageGrowthQuarterly <- subset(OECDAverageGrowthQuarterly, OECDAverageGrowthQuarterly$YEAR < 2010)

pandemic_ALLOtherOECD <- subset(ALLOtherOECD, ALLOtherOECD$YEAR > 2017)
pandemic_WESTERN_EUROPE <- subset(WESTERN_EUROPE, WESTERN_EUROPE$YEAR > 2017)
pandemic_USdata <- subset(USdata, USdata$YEAR > 2017)
pandemic_OECDAverageGrowthQuarterly <- subset(OECDAverageGrowthQuarterly, OECDAverageGrowthQuarterly$YEAR > 2017)

housingcrisis_OECDAverageGrowthQuarterly$REGION <- "OECD Average"
pandemic_OECDAverageGrowthQuarterly$REGION <- "OECD Average"

# Plot 1, 2008 recession
housingcrisis <- plot_ly() %>% 
  add_trace(data=housingcrisis_ALLOtherOECD, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", mode="markers", color = ~REGION, legendgroup=~REGION,
            colors = c('#0d0887', '#46039f', '#7201a8', '#9c179e', '#bd3786', '#d8576b', '#ed7953', '#fb9f3a', '#d8df3b')) %>% 
  add_trace(data=housingcrisis_WESTERN_EUROPE, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", name = 'Western Europe', mode = "markers", marker = list(color = 'rgb(237, 149, 220)'), opacity = .3,legendgroup=~REGION) %>% 
  add_trace(data=housingcrisis_USdata, x = ~DATE, y = ~GROWTH_RATE, type="scatter", name = 'United States', mode = "lines", line = list(color = 'rgb(47,139,153)'),legendgroup=~REGION) %>%
  add_trace(data=housingcrisis_OECDAverageGrowthQuarterly, x = ~DATE, y = ~MEAN_GROWTH_RATE, type="scatter", name = 'OECD Average', mode = "lines", line = list(color = 'rgb(209, 176, 209)'),legendgroup=~REGION)
housingcrisis

housingcrisis <- housingcrisis %>%
  layout(
    xaxis = list(nticks = 6,type = 'date',
                 tickformat = "%Y-Q%q"
    ))

housingcrisis


# Plot 2, pandemic
pandemic <- plot_ly() %>% 
  add_trace(data=pandemic_ALLOtherOECD, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", mode="markers", color = ~REGION, legendgroup=~REGION,
            colors = c('#0d0887', '#46039f', '#7201a8', '#9c179e', '#bd3786', '#d8576b', '#ed7953', '#fb9f3a', '#d8df3b')) %>% 
  add_trace(data=pandemic_WESTERN_EUROPE, x = ~DATE, y = ~GROWTH_RATE, text = ~COUNTRYFULL, type="scatter", name = 'Western Europe', mode = "markers", marker = list(color = 'rgb(237, 149, 220)'), opacity = .3, legendgroup=~REGION) %>% 
  add_trace(data=pandemic_USdata, x = ~DATE, y = ~GROWTH_RATE, type="scatter", name = 'United States', mode = "lines", line = list(color = 'rgb(47,139,153)'),legendgroup=~REGION) %>%
  add_trace(data=pandemic_OECDAverageGrowthQuarterly, x = ~DATE, y = ~MEAN_GROWTH_RATE, type="scatter", name = 'OECD Average', mode = "lines", line = list(color = 'rgb(209, 176, 209)'),legendgroup=~REGION)
pandemic

pandemic <- pandemic %>%
  layout(
    xaxis = list(nticks = 6,type = 'date',
                 tickformat = "%Y-Q%q"
    ))

pandemic <- pandemic %>%
  layout(
    yaxis = list(nticks = 7,
                 tickvals = list(-15, -10, -5, 0, 5, 10, 15)
    ))
pandemic


# Stacked plot
comparison <- subplot(style(housingcrisis, showlegend = F), pandemic,  nrows = 2)
comparison


margin_ComparativeGDPGrowth <- list(
  l = 100,
  r = 25,
  b = 50,
  t = 125)

comparison <- comparison %>% layout(title=list(text='Comparative GDP Growth Trends Illustrated', font = list(color = "grey",size = 20)),
                                    autosize = F,
                                    #width = 500, height = 500,
                                    margin = margin_ComparativeGDPGrowth)

comparison <- comparison %>% layout(annotations = list(
  list(x = -0.15 , y = 0.5, text = "GDP Growth Rate (%)",
       font = list(color = "grey",size = 18),
       textangle = 270,
       showarrow = F, xref='paper', yref='paper', size=48)
  
))

comparison <- comparison %>%
  add_annotations(
    text = "Year in Quarters",
    x = 0.5,
    y = -0,
    yref = "paper",
    xref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    yshift = -52,
    showarrow = FALSE,
    font = list(color = "grey",size = 18)
  ) 

comparison

p <- plot_ly()
p$sizingPolicy$padding <- "0"
GDP_Growth_Visualizer


saveWidget(GDP_Growth_Visualizer,
                        "GDP_GROWTH_RATES.html",
                        selfcontained=FALSE)
# #libdir="\\\\wsl$\\Ubuntu\\home\\braintree\\site\\limetree.global\\assets\\js\\COVID-19-Economic-Data")
saveWidget(as_widget(comparison),
                        "RECESSION_COMPARISONS.html",
                        selfcontained=FALSE,
                        libdir=getwd())
