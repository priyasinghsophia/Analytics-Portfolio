# Analytics-Portfolio

---
title: "TRAFFIC VIOLATION IN MARYLAND"
author: "Priya Singh"
date: "October 18, 2018"
output:
  html_document:
      previewLinks: TRUE      
      theme: cosmo  
      toc: true 
      toc_float: true
      toc_depth: 3  
      fontsize: 16pt
      fig.height: 7
      fig.width: 7
#bibliography: bibliography.bib  
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# INTRODUCTION {-}

Traffic violations occur when drivers of vehicles violate traffic laws enforced by the government. It is a major cause of unnatural death besides other calamities.  The traffic tickets are generally provided for moving vehicles like speeding, inappropriate parking, Drinking under the influence and reckless driving. In the United States, a person who has violated the traffic rules can either plead guilty or he/she can pay the fine. The article [@http://www.todayifoundout.com], states that the first paper speeding ticket was issued to a driver in the United States in Dayton Ohio in 1904. Then and now the traffic violations have become a serious concern in the United States. As per the information from the Bureau of justice statistics[@ https://www.trafficticketrhino.com/7-interesting-driving-citation-statistics/] they found that traffic stop data 2008 had a greater percentage of male drivers (9.9%) than female drivers (7.0%). Speeding is the most common traffic violation in the United States. 

We would like to explore the following questions within this project:

-	What is the most common type of traffic violation?

-	Which type of vehicles has been mostly involved in violating the traffic?

-	What are the various types of arrest for violating the traffic?

-	Which city has the highest number of traffic violation and fatalities?

-	What are the reasons for a traffic violation?

-	Which state had the highest number of fatalities and drinking under influence cases?

-	Which gender are highly involved in violating the traffic?

-	Which gender has been involved in Drinking under influence cases?

-	What is the trend in a number of violations from 2012 to 2018?

-	Does the traffic violation has resulted in any personal injury or property damage?

-	Does the traffic is violated during weekends or weekdays because of DUI? And at what time?

-	At what time of year(month) traffic is mostly violated? Is it during Vacation time?


# DATA

We will be using the traffic violation dataset from [@data.gov]. The data.gov is an open source website where we can access a lot of datasets on any topic. In our dataset, we had 35 variables and 1Million observations. This dataset has records of traffic violation which have been reported in Montgomery County of  Maryland. 

This dataset consists of data from many past years. We have  1,373,693  selected observations, 20 variables and selected 2012-2018 years of data. The most of our variables are categorical data.


```{r}
library(readr)
library(readxl)
library(dplyr)
library(plyr)
library(lubridate)
library(leaflet)
library(ggmap)
library(tidyr)
library(rgdal)
library(expss)
library(kableExtra)
library(treemap)
library(knitr)
Traffic_Violations <- read_csv("C:/Users/prath/Downloads/Traffic_Violations.csv")
write_bib(file = "r-packages.bib")

```

# DATA ANALYSIS METHODS {-}

To explore the research questions, we used bar chart plot, boxplot, treemaps, leaflets, tilemap, and line plots to study traffic violation, fatalities and injuries caused due to a traffic violation. In this data, the traffic violation has been reported electronically in Montgomery County police where it has data of all the states of United States. Most of the records have been reported from Maryland, so we have selected to study Maryland state using subset function of package [@R-dplyr]. We also have used expss package [@R-expss] for our baseline statistics.

- In the first step, We have removed the NA's from the data frame and removed unknown data from the gender variable.

- We have also extracted year, month, hour and day with the help of format function. The date was converted to days and month was converted to month abbreviation. In order to analyze the time and day of traffic violations.


```{r}
Traffic_Violations$day <- format(as.Date(Traffic_Violations$`Date Of Stop`, "%m/%d/%y"),"%A")
Traffic_Violations$year <- format(as.Date(Traffic_Violations$`Date Of Stop`, format="%m/%d/%Y"),"%Y")
Traffic_Violations$month <- format(as.Date(Traffic_Violations$`Date Of Stop`, "%m/%d/%y"),"%m")
Traffic_Violations$daydate <- format(as.Date(Traffic_Violations$`Date Of Stop`, "%m/%d/%y"),"%d")
daydate <- Traffic_Violations$daydate
month <- Traffic_Violations$month
day <- Traffic_Violations$day
year <- Traffic_Violations$year
 time_of_stop <- Traffic_Violations$`Time Of Stop`
 lattitude <- Traffic_Violations$Latitude
 longitude <- Traffic_Violations$Longitude
 accident <- Traffic_Violations$Accident
 belts <- Traffic_Violations$Belts
 personal_injury <- Traffic_Violations$`Personal Injury`
 property_damage <- Traffic_Violations$`Property Damage`
 fatal <- Traffic_Violations$Fatal
 vehicle_type <- Traffic_Violations$VehicleType
 commercial_vehicle <- Traffic_Violations$`Commercial Vehicle`
 alcohol <- Traffic_Violations$Alcohol
 race <- Traffic_Violations$Race
 arrest_type <- Traffic_Violations$`Arrest Type`
 charge <- Traffic_Violations$Charge
 gender <- Traffic_Violations$Gender
 state <- Traffic_Violations$State
 driving_license_state <- Traffic_Violations$`DL State`
 work_zone <- Traffic_Violations$`Work Zone`
 violation_type <- Traffic_Violations$`Violation Type`
 hazmat <- Traffic_Violations$HAZMAT
 driver_city<- Traffic_Violations$`Driver City`
 Traffic_Violations_new <- data.frame(daydate,day,month,year,time_of_stop,lattitude,longitude,accident, belts,personal_injury,property_damage,fatal,vehicle_type,commercial_vehicle,alcohol,race,arrest_type,charge,gender,state,driving_license_state,work_zone,violation_type,hazmat, driver_city)
 Traffic_Violations_new <- subset(Traffic_Violations_new, race == "ASIAN"|race == "BLACK"|race == "HISPANIC"|race == "NATIVE AMERICAN"|race == "WHITE")
Traffic_Violations_new <- subset(Traffic_Violations_new, gender == "F"|gender == "M")
write.csv(Traffic_Violations_new, "Traffic_Violation_project.csv")
Traffic_Violation_project <- read_csv("~/Traffic_Violation_project.csv", 
   col_types = cols(Hour = col_number(), 
         Minute = col_number(), daydate = col_number(), 
         month = col_number(), year = col_number()))
Traffic_Violation_project<- na.omit(Traffic_Violation_project) 
Traffic_Violation_project$number_of_records <- seq.int(nrow(Traffic_Violation_project))
Traffic_Violation_project$Violation <- 1
Traffic_Violation_project <- subset(Traffic_Violation_project, gender == "M"|gender == "F")
Traffic_Violation_project <- transform(Traffic_Violation_project, MonthAbb = month.abb[month])
Automobile <- subset(Traffic_Violation_project, vehicle_type == "02 - Automobile")
maryland <- subset(Automobile, state == "MD")
dui <- subset(maryland, alcohol == "Yes")
accident1 <- subset(maryland, accident == "Yes")
fatal1 <- subset(maryland, fatal == "Yes")
belt <- subset(maryland, belts == "No")
personalinjury <- subset(maryland, personal_injury == "Yes")
propertydamage <- subset(maryland, property_damage  == "Yes")
workzone <- subset(maryland, work_zone == "Yes")
hazmat1 <- subset(maryland,hazmat  == "Yes")
cities_maryland <- subset(maryland, driver_city == "SILVER SPRING"| driver_city == "GAITHERSBURG"|driver_city == "GERMANTOWN"|driver_city == "ROCKVILLE"|driver_city == "WASHINGTON"|driver_city=="BETHESDA"|driver_city == "MONTGOMERY VILLAGE"|driver_city == "HYATTSVILLE"|driver_city == "POTOMAC"|driver_city == "OLNEY"|driver_city== "LAUREL"|driver_city == "FREDERICK"|driver_city == "FREDERICK"|driver_city == "TAKOMA PARK"|driver_city == "NORTH POTOMAC"|driver_city == "KENSINGTON"|driver_city == "DAMASCUS"|driver_city == "BURTONSVILLE"|driver_city == "BELTSVILLE"|driver_city == "CHEVY CHASE"|driver_city == "BALTIMORE")
Traffic_Violations_new <- separate(data = Traffic_Violations_new, col = "time_of_stop", into = c("Hour","Minute"))
```


#DATA ANALYSIS AND VISUALIZATION

## STEP 1 {-}

In the first step of the analysis, we have studied on the below points for our analysis.

- Types of a traffic violation in the United States

- Type of vehicles which have been involved in violating the traffic

### 1.Types of a traffic violation in the United States


The bar plot was created with the help of [@R-ggplot2] to analyze the different type of violations which are citations, SERO, ESERO, and warnings. Citations are given for speeding, inappropriate parking, reckless driving etc. SERO is called as Safety Equipment Repair Order and SERO is called as Electronic Safety Equipment Repair Order.


```{r, fig.cap="Figure 1: Type of traffic violation in United States", fig.align='center'}

ggplot(Traffic_Violation_project, aes(x = Traffic_Violation_project$violation_type)) + geom_bar(position = "fill",fill = "blue") + geom_text(aes(label=scales::percent(..count../sum(..count..))), stat='count',position=position_fill(vjust = 0.5)) + ggtitle("Most common type of Traffic Violation") + theme_bw() + ylab("Percentage")  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Types of traffic violation") 
violations <- apply_labels(Traffic_Violation_project, violation_type = "Type of violations")
violations <- cro_cases(violations$violation_type)
violations
```

In figure 1, we can analyze that the warning has 49.1% and the citation has 45.9% in having more number of traffic violation records. There are total 551756 records of citations and 590563 records for warning in the table.

### 2.Type of vehicles which have been involved in violating the traffic

The treemap is used with the help of [@R-treemap] for analyzing the types of vehicles which are involved in the traffic violation against the number of records.

```{r, fig.cap="Figure 2: Type of vehicles involved in traffic violation", fig.align='center'}
vehicle_count <- count(Traffic_Violation_project, 'vehicle_type')
colnames(vehicle_count)<-c('vehicle','count')
vehicle_count <- subset(vehicle_count, count > 1000)
vehicle_count <- subset(vehicle_count, vehicle != "29 - Unknown")
treemap(vehicle_count,index="vehicle",vSize="count",type="index", title="Type of vehicles by number of records", border.col ="brown",fontcolor.labels=c("blue","red"),fontface.labels=c(2,1),bg.labels=c("transparent"), align.labels=list(c("center", "center"), c("right", "bottom")),overlap.labels=0.5,inflate.labels=F)
vehicles <- apply_labels(Traffic_Violation_project, vehicle_type = "Type of vehicle")
vehicles <- cro_cases(vehicles$vehicle_type)
head(vehicles)
```

## STEP 2 {-}

In the next step of our analysis, we have filtered out the traffic violations which have been caused by automobiles in Maryland. And we have also studied two factors (belt and alcohol) which have caused traffic violations in Maryland by automobiles. 

- Cities of Maryland which have the highest number of traffic violation over years
- Reasons for a traffic ticket and types of arrest


### 1. Cities of Maryland which have the highest number of traffic violation over years 

The cities of Maryland have been faceted with a year on x-axis and number of records on the y-axis. We have used ggplot function with facet_wrap function for analyzing the cities and number of records over year. The x-axis is a year and a y-axis is a number of traffic violation records every year. The bars in each facet depicts the number of records every year.

```{r, fig.cap="Figure 3: Number of traffic violation records over years in cities of Maryland", fig.align='center'}

driver_cities <- cities_maryland$driver_city
year <- cities_maryland$year
number_city <- cities_maryland$number_of_records
facets_data <- data.frame(driver_cities,year,number_city)
ggplot(data = facets_data, aes(x = facets_data$year, fill = driver_cities, group = driver_cities)) + geom_bar(stat = "count") + facet_wrap(~ driver_cities, shrink = TRUE) + theme_bw() + theme(legend.position = 'none',strip.text = element_text(size=5)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = 2012:2018) + xlab("Year") + ylab("Count") + ggtitle("Facets wrap - Traffic violation for Cities versus year")
top_cities <- apply_labels(facets_data, driver_cities = "City", year = "Year")
top_cities <- cro_cases(top_cities$driver_cities,top_cities$year)
top_cities
```

In figure 3, we have found out that Silver Spring had  45,671 cases which is the highest number of traffic violations in the year 2015. The total number of violations reported in  2015 is 133109. 

### Reasons for a traffic ticket and types of arrest 

The two reasons for a traffic violation are no seat belt at the time of violating traffic and alcohol use while driving. We will be analyzing both variables with respect to time, day, gender and violation type. We have also studied the type of arrest for violating different types of traffic violations.

### 1. Analyzing time and day of Drinking under influence

In this graph, we have used heatmaps with the help of ggplot and tilemap. The x-axis is the day of the week and the y-axis is the time when the violation occurred. The number of records has been used as a fill.


```{r, fig.cap="Figure 4: Heatmap - Time and day of Drinking under influence", fig.align='center'}


 ggplot(dui, aes(x = dui$day, y = dui$Hour)) + geom_tile(aes(fill = dui$number_of_records), colour = "white") + scale_fill_gradient(name = "Number of records", low = "steelblue", high = "red") + scale_y_continuous(breaks = unique(Traffic_Violation_project$Hour)) +
    labs(title = "Heatmap of daily traffic violation due to DUI", x = "Day of week", y = "Hour") +
    theme(axis.ticks = element_blank()) + theme_bw()
```

In figure 4, the red areas represent the times of the day and the days of the week that receive the most reports of traffic violations activity. We can see that these incidents are concentrated on the weekends and during the weekday late evenings 5 pm in all the days of the week is having more number of traffic violation records due to Drinking under influence.

### 2. Analyzing Drinking under influence locations in Maryland

The variable drinking under influence has two levels(Yes, No). We have selected yes in order to map the location of drinking under influence in Maryland. Leaflets pacakge [@R-leaflet] have been used in order to identify the locations. The shapefiles using [@R-sp] of Maryland have been used in order to map districts layer and plot the locations.


```{r, fig.cap="Figure 5: Mapping drinking under influence location in Maryland", fig.align='center'}

counties = readOGR(dsn="C:/Users/prath/Documents/priya/GIS Files/cb_2016_us_county_500k", layer="cb_2016_us_county_500k", stringsAsFactors = FALSE)

md_districts<-readOGR(dsn = "C:/Users/prath/Documents/priya/GIS Files",layer="cb_2017_24_sldu_500k")

districts<-md_districts["STATEFP"=="24"]

mymap1 <- leaflet(dui) %>%
     addTiles()%>% addCircleMarkers(lat = dui$lattitude, lng = dui$longitude, label = ~as.character(driver_city), stroke = FALSE, fillOpacity= 0.3, radius=2, color = "red") %>% addPolygons(data= districts, fill = FALSE, color = "blue") %>% setView(lng = -77.0261, lat = 39.1434, zoom = 10) %>% addRectangles(lat1 =  39.3762, lng1 = -77.2794, lat2 = 38.9907,lng2 = -77.0261, color = "black")
mymap1
duitable <- apply_labels(dui, alcohol = "Drinking under influence", driver_city= "City")
duitable <- cro_cases(duitable$driver_city,duitable$alcohol)
duitable
```

In figure 5, we have found out that the Gaithersburg, Germantown, and Damascus are said to have a number of drinking under influence cases in Maryland. 


### 3. Distribution of type of violations and gender - No seat belt

The variables belt has two levels(yes, no). The no level from the variable has been selected from the data using subset function. The bar chart is used to analyze the percentage of gender having no seat belt at the time of violating traffic and in which of the type of violation they are more.

```{r, fig.cap="Figure 6: No seat belt - Distribution of type of violations and gender", fig.align='center'}

ggplot(belt, aes(x= belt$violation_type, fill = belt$gender)) + geom_bar(position = "fill")+ ggtitle("Gender - No seat Belt") + theme_bw() + labs(fill = "Gender") +  xlab("Violation type") +  geom_text(aes(label=scales::percent(..count../sum(..count..))), stat='count',position=position_fill(vjust = 0.5)) + annotate("text", x = 2, y = 1.1, label = "Citation is highest in males for no seat belt")
beltviolation <- apply_labels(belt, gender =  "Gender", violation_type= "Type of violation")
beltviolation <- cro_cases(beltviolation$gender,beltviolation$violation_type)
beltviolation
```

In figure 6, the percentage of the male is 31% for no seat belt in citation and 30% in warnings. The percentage of a female in no seat belt is less in compared to males. The males have total 2,77,570 cases for citations and 2,77,160 for warnings.


### 4. Different types of arrest for violating the traffic 

In the variable arrest_type, there are 19 types of arrest for violating the traffic. But we have selected Marked Patrol, Unmarked Patrol, Marked VASCAR, Marked Laser, License Plate Recognition, Foot Patrol, Motorcycle and Marked Stationary Radar. A bar graph is used for analyzing the distribution of various types of arrest type in the type of violations.

```{r, fig.cap="Figure 7: Different types of arrest for violating the traffic",fig.align='center'}
arrest_types_subset <- subset(Traffic_Violation_project, arrest_type == "A - Marked Patrol"|arrest_type == "B - Unmarked Patrol"|arrest_type == "C - Marked VASCAR"|arrest_type == "Q - Marked Laser"|arrest_type == "S - License Plate Recognition"| arrest_type == "O - Foot Patrol"|arrest_type == "L - Motorcycle"|arrest_type == "E - Marked Stationary Radar")
ggplot(data=arrest_types_subset,aes(arrest_types_subset$arrest_type, fill = arrest_types_subset$violation_type)) + 
     geom_bar(position = "fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))), stat='count',position=position_fill(vjust = 0.5)) + xlab("Arrest type") + ylab("Percentage") + labs(fill = "Type of violation") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + annotate("text", x = 1, y = 1.1, label = "Marked patrol is highest in arresting for citation and warning") + ggtitle("Distribution of arrest type and violation type")
arrest <- apply_labels(arrest_types_subset, arrest_type = "Arrest type", violation_type = "Type of violation")
arrest <- cro_cases(arrest$arrest_type,arrest$violation_type)
arrest
```

In figure 7, the most common type of arrest is marked patrol which is 42% in warning and 36.5% in the citation. ESERO and SERO are not very common traffic violation in the United States. Marked patrol is a ground vehicle used by police for transportation during patrols and also to be able to respond to any incidents and a traffic violation. There are total 4,33,202 cases of Marked Patrol arrest for citations and 5,01,281 for the warning.

## STEP 3 {-}

In step 3, we have analyzed the trend from the year 2012 to 2018

- Traffic violation every year
- Fatalities every year
- Property damages every year
- Personal injuries every year

### 1. Traffic violation from 2012 - 2018

In this analysis, we have analyzed the trend of traffic violation from 2012 to 2018. The line graph is used with help of function geom_smooth and ggplot function. The x-axis is a month and the y-axis is the number of traffic violation records. The color aesthetic is used to denotes every year with the help of line.


```{r, fig.cap="Figure 8: Traffic violation from 2012 - 2018", fig.align='center'}

 Traffic_Violation_project$year <- factor(Traffic_Violation_project$year)
 ggplot(data=Traffic_Violation_project,aes(Traffic_Violation_project$month)) +
     geom_smooth(stat = "count",aes(color=Traffic_Violation_project$year))+
     xlab("Month") + ylab("Violations per year") + scale_x_continuous(breaks=1:12) + labs(color = "Year") + ggtitle("Traffic violation from 2012 to 2018") + theme_bw()

```

In figure 8, we have analyzed that traffic violation has been increased in 2016 in the month of March - April. This year had the highest number of violation in the month of April.

### 2. Fatalities caused due  to traffic violation from 2012 - 2018

In this analysis, we have analyzed the trend of fatalities from 2012 to 2018. The line graph is used with help of function geom_smooth and ggplot function. The x-axis is a month and the y-axis is the number of traffic violation records. The color aesthetic is used to denotes every year with the help of line chart.


```{r,fig.cap="Figure 9: Fatalities from 2012 - 2018", fig.align='center'}

ggplot(data=fatal1,aes(fatal1$year)) +
     geom_smooth(stat = "count",aes(color=fatal1$fatal))+
      xlab("Year") + ylab("Number of Ftalities") + scale_x_continuous(breaks=2012:2018) + ggtitle("Fatalities from 2012 to 2018") + theme_bw() + labs(color = "Fatalities") + annotate("text",x = 2015, y = 38, label = "Highest fatalities due to traffic violation")
```

In figure 9, it depicts that the highest number of fatalities have been caused in the year 2015. 


### 3. Property damages caused due to traffic violation from 2012 - 2018

In this analysis, we have analyzed the trend of property damages from 2012 to 2018. The line graph is used with help of function geom_smooth and ggplot function. The x-axis is a month and the y-axis is the number of traffic violation records. The color aesthetic is used to denotes every year with the help of line chart.

```{r, fig.cap="Figure 10: Property damages from 2012 - 2018", fig.align='center'}
ggplot(data=propertydamage,aes(propertydamage$year)) + geom_smooth(stat = "count",aes(color=propertydamage$property_damage))+ xlab("Year") + ylab("Number of Property damage") + scale_x_continuous(breaks=2012:2018) + theme_bw() + labs(color = "Property damage") + annotate("text",x = 2017, y = 3100, label = "Highest property damages") + ggtitle("Property damage from 2012 to 2018")
```

In figure 10, we have analyzed that The highest number of property damages occurred in the year 2017.

### 4. Personal injury caused due to traffic violation from 2012 - 2018

The time series from the year 2012 to 2018 has been analyzed in this graph. We have used geom_smooth to see the trend in the years.

```{r, fig.cap="Figure 11:Personal injuries from 2012 - 2018", fig.align='center'}
ggplot(data=personalinjury,aes(personalinjury$year)) +
    geom_smooth(stat = "count",aes(color=personalinjury$property_damage)) + ylab("Number of Personal injuries") + scale_x_continuous(breaks=2012:2018) + ggtitle("Personal injuries from 2012 to 2018") + theme_bw() + labs(color = "Personal injuries") + annotate("text",x = 2017, y = 2100, label = "Highest personal injury") + xlab("Year")
```

In figure 11, we have analyzed that the highest number of personal injuries took place in the year 2017. 

### 5. Maximum personal injury in a month

In this analysis, we have analyzed the number of personal injuries in a month on an average using bar plot.

```{r, fig.cap="Figure 12: Maximum personal injuries in a month", fig.align='center'}
reorder_vehicles <- function(x) {
  factor(x, levels = names(sort(table(x),decreasing = TRUE)))
}

ggplot(personalinjury, aes(x=reorder_vehicles(personalinjury$MonthAbb))) + geom_bar(fill = "blue") + xlab("Month") + ylab("Count") + theme_bw() + ggtitle("Maximum personal injury in a month")

```

Figure 12 depicts that most numbers of injuries have taken place in the month of October - May due to vacation time. The least is in January, February, and March due to the winter season.

### 6. Maximum property damage in a month

In this analysis, we have analyzed the number of property damages in a month on an average using bar plot.

```{r, fig.cap="Figure 13: Fatality regions of Maryland", fig.align='center'}
reorder_vehicles <- function(x) {
  factor(x, levels = names(sort(table(x),decreasing = TRUE)))
}

ggplot(propertydamage, aes(x=reorder_vehicles(propertydamage$MonthAbb))) + geom_bar(fill = "red") + xlab("Month") + ylab("Count") + theme_bw() + ggtitle("Maximum property damage in a month")
```

Figure 13 depicts that most numbers of property damages have taken place in the month of May and December due to vacation time. The least is in April, February, and September.

## STEP 4 {-}

In this step, we will analyze the locations where most of the fatalities, property damages, and personal injuries took place using leaflets [@R-leaflet]. 

### 1. Fatalities in cities of Maryland

In this analysis, we have analyzed the locations of fatalities in cities of Maryland. The awesome markers are used mark the locations in the map. Regions data is used to make the boundary in the map.

```{r, fig.cap="Figure 14: Fatality regions of Maryland", fig.align='center'}
d<-leaflet(md_districts) %>% addTiles() %>% addPolygons(weight=.75,color="green",fillOpacity = .2)
 
new <- c("red")[fatal1$fatal]
                           icons <- awesomeIcons(
                                 icon = 'ios-close',
                                 iconColor = 'black',
                                 library = 'ion',
                                 markerColor = new
                             )
 leaflet(fatal1)%>% addTiles() %>% addProviderTiles(providers$Stamen.TonerLines,options = providerTileOptions(opacity = 0.3))%>% addAwesomeMarkers(lng= fatal1$longitude, lat=fatal1$lattitude, icon=icons,popup=~fatal1$driver_city,                                                          label=~as.character(driver_city)) %>% addPolygons(data= md_districts, fill = FALSE, color = "brown") %>% setView(lng = -77.2014, lat = 39.1434, zoom = 11)
fataltable <- apply_labels(cities_maryland, driver_city = "City", fatal = "Fatalities")
fataltable <- cro_cases(fataltable$driver_city,fataltable$fatal)
fataltable
```


In figure 14, we have analyzed that most of the incidents have occurred in Silver Spring and Gaithersburg and German town. There are total  48 cases of fatalities in Maryland and total cases are 130 in selected cities of Maryland. The fatalities reported are less in Maryland.

### 2. Property damages in cities of Maryland

In this analysis, we have analyzed the locations of property damage in the cities of Maryland. The circle markers are used to plot the cities in Maryland. Regions data is used to make the boundary in the map.


```{r, fig.cap="Figure 15: Property damage regions of Maryland", fig.align='center'}

mymap2 <- leaflet(propertydamage) %>%
   addTiles() %>%
     addCircleMarkers(lat = propertydamage$lattitude, lng = propertydamage$longitude, label = ~as.character(driver_city), stroke = FALSE, fillOpacity= 0.3, radius=1) %>% addPolygons(data= md_districts, fill = FALSE, color = "brown") %>% setView(lng = -77.0261, lat = 39.1434, zoom = 10) %>% addRectangles(lat1 =  39.1732, lng1 = -77.2039, lat2 = 38.9907,lng2 = -77.0261, color = "Red")
mymap2 
propertydamtable <- apply_labels(cities_maryland, driver_city = "City", property_damage="Property damage")
propertydamtable <- cro_cases(propertydamtable$driver_city,propertydamtable$property_damage)
propertydamtable
```

In figure 15, we have analyzed that the cities Silver Spring, White Oak, Gaithersburg, and Rockville have the highest number of property damage in Maryland. There were total 5524 cases in Silver Spring and total cases of property damage are 13723.

### 3. Personal injuries in cities of Maryland

In this analysis, we have studied the locations in Maryland where personal injuries took place. The circle markers are used to plot the cities in Maryland. Regions data is used to make the boundary in the map.

```{r, fig.cap="Figure 16: Personal injury regions of Maryland", fig.align='center'}
                                                                                                        
mymap3 <- leaflet(personalinjury) %>%
  addTiles()%>% addCircleMarkers(lat = personalinjury$lattitude, lng = personalinjury$longitude, label = ~as.character(driver_city), stroke = FALSE, fillOpacity= 0.3, radius=2, color = "red") %>% addPolygons(data= counties, fill = FALSE, color = "blue") %>% setView(lng = -77.0261, lat = 39.1434, zoom = 11) %>% addRectangles(lat1 =  39.1732, lng1 = -77.2039, lat2 = 38.9907,lng2 = -77.0261, color = "black")
mymap3
personalinjurytable <- apply_labels(cities_maryland, driver_city = "City",personal_injury = "Personal injury")
personalinjurytable <- cro_cases(personalinjurytable$driver_city,personalinjurytable$personal_injury)
personalinjurytable
```

In figure 16, we have analyzed that the cities Silver Spring, White Oak, Gaithersburg, and Rockville have the highest number of personal injurires in Maryland. There were total 2979  cases in Silver Spring and total cases of property damage are 8203.

# CONCLUSION {-}

- Silver Spring had a number of fatalities, personal injuries and property damages.

- The frequency of the incident was high in the year 2016 and also in 2018 in the month of April  

-	In majority cases, violations constantly occurred during vacation time    N MAJORITY CASES  violations constantly occurred during vacation time.

-	Drinking under influence took place generally during the weekends and between 3 pm - 10 pm.

-	Marked patrol is the most common type of arrest for violating the traffic

- Males have been more involved in violating traffic

- Female count in drinking under influence is more than males.

- Silver Spring has the highest number of traffic violation in Maryland.

- In this study, we have analyzed that this data can be used to reduce traffic violation in future and understanding the causes.


# BIBLIOGRAPHY
Data.gov. (n.d.). Retrieved from https://www.data.gov/
 
Traffic Violations. (2018, August 09). Retrieved from https://catalog.data.gov/dataset/traffic-violations-56dda
 
Campbell, C. (2018, June 18). 'The traffic is horrible': Baltimore commute ranks among nation's longest. Retrieved from   http://www.baltimoresun.com/news/maryland/baltimore-city/bs-md-commute-times-20180510-story.html

Aesthetic specifications. (n.d.). Retrieved from https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
 
(n.d.). Retrieved from https://rstudio.github.io/leaflet/?Name=Leaflet for R (required reading)

(n.d.). Retrieved from http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#facetwrap

Treemap. (n.d.). Retrieved from https://www.rdocumentation.org/packages/treemap/versions/2.4-2/topics/treemap

Contact Between the Police and Public, 2008. (n.d.). Retrieved from https://www.bjs.gov/index.cfm?ty=pbdetail&iid=2229

The First Speeding Infraction in the U.S. was Committed by a New York City Taxi Driver in an Electric Car on May 20, 1899. (2012, November 28). Retrieved from http://www.todayifoundout.com/index.php/2011/05/the-first-speeding-infraction-in-the-u-s-was-committed-by-a-new-york-city-taxi-driver-in-an-electric-car-on-may-20-1899
 
Interesting Driving Citation Statistics. (2016, April 15). Retrieved from https://www.trafficticketrhino.com/7-interesting-driving-citation-statistics/
 
Mcphate, M. (2017, May 15). California Today: Tackling Some 'Ridiculously High' Traffic Fines. Retrieved from https://www.nytimes.com/2017/05/15/us/california-today-traffic-fines.html
 
Do traffic tickets reduce motor vehicle crashes? Evidence from "Click It or Ticket". (2015, May 19). Retrieved from https://journalistsresource.org/studies/environment/transportation/motor-vehicle-crashes-click-it-or-ticket-laws

#APPENDIX

```{r,  ref.label=knitr::all_labels(),echo=TRUE,eval=FALSE}

```


