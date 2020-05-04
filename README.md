# DSP539Paper
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(broom)
library(readxl)

#Count the number of Fires that occured in a city's boundary for a given month/year. 
#Then sum the total acreage burned within a city's boundary for a given month/year. Combine those into one data frame.
City_Fire_Count <- X2007_2018_CA_FIRES%>%
  group_by(City,Counties,Month,Year)%>%
  count(City)

Fire_Sum_City <- X2007_2018_CA_FIRES %>% 
  filter(!is.na(City)) %>% #or use drop_na() to ride of any NA
  group_by(City,Counties,Month,Year) %>%
  summarize(Total_City_Fire_Acres = sum(GIS_Acres))

City_Fire_Sum_Count <- City_Fire_Count%>%
  select(City, Counties, Month, Year, n)%>%
  left_join(select(Fire_Sum_City, Total_City_Fire_Acres, City, Month, Year),
            by=c("City", "Counties","Month", "Year"))

#Repeat the above process for the counties.
County_Fire_Count <- X2007_2018_CA_FIRES%>%
  group_by(Counties,Month,Year)%>%
  count(Counties)

Fire_Sum_County <- X2007_2018_CA_FIRES %>% 
  group_by(Counties,Month,Year) %>%
  summarize(Total_County_Fire_Acres = sum(GIS_Acres))

County_Fire_Sum_Count <- County_Fire_Count%>%
  select(Counties, Month, Year, n)%>%
  left_join(select(Fire_Sum_County, Total_County_Fire_Acres, Counties, Month, Year),
            by=c("Counties", "Month", "Year"))


#Joining City and County Total Fire acres and count
City_County_Fire_Sum_Count<- City_Fire_Sum_Count %>%
  select(City, Counties, Month, Year, Total_City_Fire_Acres, n)%>%
  left_join(select(County_Fire_Sum_Count, Counties, Month, Year, Total_County_Fire_Acres, n),
            by = c("Counties", "Month", "Year"))

Seasonal_Average <-City_County_Fire_Sum_Count %>% 
  filter(!is.na(Total_County_Fire_Acres)) %>% #or use drop_na() to ride of any NA
  mutate(Season = case_when(Month >= 3 & Month < 6 ~ "Spring",
                            Month >= 6 & Month < 9 ~ "Summer",
                            Month >=9 & Month <12 ~ "Fall",
                            Month ==12 | Month <3 ~ "Winter"))%>%
  group_by(Season, Year) %>%
  summarize(Average_Seasonal_Fire_Acreage = mean(Total_County_Fire_Acres))


#Create graphs
Yearly_Seasonal_Average_Acres<-ggplot(Seasonal_Average, aes(Year, Average_Seasonal_Fire_Acreage)) + 
  geom_point() + facet_wrap(~Season)+theme_bw()+
  labs(title = "Comparing Average Acres Burned In California from 2007-2008 by Season", x= "Year", y= "Total County Fire Acres Burned", color="") +
  scale_x_continuous(breaks = c(2007, 2010, 2013, 2016, 2018))+
  scale_y_continuous(limits=c(0, 400000))

print(Yearly_Seasonal_Average_Acres)

Fire_Season_Average <-City_County_Fire_Sum_Count %>% 
  filter(!is.na(Total_County_Fire_Acres)) %>% #or use drop_na() to ride of any NA
  mutate(F_Season = case_when(Month >= 8 | Month < 5 ~ "Fire Season",
                            Month == 5 | Month < 8 ~ "Not Fire Season"))%>%
  group_by(F_Season, Year) %>%
  summarize(Average_F_Seasonal_Fire_Acreage = mean(Total_County_Fire_Acres))

Yearly_Fire_Season_Average<- ggplot(Fire_Season_Average, aes(Year, Average_F_Seasonal_Fire_Acreage)) + 
  geom_point() + geom_line() + facet_wrap(~F_Season)+theme_bw()+
  labs(title = "Comparing Average Acres Burned In California from 2007-2008 in and out of Fire Season", x= "Year", y= "Total County Fire Acres Burned", color="") +
  scale_x_continuous(breaks = c(2007, 2010, 2013, 2016, 2018))+
  scale_y_continuous(limits=c(0, 150000))

print(Yearly_Fire_Season_Average)
