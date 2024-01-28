## Google capstone - Neemias Moreira
#Marketig, looking for growth opportunities


#Loading libraries
library(dplyr)
library(janitor)
library('skimr')
library(here)
library(tidyverse)

#Loading dataset and checking them

minuteStepsNarrow_merged<- read.csv("minuteStepsNarrow_merged.csv")
minuteStepsNarrow_merged
head(minuteStepsNarrow_merged)
tail(minuteStepsNarrow_merged)

sleepDay_merged <- read.csv("sleepDay_merged.csv")

head(sleepDay_merged)

hourlyCalories_merged<- read.csv("hourlyCalories_merged.csv")
str(hourlyCalories_merged)
head(hourlyCalories_merged)

dailyCalories_merged<- read.csv("dailyCalories_merged.csv")

head(dailyCalories_merged)
skim_without_charts(dailyCalories_merged)
glimpse(dailyCalories_merged)

#Activities of the day
dailyActivity_merged<-read.csv("dailyActivity_merged.csv")
glimpse(dailyActivity_merged)

ggplot(data=hourlyCalories_merged) +
  geom_smooth(mapping = aes(x=Id,y=ActivityHour))

str(dailyActivity_merged)

clean_names(dailyActivity_merged)

  
ggplot(data=dailyActivity_merged)+
  geom_point(mapping = aes(x=TotalDistance,y=TotalSteps))

#Majority under 15000 steps day  

ggplot(data=dailyActivity_merged)+
  geom_point(mapping = aes(x=TotalDistance,y=Id ))


dailyActivity_merged$Id <- as.numeric(dailyActivity_merged$Id)


dailyActivity_merged<-read.csv("dailyActivity_merged.csv")

dailyActivity_merged$Id <- as.integer(dailyActivity_merged$Id)
dailyActivity_merged<-read.csv("dailyActivity_merged.csv")

str(dailyActivity_merged)

ggplot(data=dailyActivity_merged)+
  geom_point(mapping = aes(x=TotalDistance,y=TotalSteps ))
glimpse(dailyActivity_merged)

sum(dailyActivity_merged$TotalDistance)
mean(dailyActivity_merged$TotalDistance)
plot(dailyActivity_merged$TotalDistance)
plot(dailyActivity_merged$ActivityDate,)

df <-dailyActivity_merged

ggplot(df, aes(x = factor(1), y = TotalDistance)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  theme_minimal()

plot(dailyActivity_merged$ActivityDate)

str(dailyActivity_merged)

glimpse(dailyActivity_merged)

##create a box plot or pieplot of VeryActiveDistance, ModeratelyActiveDistance,LightActiveDistance 
#create using the mean of each one

print("Mean Very Active Distance:")
mean(dailyActivity_merged$VeryActiveDistance)
mean(dailyActivity_merged$ModeratelyActiveDistance)
mean(dailyActivity_merged$LightActiveDistance)

mean(dailyActivity_merged$VeryActiveMinutes)
mean(dailyActivity_merged$FairlyActiveMinutes)
mean(dailyActivity_merged$LightlyActiveMinutes)

new_data<- data.frame(
  category = c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes"),
  mean_distance = c(1.50,0.57,3.34),
  mean_minutes = c(21.16,13.56,192.81)
)
new_data
dailyActivity_merged
is_na <- any(is.na(dailyActivity_merged))
is_na

ggplot(new_data, aes(x = "", y = mean_distance, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Mean Distance")


ggplot(new_data, aes(x = "", y = mean_minutes, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Mean Minutes")

dailyActivity_merged$LoggedActivitiesDistance

sum(dailyActivity_merged$LoggedActivitiesDistance)
sum(dailyActivity_merged$TotalDistance)

print("The Total Logged Activities Distance: ")
x = sum(dailyActivity_merged$LoggedActivitiesDistance)
print("The Total Activities Distance(Logged and unlogged): ")
y = sum(dailyActivity_merged$TotalDistance)

porcentage = x/y 


