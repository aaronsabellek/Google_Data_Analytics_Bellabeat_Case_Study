# Google "Data Analytics" Case Study
### How Can a Wellness Technology Company Play It Smart?

The following analysis is the **final project of the "Data Analysis" course from Google** offered on the online platform Coursera. It is intended to apply, deepen and present what has been learned. The task is to analyze a specific data set for the company Bellabeat, a high-tech manufacturer of health-focused products for women, and its marketing purposes. For this purpose it follows the 6 steps taught by Google for analysis:

1. Ask -> formulate the task
2. Prepare -> present and classify the available data
3. Process -> cleaning the data
4. Analyze -> analyze the data
5. Share -> visualize the results
6. Act -> formulate concrete instructions for data-driven decisions

Since visualizing the data in my approach is already an important part of the analysis itself, points 4. and 5. are merged.

# 1. Ask
The Bellabeat company sells three products designed to support women's health: An app, a wellness tracker and a wellness smart watch. The key task is now to **analyze smart device usage data** to gain insights into three main questions:

1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

Key stakeholders are Bellabeat's co-founders Urška Sršen and Sando Mur as well as it's marketing analytics team.

# 2. Prepare
The main data resource pointed to by Bellabeat's co-founder Sršen is **an open source fitness tracker dataset by FitBit**. This data set can be downloaded on the online platform Kaggle (https://www.kaggle.com/datasets/arashnic/fitbit) and can be used without any restrictions (CC0). This data was generated by 33 users through Amazon Mechanical Turk from 12/04-12/05/2016 (the website states an incorrect number of users as well as an incorrect time period). The data is stored in eighteen csv files and includes information on physical activity, sleep, and heart rate. Most of the data is given in daily, hourly and minutely intervals, the heart rate only in 5 second intervals. Some data overlap.

The data set contains **five main limitations**:

1. It was generated between 12.04.2016 and 12.05.2016 and therefore has a **high risk of being outdated**
2. It relies on the data of only thirty FitBit users and has therefore only **little significance**
3. It doesn't contain any demographic information and therefore has a **high risk of being biased**, especially regarding the sex/gender for the women-centered company Bellabeat
4. It is a third-party source **not** generated under **transparent** circumstances
5. In many cases the meaning of the data is not clear, for example what units are used to measure distances, making the data sets **very open for interpretation**

This limitations make the data set all in all an unreliable source for making data driven decisions, but can be used as **an heuristic starting point**.

# 3. Process

I am using the programming language R inside of the IDE RStudio for cleaning, analyzing and visualizing the data.

First, the necessary packages are loaded.
```{r echo=T, results='hide', message=FALSE}
library("here")
library("skimr")
library("janitor")
library("tidyverse")
library("dplyr")
library("cowplot")
library("formattable")
```

The **cleaning process** consists of the following steps **for each data set**:

* Loading data into own data frames and variables
* Checking data frames for column names, number of rows, missing data, white space and format consistency with the skim_without_charts()-function.
* Cleaning column names with the clean_names()-function
* Most date columns where stored in string-format, so they have to be changed to date format with the Date()- or strptime()-function
* Check for duplicate rows by combining the sum()- and duplicated()-functions and delete duplicated rows with the distinct()-function if needed
* Count the distinct IDs for each data frame with the n_distinct()-function
* Add a column showing the weekday of the date with the mutate()- and weekdays()-function and sometimes sort it with the factor()-function
* Merging the matching data with the merge()-function
* Re-check that all the cleaning processes have gone correctly and the data is ready for analysis

The used data sets that will be cleaned are the following:

* 3.1 daily activity
* 3.2 hourly calories
* 3.3 hourly steps
* 3.4 daily sleep
* 3.5 weight
* 3.6 heart rate

There remain some data sets that are not analyzed. The data for daily calories, daily intensity, and daily steps is already included in the daily activity package. The minute data for calories, intensity, METs, sleep, and steps is too specific for analysis and most already exists in daily or hourly format. Similarly, the data for hourly intensities is too specific for analysis and already exists in daily format.

Since I use the German version of RStudio the automatically generated weekdays have German names.

### 3.1 Daily activity
```{r echo = T, results = 'hide', message=FALSE}
daily_activity <- read_csv("../Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
skim_without_charts(daily_activity)
daily_activity <- clean_names(daily_activity)
daily_activity$activity_date <- as.Date(daily_activity$activity_date, format="%m/%d/%Y")
sum(duplicated(daily_activity))
n_distinct(daily_activity$id)
daily_activity <- daily_activity %>%
  mutate(weekday=weekdays(activity_date))
daily_activity$weekday <- factor(daily_activity$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/378af772-f3cb-4b1e-aa35-902d51930944)

### 3.2 Hourly calories
```{r echo = T, results = 'hide', message=FALSE}
hourly_calories <- read_csv("../Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
skim_without_charts(hourly_calories)
hourly_calories <- clean_names(hourly_calories)
hourly_calories$activity_hour <- strptime(hourly_calories$activity_hour, "%m/%d/%Y %I:%M:%S %p")
sum(duplicated(hourly_calories))
n_distinct(hourly_calories$id)
hourly_calories <- hourly_calories %>%
  mutate(weekday=weekdays(activity_hour))
hourly_calories$weekday <- factor(hourly_calories$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/0c9bd3b4-d4e2-439c-bf3c-e023f9edad62)

### 3.3 Hourly steps
```{r echo = T, results = 'hide', message=FALSE}
hourly_steps <- read_csv("../Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
skim_without_charts(hourly_steps)
hourly_steps <- clean_names(hourly_steps)
hourly_steps$activity_hour <- strptime(hourly_steps$activity_hour, "%m/%d/%Y %I:%M:%S %p")
sum(duplicated(hourly_steps))
n_distinct(hourly_steps$id)
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/520f38d3-0dc3-4787-8b88-d9ce55aacb98)

Now the hourly steps can be merged with the hourly calories into one data frame for hourly data, added by a column for the hour only.
```{r echo = T, results = 'hide', message=FALSE}
hourly_data <- merge(hourly_calories, hourly_steps, by=c("id", "activity_hour"), all = TRUE)
n_distinct(hourly_data$id)
hourly_data$hour <- format(hourly_data$activity_hour, "%H:%M:%S")
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/b6db40f7-2a38-4ac4-950f-888822d615be)

### 3.4 Sleep
```{r echo = T, results = 'hide', message=FALSE}
sleep_day <- read_csv("../Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
skim_without_charts(sleep_day)
sleep_day <- clean_names(sleep_day)
sleep_day$sleep_day <- strptime(sleep_day$sleep_day, "%m/%d/%Y %I:%M:%S %p")
sleep_day <- sleep_day %>%
  mutate(weekday=weekdays(sleep_day))
sleep_day$weekday <- factor(sleep_day$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
sum(duplicated(sleep_day))
sleep_day <- sleep_day %>% 
  distinct()
sum(duplicated(sleep_day))
n_distinct(sleep_day$id)
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/f7e34053-f4d5-4651-994c-2a748c346c89)

Data on sleeping is only available from 24 of the 33 users.

### 3.5 Weight
```{r echo = T, results = 'hide', message=FALSE}
weight_log_info <- read_csv("../Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
skim_without_charts(weight_log_info)
weight_log_info <- clean_names(weight_log_info)
sum(duplicated(weight_log_info))
n_distinct(weight_log_info$id)
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/346de105-932f-4fc1-9914-7b1038a1a875)

The data frame for weight only consists of 67 rows and of data from only 8 of the 33 users. Also the column for fat has a lot of missing data with a complete_rate of only 0.299. Since the data frame won't be analysed by time, the string format of the date does not need to be changed.

### 3.6 Heart rate
```{r echo = T, results = 'hide', message=FALSE}
heartrate_seconds <- read_csv("../Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
skim_without_charts(heartrate_seconds)
heartrate_seconds <- clean_names(heartrate_seconds)
sum(duplicated(heartrate_seconds))
n_distinct(heartrate_seconds$id)
```
![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/603b9105-43f7-460d-9749-663f7cf649bc)

Data on sleeping is only available from 14 of the 33 users. Since the data frame won't be analysed by time, the string format of the date does not need to be changed.

# 4. Analyze and Share
The following things are analysed:

* 4.1 Daily activity level of users
  + 4.1.1 Active minutes per day
  + 4.1.2 Steps per day
* 4.2 Usage time of FitBit devices
  + 4.2.1 Hours per day
  + 4.2.2 Days in month
* 4.3 Health of users
  + 4.3.1 Sleep
  + 4.3.2 BMI
  + 4.3.3 Heart rate
* 4.4 Activity peaks per weekday

Since the visualizations were a central part of the analysis itself, the steps of analyzing and sharing the data are merged. Specific numbers were calculated with using the sum()-function.

### 4.1 Daily activity level of the users
Analyzing users' daily activity levels is important to gain insight into users' specific needs for fitness and wellness equipment. In this regard the data frame for daily activity differentiates between four different activity intensities and stores the daily minutes users spend in each. This type of data is stored only per day and provides the most accurate way to determine the activity level of users. 

The result can be supported by an analysis of daily steps, as it is easy to compare with the recommended amount of 8,000 steps per day for a healthy life (https://www.medicalnewstoday.com/articles/brisk-walking-1-to-2-days-a-week-reduce-all-cause-cardiovascular-mortality).

#### 4.1.1 Active minutes per day
The mean time each user spends daily in the different activity intensities are calculated and stored in a new data frame. A statistical summary is then created for an initial exploration of the data.

```{r}
mean_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(very_active=mean(very_active_minutes), fairly_active=mean(fairly_active_minutes), lightly_active=mean(lightly_active_minutes), sedentary=mean(sedentary_minutes))
summary(mean_activity %>% 
          select(-id))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/cf06f168-ce0f-424e-9be9-5a7a8af9d3b0)

The new data frame is then visualized in a plot for each activity intensity.

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/b99ee1e6-fba3-45dd-b963-30d628716503)

25 of 33 are very active between 0 and 25 minutes per day. 12 users are even very active more than 20 minutes, but 9 users are also very active less than 5 minutes per day.

30 of 33 users are fairly active between 0 and 30 minutes per day. 8 users are even fairly active more than 20 minutes, but 10 persons are also fairly active under 5 minutes per day. There are 8 overlaps between the 9 to 10 barely active users in both activity intensities.

The range of lightly activity is very large and more widely spread than the other plots. But 16 users, almost half of the 33, are lightly active between 90 and 220 minutes a day.

The plot showing the sedentary activity shows a clear divide between 14 users being sedentary between 650 and 860 minutes per day (~10-14h) and 19 users being sedentary between 1,050 and 1,350 minutes per day (~17.5-22.5h).

The analysed users of FitBit fall into a wide range of activity levels, there is no specialization in very active or inactive levels. But **the majority are either low or normal in their activity level**. Also for a majority of users **spending most of the day sedentary** most is normal.

#### 4.1.2 Steps per day
The daily mean of each user's steps is calculated and stored in a new data frame. A statistical summary is then created for an initial exploration of the data.
```{r}
daily_steps <- daily_activity %>%
  group_by(id) %>%
  summarise(mean_steps=mean(total_steps))
summary(daily_steps %>% 
          select(mean_steps))
```

![Screenshot 2023-07-13 135540](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/73af1bdd-2d85-4c9c-968f-d9da9d60c254)

The new data frame and the ratio of users reaching the goal of 8,000 steps daily is then visualized in a plot.

![Screenshot 2023-07-13 140938](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/9142ea85-f9e8-422a-a67f-c1013fc0616b)

14 reach the goal of 8,000 steps per day in mean. This means that more than half of the 33 users do not reach this goal, and also 6 users do not even reach half of 8,000 steps per day in mean. So while many users are already living healthy lives in terms of their daily steps, **a large percentage still need to improve in this regard**.

### 4.2 Usage time of fitness trackers
Analyzing the usage time of fitness devices is important to gain insights about the role they play in users' daily lives. Fitness tracker usage can be analyzed separately with our data sets in hours per day and in days for the month in which the data was collected.

#### 4.2.1 Hours per day
The most accurate way to calculate the mean hours used by each user per day is to add up the mean time of the individual activity intensities per user, which was already determined in 4.1.1. This value only needs to be converted to hours and stored in a new data frame. A statistical summary is then created for an initial exploration of the data.
```{r}
daily_usage <- daily_activity %>%
  group_by(id) %>%
  summarise(daily_hour=(mean(very_active_minutes)+mean(fairly_active_minutes)+mean(lightly_active_minutes)+mean(sedentary_minutes))/60)
summary(daily_usage %>%
          select(-id))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/22e31f25-05d7-45ec-a655-32a5fcb730ca)

The visualization resulting from this data framework can be seen below and shows a clear separation between two groups of users. 15 users use their device between 15 and 19 hours per day. The other 18 users use it more than 22 hours per day, 12 of them even more than 23 hours.

Fitness trackers are used by almost all users for most of the day, and by even half for almost the entire day. The devices are used as **everyday devices** and do not seem to be associated with any special activities.

#### 4.2.2 Days in month
The usage intensity of the wellness devices in the entire month in which the data was collected can be calculated by adding up the individual user IDs for each date. This shows on which day how many users used the devices and gives information about the usage in the long-term comparison. This figure is stored in a new data frame and supplemented by a statistical summary for an initial examination of the data.
```{r}
user_days <- daily_activity %>%
  group_by(activity_date) %>%
  summarise(users=n_distinct(id))
summary(user_days %>%
          select(-activity_date))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/2c6dfe9b-5f6e-4c79-ac1c-aa540b3cdbef)

The resulting plot from this data frame can be seen below. The device is used by the most users at the beginning, then is at a high average level during the measurement period, and drops off rapidly towards the end. **Long-term motivation appears to be an important and challenging factor** for wellness device use.

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/706e9f30-1feb-43b2-87f2-40d673b69a53)

### 4.3 Health of users
Analyzing the general health of users can provide insight into specific problems that many of them may have. Bellabeat can then respond to these problems with appropriate device functions or programs. Users' general health can be analyzed from the given data by calculating four different things: Their sleep per day, the ratio of their sleep time to their time spent in bed, their average BMI, and their average heart rate.

#### 4.3.1 Sleep
The mean hours of sleep per day, of time staying in bed, and the ratio between the two values is calculated and stored in a new data frame. A statistical summary is then created for an initial exploration of the data. Sleep of at least seven hours per day is recommended for a healthy lifestyle (https://www.mayoclinic.org/healthy-lifestyle/adult-health/expert-answers/how-many-hours-of-sleep-are-enough/faq-20057898).

```{r}
sleep <- sleep_day %>%
  group_by(id) %>%
  summarise(mean_sleep_hours=mean(total_minutes_asleep)/60, mean_bedtime_hours=mean(total_time_in_bed)/60, sleep_bedtime_relation=mean_sleep_hours/mean_bedtime_hours)
summary(sleep %>%
          select(-id))
```

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/29e5ff3a-a92c-43e7-af6f-01bb8946e7b2)

The new data frame is then visualized in a plot.

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/e4318ca3-54af-4c31-9757-9c36e6eda16a)

![Screenshot 2023-07-13 140754](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/d630dc87-c035-401c-bdd0-978aab6c4859)

There is data on the sleep of 24 of the 33 users, that is almost 3/4. 13 of that 24 users, more than half of them, sleep less than 7 hours per day. 8 users, 1/3 of them, sleep even less than 6 hours per day. 

Only two individuals appear to have very severe sleep disturbances (less than 70% of sleep time in bed), 4 individuals sleep less than 90% of their bedtime in bed.

Then the time of daily sleep is grouped by weekday and visualized in a plot.

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/1a274a24-bc4d-4a71-b815-0b928b512c08)

Problems with falling asleep do not seem to be a problem of the majority, however **too little sleep**, especially from **Sunday to Monday**.

#### 4.3.2 BMI
As it was seen in the cleaning process, there is only data for the BMI of 8 from the 33 users, making this data much more insignificant compared to the entire data set. What can be said for sure, however, is that **most users in this data set (almost 3/4) do not use their device for weight control**. The ratio of participation in BMI monitoring is visualized below.

#### 4.3.3 Heart rate
As it was seen in the cleaning process, there is only data for the heart rate of 14 from the 33 users, making this data much more insignificant compared to the entire data set. What can be said for sure, however, is that **most users in this data set (more than 1/2) do not use their device to monitor heart rate**. The ratio of participation in heart rate monitoring is visualized as a pie chart.

![Screenshot 2023-07-13 111334](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/fdd6cfa8-578f-4f69-a13f-eecb1439b74c)

### 4.4 Activity peaks per weekday
Analyzing activity peaks per weekday can be very important to motivate people at the right time. Two types of data from the hourly data set can be used for that purpose:  calories burned and the steps.

First the calories burned per hour are visualized in a plot for each weekday.

![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/82c5dea5-ef85-489a-a6ac-81dc1e4414f9)

Days seem to be very actively used when they record have more than 17500 steps per hour.

The same visualization is then done for the steps per hour for each weekday.
![image](https://github.com/aaronsabellek/Google_Data_Analytics_Bellabeat_Case_Study/assets/77847547/54bda3d7-86d3-489a-950e-e156c592d9a7)

Days seem to be very actively used when they record have more than 9000 steps per hour.

The results confirm each other. The **most actively used hours per weekday** are:

* Monday: 18:00-19:00
* Tuesday: 12:00, 17:00-19:00
* Wednesday: 17:00-19:00
* Thursday: 12:00, 14:00
* Friday: 18:00-19:00
* Saturday: 12:00-14:00
* Sunday: 10:00, 14:00

On **Tuesday, Wednesday and Saturday** the activity level is particularly high, on Sunday it is particularly low.

# 6. Act
Based on the analysis, the following six recommendations are made:

* **Adapt** the app and devices most **to the average person** who does no to about 2 hours of exercise per week
* **Fight** especially **sitting too long and promote the 8000 steps per day** challenge
* Focus a lot on the **long-term motivation of users**
* Empower women to **monitor their BMI and heart rate**
* Focus on empowering users to **get enough sleep**, especially on Sundays
* Create **motivational programs using special days and times** with generally high activity levels
* **Collect more data** to gain a deeper understanding of smart device usage trends
