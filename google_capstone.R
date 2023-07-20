# data is downloaded from https://www.kaggle.com/datasets/arashnic/fitbit

# load packages
library("here")
library("skimr")
library("janitor")
library("tidyverse")
library("dplyr")
library("cowplot")
library("formattable")

# load and clean data of daily activity
daily_activity <- read_csv("dailyActivity_merged.csv")
skim_without_charts(daily_activity)
daily_activity <- clean_names(daily_activity)
daily_activity$activity_date <- as.Date(daily_activity$activity_date, format="%m/%d/%Y")
sum(duplicated(daily_activity))
n_distinct(daily_activity$id)
daily_activity <- daily_activity %>%
  mutate(weekday=weekdays(activity_date))
daily_activity$weekday <- factor(daily_activity$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))

glimpse(daily_activity)

# load and clean data of hourly calories
hourly_calories <- read_csv("hourlyCalories_merged.csv")
skim_without_charts(hourly_calories)
hourly_calories <- clean_names(hourly_calories)
hourly_calories$activity_hour <- strptime(hourly_calories$activity_hour, "%m/%d/%Y %I:%M:%S %p")
sum(duplicated(hourly_calories))
n_distinct(hourly_calories$id)
hourly_calories <- hourly_calories %>%
  mutate(weekday=weekdays(activity_hour))
hourly_calories$weekday <- factor(hourly_calories$weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
glimpse(hourly_calories)

# load and clean data of hourly steps
hourly_steps <- read_csv("../Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
skim_without_charts(hourly_steps)
hourly_steps <- clean_names(hourly_steps)
hourly_steps$activity_hour <- strptime(hourly_steps$activity_hour, "%m/%d/%Y %I:%M:%S %p")
sum(duplicated(hourly_steps))
n_distinct(hourly_steps$id)
glimpse(hourly_steps)

# merge hourly steps with hourly calories
hourly_data <- merge(hourly_calories, hourly_steps, by=c("id", "activity_hour"), all = TRUE)
n_distinct(hourly_data$id)
hourly_data$hour <- format(hourly_data$activity_hour, "%H:%M:%S")
glimpse(hourly_data)

# load and clean data of sleep
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
glimpse(sleep_day)

# load and clean data of weight
weight_log_info <- read_csv("../Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
skim_without_charts(weight_log_info)
weight_log_info <- clean_names(weight_log_info)
sum(duplicated(weight_log_info))
n_distinct(weight_log_info$id)
glimpse(weight_log_info)

# load and clean data of heart rate
heartrate_seconds <- read_csv("../Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
skim_without_charts(heartrate_seconds)
heartrate_seconds <- clean_names(heartrate_seconds)
sum(duplicated(heartrate_seconds))
n_distinct(heartrate_seconds$id)
glimpse(heartrate_seconds)

# analyze and summarize daily activity per user
mean_activity <- daily_activity %>%
  group_by(id) %>%
  summarize(very_active=mean(very_active_minutes), fairly_active=mean(fairly_active_minutes), lightly_active=mean(lightly_active_minutes), sedentary=mean(sedentary_minutes))
summary(mean_activity %>% 
          select(-id))

# visualize daily activity per user
plot_very_active <- ggplot(data=mean_activity)+
  geom_point(mapping=aes(x=id, y=very_active))+
  xlab("user ID")+
  ylab("very active minutes per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_fairly_active <- ggplot(data=mean_activity)+
  geom_point(mapping=aes(x=id, y=fairly_active))+
  xlab("user ID")+
  ylab("Fairly active minutes per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_lightly_active <- ggplot(data=mean_activity)+
  geom_point(mapping=aes(x=id, y=lightly_active))+
  xlab("user ID")+
  ylab("Lightly active minutes per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_sedentary <- ggplot(data=mean_activity)+
  geom_point(mapping=aes(x=id, y=sedentary))+
  xlab("user ID")+
  ylab("Sedentary minutes per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_grid(plot_very_active, plot_fairly_active, plot_lightly_active, plot_sedentary)

# analyze and summarize daily steps per user
daily_steps <- daily_activity %>%
  group_by(id) %>%
  summarise(mean_steps=mean(total_steps))
summary(daily_steps %>% 
          select(mean_steps))

# visualize daily steps per user
daily_steps_plot <- ggplot(data=daily_steps)+
  geom_point(mapping=aes(x=id, y=mean_steps))+
  xlab("user ID")+
  ylab("Mean steps per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

daily_steps_goal <- data.frame(
  steps=c("more than 8,000 steps", "less than 8,000 steps"),
  users=c(sum(daily_steps$mean_steps >= 8000), sum(daily_steps$mean_steps < 8000))
)

steps_goal_plot <- ggplot(daily_steps_goal, aes(x="", y=users, fill=steps))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=c("lightblue", "aliceblue"))+
  geom_text(aes(y = users/2 + c(0, cumsum(users)[-length(users)]),
                label = percent(users/sum(n_distinct(daily_activity$id)))), size=3)+
  theme_void()

plot_grid(daily_steps_plot, steps_goal_plot)

# analyze and summarize daily duration of device usage
daily_usage <- daily_activity %>%
  group_by(id) %>%
  summarise(daily_hour=(mean(very_active_minutes)+mean(fairly_active_minutes)+mean(lightly_active_minutes)+mean(sedentary_minutes))/60)
summary(daily_usage %>%
          select(-id))

# analyze and summarize used days of devices in month
user_days <- daily_activity %>%
  group_by(activity_date) %>%
  summarise(users=n_distinct(id))
summary(user_days %>%
          select(-activity_date))

# visualize daily and monthly usage
usage_days <- ggplot(data=user_days)+
  geom_line(mapping=aes(x=activity_date, y=users))+
  xlab("Date")+
  ylab("Active users")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

usage_hours <- ggplot(data=daily_usage)+
  geom_point(mapping=aes(x=id, y=daily_hour))+
  xlab("user ID")+
  ylab("Hours used per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_grid(usage_hours, usage_days)

# analyze and summarize daily sleep, time laying in bed and ratio between both values per user
sleep <- sleep_day %>%
  group_by(id) %>%
  summarise(mean_sleep_hours=mean(total_minutes_asleep)/60, mean_bedtime_hours=mean(total_time_in_bed)/60, sleep_bedtime_relation=mean_sleep_hours/mean_bedtime_hours)
summary(sleep %>%
          select(-id))

# visualize daily sleep, time laying in bed and ratio between both values per user
sleep_hours <- ggplot(data=sleep)+
  geom_point(mapping=aes(x=id, y=mean_sleep_hours))+
  xlab("user ID")+
  ylab("Hours of sleep per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

sleep_bedtime <- ggplot(data=sleep)+
  geom_point(mapping=aes(x=id, y=sleep_bedtime_relation))+
  xlab("user ID")+
  ylab("Relation of sleep to bedtime per day")+
  theme(panel.background=element_rect(fill="lightblue",
                                      colour="lightblue",
                                      linetype="solid"))

plot_grid(sleep_hours, sleep_bedtime)

sleep_ratio_1 <- data.frame(
  sleeptime=c("no information", "more than 7 hours", "less than 7 hours"),
  users=c(sum(n_distinct(daily_steps$id))-sum(n_distinct(sleep$id)), sum(sleep$mean_sleep_hours >= 7, na.rm=T), sum(sleep$mean_sleep_hours < 7, na.rm=T))
)

sleep_ratio_2 <- data.frame(
  sleeptime=c("more than 7 hours", "less than 7 hours"),
  users=c(sum(sleep$mean_sleep_hours >= 7, na.rm=T), sum(sleep$mean_sleep_hours < 7, na.rm=T))
)

sleep_pie_1 <- ggplot(sleep_ratio_1, aes(x="", y=users, fill=sleeptime))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0)+
  ggtitle("Every user")+
  geom_text(aes(y = users/2 + c(0, cumsum(users)[-length(users)]),
                label = percent(users/sum(n_distinct(daily_activity$id)))), size=3)+
  scale_fill_manual(values=c("lightblue", "aliceblue", "steelblue"))+
  theme_void()

sleep_pie_2 <- ggplot(sleep_ratio_2, aes(x="", y=users, fill=sleeptime))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0)+
  ggtitle("Only users with sleep tracking")+
  geom_text(aes(y = users/2 + c(0, cumsum(users)[-length(users)]),
                label = percent(users/sum(n_distinct(sleep$id)))), size=3)+
  scale_fill_manual(values=c("lightblue", "aliceblue"))+
  theme_void()

plot_grid(sleep_pie_1, sleep_pie_2)

# visualize daily sleep by weekday
ggplot(data=sleep_day, aes(x=weekday, y=total_minutes_asleep/60))+
  geom_bar(stat="identity", fill="lightblue")+
  xlab("Weekday")+
  ylab("Hours of sleep")+
  theme(panel.background=element_rect(fill="white",
                                      colour="white",
                                      linetype="solid"))

# analyze usage of BMI monitoring
bmi_participation <- data.frame(
  status=c("participating", "not participating"),
  users=c(sum(n_distinct(weight_log_info$id)), sum(n_distinct(daily_activity$id))-sum(n_distinct(weight_log_info$id)))
)

# analyze usage of heart rate monitoring
hr_participation <- data.frame(
  status=c("participating", "not participating"),
  users=c(sum(n_distinct(heartrate_seconds$id)), sum(n_distinct(daily_activity$id))-sum(n_distinct(heartrate_seconds$id)))
)

# visualize usage of BMI and heart rate monitoring
bmi_plot <- ggplot(bmi_participation, aes(x="", y=users, fill=status))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0)+
  ggtitle("BMI monitoring")+
  geom_text(aes(y = users/2 + c(0, cumsum(users)[-length(users)]),
                label = percent(users/sum(n_distinct(daily_activity$id)))), size=3)+
  scale_fill_manual(values=c("lightblue", "aliceblue"))+
  theme_void()

hr_plot <- ggplot(hr_participation, aes(x="", y=users, fill=status))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar("y", start=0)+
  ggtitle("Heart rate monitoring")+
  geom_text(aes(y = users/2 + c(0, cumsum(users)[-length(users)]),
                label = percent(users/sum(n_distinct(daily_activity$id)))), size=3)+
  scale_fill_manual(values=c("lightblue", "aliceblue"))+
  theme_void()

plot_grid(bmi_plot, hr_plot)

# create a function to show only every second label tick on the x-axis
everysecond <- function(x){
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

# visualize activity peaks per weekday with calories
ggplot(data=hourly_data, aes(x=hour, y=calories))+
  geom_bar(stat="identity", fill="lightblue")+
  xlab("Hour")+
  ylab("Calories")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid=element_line(color="grey"),
        panel.background=element_rect(fill="white",
                                      colour="white",
                                      linetype="solid"),
        strip.background=element_rect(fill="white"))+
  scale_x_discrete(labels=everysecond(hourly_data$hour))+
  facet_wrap(~weekday, ncol=2)

# visualize activity peaks per weekday with steps
ggplot(data=hourly_data, aes(x=hour, y=step_total))+
  geom_bar(stat="identity", fill="lightblue")+
  xlab("Hour")+
  ylab("Steps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid=element_line(color="grey"),
        panel.background=element_rect(fill="white",
                                      colour="white",
                                      linetype="solid"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=everysecond(hourly_data$hour))+
  facet_wrap(~weekday, ncol=2)
