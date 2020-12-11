# ----------------------------------------
# EDA R Script
# STAT 310-1
# Diego Williams
# ----------------------------------------

# ----------------------------------------
## Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
# ----------------------------------------

# ----------------------------------------
# Note: working with the following data items from `data_cleaning.R`:
# `accidents_cleaned`, `distractions_cleaned`, `people_cleaned`, and
# `vehicles_cleaned`; make sure to create them properly
# ----------------------------------------

# ----------------------------------------
## Loading the .rds files from the `processed` directory
accidents_cleaned <- read_rds("data/processed/accidents_cleaned.rds")

distractions_cleaned <- read_rds("data/processed/distractions_cleaned.rds")

people_cleaned <- read_rds("data/processed/people_cleaned.rds")

vehicles_cleaned <- read_rds("data/processed/vehicles_cleaned.rds")
# ----------------------------------------

# ----------------------------------------
## High-Level `accidents_cleaned` Exploration

# Looking at `region`
accidents_cleaned %>%
  group_by(region) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(accidents_cleaned) +
  geom_bar(aes(x = region, fill = region)) +
  theme(legend.position = "none")

# Looking at `month` and `wday`
# `month`
accidents_cleaned %>% 
  group_by(month) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

# `month` barplot
ggplot(accidents_cleaned) +
  geom_bar(aes(x = month))

# `wday`
accidents_cleaned %>% 
  group_by(wday) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n))

# `wday` barplot
ggplot(accidents_cleaned) +
  geom_bar(aes(x = wday))

# `wday` barplot faceted by `month`
ggplot(accidents_cleaned) +
  geom_bar(aes(x = wday)) +
  facet_wrap(~month)

# Looking at `light_con` and `weather`
accidents_cleaned %>% 
  group_by(light_con) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))

accidents_cleaned %>% 
  group_by(weather) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))
  
# Looking at `time` and `light_con` in a visualization
ggplot(accidents_cleaned) +
  geom_histogram(aes(x = time, fill = light_con), bins = 48) +
  annotate("rect", xmin = 21600, xmax = 36000, ymin = 0, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 54000, xmax = 68400, ymin = 0, ymax = Inf, 
           alpha = .2) +
  labs(x = "Time of Accident", y = "Count", fill = "Reported Light Condition")

# Looking at `weather` and `month`
accidents_cleaned %>% 
  count(weather) %>% 
  arrange(desc(n))

accidents_cleaned %>% 
  filter(weather == "Clear" | weather == "Cloudy" | weather == "Rain"
         | weather == "Not Reported/Reported as Unknown" | weather == "Snow" |
           weather == "Fog, Smog, Smoke") %>% 
  # Keeping six most frequent conditions to avoid overwhelming legend
  ggplot() +
  geom_bar(aes(x = month, fill = weather))

# Looking at the `manner_coll` and `crash_max_sev`
# `manner_coll` summary and barplot
accidents_cleaned %>% 
  group_by(manner_coll) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n)) %>% 
  mutate(manner_coll = fct_reorder(manner_coll, n, .desc = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x = manner_coll, y = n)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# `crash_max_sev` summary and barplot
accidents_cleaned %>% 
  group_by(crash_max_sev) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n)) %>% 
  mutate(crash_max_sev = fct_reorder(crash_max_sev, n, .desc = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x = crash_max_sev, y = n)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# geom_tile with both `manner_coll` and `crash_max_sev`
accidents_cleaned %>% 
  count(manner_coll, crash_max_sev) %>% 
  arrange(desc(n))

accidents_cleaned %>% 
  count(manner_coll, crash_max_sev) %>% 
  ggplot(aes(x = crash_max_sev, y = manner_coll)) +
  geom_tile(aes(fill = n)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Maximum Injury Severity in Crash", y = "Manner of Collision")
  
# ----------------------------------------

# ----------------------------------------
## Pulling in `distractions_cleaned` data into `people_cleaned`
## and generally looking through `people_cleaned`

# Taking a peek at `age` and it's relationship with `injury_sev`
mean(people_cleaned$age)

people_cleaned %>%
  mutate(injury_sev = fct_infreq(injury_sev)) %>%
  ggplot() +
  geom_boxplot(aes(x = injury_sev, y = age)) +
  geom_hline(yintercept = 37.37703, size = 2, color = "grey") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Peeking at the distribution of injuries by reported `sex`
people_cleaned %>% 
  mutate(injury_sev = fct_infreq(injury_sev)) %>% 
  filter(sex != "Not Reported/Unknown") %>% 
  count(sex, injury_sev) %>% 
  group_by(sex) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = injury_sev, y = prop)) +
  facet_wrap(~sex) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # Seems pretty even across reported `sex`
  
# Let's try the same analysis we did with reported `sex` with `person_type`
people_cleaned %>% 
  mutate(injury_sev = fct_infreq(injury_sev)) %>% 
  count(person_type) %>% 
  arrange(desc(n))

people_cleaned %>% 
  mutate(injury_sev = fct_infreq(injury_sev)) %>% 
  filter(person_type != "Persons on Personal Conveyances",
         person_type != "Unknown in a Motor Vehicle in Transport",
         person_type != "Other Cyclist ",
         person_type != "Persons in or on Buildings") %>% 
  count(person_type, injury_sev) %>% 
  group_by(person_type) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = injury_sev, y = prop)) +
  facet_wrap(~person_type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Injury Severity", y = "Proportion of Injuries")

# Let's join in the data from `distractions`
people_joined <- people_cleaned %>% 
  inner_join(distractions_cleaned, by = c("case_num", "vehicle_num"))

# Let's look at the most common distractions
people_joined %>% 
  group_by(driver_distraction) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

# Let's keep the 9 most common ones and build a visualization

people_joined %>% 
  filter(driver_distraction == "Not Distracted"|
         driver_distraction == "Not Reported/Unknown"|
         driver_distraction == "Inattention, Details Unknown"|
         driver_distraction == "Careless/Inattentive"|
         driver_distraction == "Distracted By Outside Person, Object Or Event"|
         driver_distraction == "Distraction, Details Unknown"|
         driver_distraction == "While Using Or Reaching For Device/Object Brought into Vehicle"|
         driver_distraction == "By Other Occupants"|
         driver_distraction == "Other Distraction") %>% 
  mutate(injury_sev = fct_infreq(injury_sev)) %>% 
  count(driver_distraction, injury_sev) %>% 
  group_by(driver_distraction) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot() +
  geom_col(aes(x = injury_sev, y = prop)) +
  facet_wrap(~driver_distraction) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Type of Driver Distraction", y = "Proportion of Injuries")

# Seems extremely consistent across distraction categories! However, high degree
# of `Not Reported/Unknown` here IS worth noting

# Out of curiosity: let's look at the age range for each of the
# 9 most common distractions
people_joined %>% 
  filter(driver_distraction == "Not Distracted"|
           driver_distraction == "Not Reported/Unknown"|
           driver_distraction == "Inattention, Details Unknown"|
           driver_distraction == "Careless/Inattentive"|
           driver_distraction == "Distracted By Outside Person, Object Or Event"|
           driver_distraction == "Distraction, Details Unknown"|
           driver_distraction == "While Using Or Reaching For Device/Object Brought into Vehicle"|
           driver_distraction == "By Other Occupants"|
           driver_distraction == "Other Distraction") %>%
  mutate(driver_distraction = fct_reorder(driver_distraction, age)) %>% 
  group_by(driver_distraction) %>% 
  ggplot() +
  geom_boxplot(aes(x = driver_distraction, y = age)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Type of Driver Distraction", y = "Age of Person")
  
# ----------------------------------------

# ----------------------------------------
## Looking through `vehicles_cleaned`

# Looking at the distrbution of `speed` in the `vehicles_cleaned` data item
vehicles_cleaned %>% 
  mutate(speed = fct_collapse(speed,
                              `Not Reported/Unknown` = "Not Reported/Unknown",
                              `1-10 mph` = "1-10 mph",
                              `11-20 mph` = "11-20 mph",
                              `21-30 mph` = "21-30 mph",
                              `31-40 mph` = "31-40 mph",
                              `41-50 mph` = "41-50 mph",
                              `51-60 mph` = "51-60 mph",
                              `61-70 mph` = "61-70 mph",
                              `71-80 mph` = "71-80 mph",
                              `81-90 mph` = "81-90 mph",
                              `91-100 mph` = "91-100 mph",
                              `Greater than 100 mph` = c("101-110 mph",
                                                         "111-120 mph",
                                                         "121-130 mph",
                                                         "131-140 mph",
                                                         "Greater than 151 mph"))) %>% 
  mutate(speed = fct_relevel(speed, "91-100 mph", after = 11)) %>% 
  mutate(speed = fct_relevel(speed, "Greater than 100 mph", after = 12)) %>% 
  mutate(speed = fct_relevel(speed, "Not Reported/Unknown", after = 0)) %>% 
  ggplot() +
  geom_bar(aes(x = speed)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # Alarmingly high proportion of unknown values here

# Looking at the distribution of `vehicle_make`
vehicles_cleaned %>% 
  group_by(vehicle_make) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n)) %>% 
  print(n = 67)
  # Pretty evenly-spread distribution here;
  # let's stick with ~ top 20 makes moving forwards

# Creating the lists for the top 20 and top 9 makes for joining purposes
top_20_makes <- vehicles_cleaned %>% 
  group_by(vehicle_make) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 20) %>% 
  select(vehicle_make)

top_9_makes <- vehicles_cleaned %>% 
  group_by(vehicle_make) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 9) %>% 
  select(vehicle_make)

# Looking at the distribution of `vehicle_make` and `dmg_extent` together
# geom_tile() for top 20 makes
vehicles_cleaned %>% 
  inner_join(top_20_makes, by = "vehicle_make") %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Disabling Damage", after = 2)) %>%
  mutate(dmg_extent = fct_relevel(dmg_extent, "Functional Damage", after = 1)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "No Damage", after = 0)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Not Reported/Unknown", after = 0)) %>% 
  count(vehicle_make, dmg_extent) %>% 
  group_by(vehicle_make) %>% 
  mutate(prop_for_make = n/sum(n)) %>% 
  ggplot(aes(x = dmg_extent, y = vehicle_make)) +
  geom_tile(aes(fill = prop_for_make)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# geom_col for top 9 makes, faceted
vehicles_cleaned %>% 
  inner_join(top_9_makes, by = "vehicle_make") %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Disabling Damage", after = 2)) %>%
  mutate(dmg_extent = fct_relevel(dmg_extent, "Functional Damage", after = 1)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "No Damage", after = 0)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Not Reported/Unknown", after = 0)) %>% 
  count(vehicle_make, dmg_extent) %>% 
  group_by(vehicle_make) %>% 
  mutate(prop_for_make = n/sum(n)) %>% 
  ggplot(aes(x = dmg_extent, y = prop_for_make)) +
  geom_col() +
  facet_wrap(~vehicle_make) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Checking general pattern now
vehicles_cleaned %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Disabling Damage", after = 2)) %>%
  mutate(dmg_extent = fct_relevel(dmg_extent, "Functional Damage", after = 1)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "No Damage", after = 0)) %>% 
  mutate(dmg_extent = fct_relevel(dmg_extent, "Not Reported/Unknown", after = 0)) %>% 
  ggplot() +
  geom_bar(aes(x = dmg_extent)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Looking at the distribution of `vehicle_make` and `vehicle_max_sev`
# Just looking at `vehicle_max_sev`
vehicles_cleaned %>% 
  mutate(vehicle_max_sev = fct_infreq(vehicle_max_sev)) %>% 
  ggplot() +
  geom_bar(aes(x = vehicle_max_sev)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# geom_tile() with top 20 makes
vehicles_cleaned %>% 
  inner_join(top_20_makes, by = "vehicle_make") %>% 
  mutate(vehicle_max_sev = fct_infreq(vehicle_max_sev)) %>% 
  count(vehicle_make, vehicle_max_sev) %>% 
  group_by(vehicle_make) %>% 
  mutate(prop_for_make = n/sum(n)) %>% 
  ggplot(aes(x = vehicle_max_sev, y = vehicle_make)) +
  geom_tile(aes(fill = prop_for_make)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# faceted barplot with top 9 makes
vehicles_cleaned %>% 
  inner_join(top_9_makes, by = "vehicle_make") %>% 
  mutate(vehicle_max_sev = fct_infreq(vehicle_max_sev)) %>% 
  count(vehicle_make, vehicle_max_sev) %>% 
  group_by(vehicle_make) %>% 
  mutate(prop_for_make = n/sum(n)) %>% 
  ggplot(aes(x = vehicle_max_sev, y = prop_for_make)) +
  geom_col() +
  facet_wrap(~vehicle_make) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------------------------------------