# ----------------------------------------
# Data Cleaning
# STAT 310-1
# Diego Williams
# ----------------------------------------

# ----------------------------------------
## Load Packages
library(tidyverse)
library(lubridate)
# ----------------------------------------

# ----------------------------------------
# Note: working with the following data items from `data_collection.R`:
# `accidents`, `distractions`, `people`, and `vehicles`
# ----------------------------------------

# ----------------------------------------
## Cleaning the `accidents` item
accidents_selected <- accidents %>% 
  # Selecting relevant variables
  select(1, 2, 8, 14:15, 17:19, 22, 23, 29, 32)

# Renaming variables to follow `snakecase` naming conventions
accidents_renamed <- accidents_selected %>% 
  rename(
    case_num = CASENUM,
    region = REGION,
    num_vehicles = VE_TOTAL,
    num_injured = NUM_INJ,
    month = MONTH,
    wday = DAY_WEEK,
    hour = HOUR,
    min = MINUTE,
    crash_max_sev = MAX_SEV,
    manner_coll = MAN_COLL,
    light_con = LGT_COND,
    weather = WEATHER
  )

# Consolidating date/time variables
# (ISSUE: No date provided in data item;
# SOLUTION: Combine `hour` and `min`, but convert `month` and `wday` to factors)
accidents_renamed <- accidents_renamed %>% 
  # Creating a single variable to store the `hour` and `min` data
  filter(hour != 99, min != 99) %>% 
  mutate(
    seconds_since_midnight = (hour*60*60) + (min * 60),
    time = hms::as_hms(seconds_since_midnight)
  ) %>% 
  select(1:6, 9:12, 14) %>% 
  # Converting `month` and `wday` to the appropriate data types
  mutate(
    month = month(month, label = TRUE),
    wday = wday(wday, label = TRUE)
  )

# Converting all of the variables to the appropriate type
accidents_renamed <- accidents_renamed %>% 
  mutate(
    case_num = as.character(case_num),
    region = as_factor(region),
    crash_max_sev = as_factor(crash_max_sev),
    manner_coll = as_factor(manner_coll),
    light_con = as_factor(light_con),
    weather = as_factor(weather))

# Using `case_when()` to replace factors with actual values
accidents_cleaned <- accidents_renamed %>%
  mutate(
    region = case_when(
      region == 4 ~ "West",
      region == 3 ~ "South",
      region == 2 ~ "Midwest",
      region == 1 ~ "Northeast"),
    crash_max_sev = case_when(
      crash_max_sev == 4 ~ "Fatal",
      crash_max_sev == 3 ~ "Suspected Serious Injury",
      crash_max_sev == 2 ~ "Suspected Minor Injury",
      crash_max_sev == 1 ~ "Possible Injury",
      crash_max_sev == 5 ~ "Injured, Severity Unknown",
      crash_max_sev == 0 ~ "No Apparent Injury",
      crash_max_sev == 6 ~ "Died Prior to Crash",
      crash_max_sev == 9 ~ "Unknown/Not Reported",
      crash_max_sev == 8 ~ "No Person Involved in Crash"),
    manner_coll = case_when(
      manner_coll == 0 ~ "Not Collision with Motor Vehicle",
      manner_coll == 1 ~ "Front-to-Rear",
      manner_coll == 2 ~ "Front-to-Front",
      manner_coll == 6 ~ "Angle",
      manner_coll == 7 ~ "Sideswipe, Same Direction",
      manner_coll == 8 ~ "Sideswipe, Opposite Direction",
      manner_coll == 9 ~ "Rear-to-Side",
      manner_coll == 10 ~ "Rear-to-Rear",
      manner_coll == 11 ~ "Other",
      manner_coll == 98 ~ "Not Reported/Reported as Unknown",
      manner_coll == 99 ~ "Not Reported/Reported as Unknown"),
    light_con = case_when(
      light_con == 1 ~ "Daylight",
      light_con == 2 ~ "Dark – Not Lighted",
      light_con == 3 ~ "Dark – Lighted",
      light_con == 4 ~ "Dawn",
      light_con == 5 ~ "Dusk",
      light_con == 6 ~ "Dark – Unknown Lighting",
      light_con == 7 ~ "Other",
      light_con == 8 ~ "Not Reported/Reported as Unknown",
      light_con == 9 ~ "Not Reported/Reported as Unknown"),
    weather = case_when(
      weather == 0 ~ "No Additional Atmospheric Conditions",
      weather == 1 ~ "Clear",
      weather == 2 ~ "Rain",
      weather == 3 ~ "Sleet or Hail",
      weather == 4 ~ "Snow",
      weather == 5 ~ "Fog, Smog, Smoke",
      weather == 6 ~ "Severe Crosswinds",
      weather == 7 ~ "Blowing Sand, Soil, Dirt",
      weather == 8 ~ "Other",
      weather == 10 ~ "Cloudy",
      weather == 11 ~ "Blowing Snow",
      weather == 12 ~ "Freezing Rain or Drizzle",
      weather == 98 ~ "Not Reported/Reported as Unknown",
      weather == 99 ~ "Not Reported/Reported as Unknown")) %>% 
  filter(num_injured <= 18)
  
# --------------------
## Cleaning the `distractions` item
distractions_selected <- distractions %>% 
  # Selecting relevant variables
  select(1, 2, 9)

# Renaming variables to follow `snakecase` naming conventions
distractions_renamed <- distractions_selected %>% 
  rename(
    case_num = CASENUM,
    vehicle_num = VEH_NO,
    driver_distraction = MDRDSTRD
  )

# Converting all of the variables to the appropriate type
distractions_renamed <- distractions_renamed %>% 
  mutate(
    case_num = as.character(case_num),
    vehicle_num = as.character(vehicle_num),
    driver_distraction = as_factor(driver_distraction)
  )

# Using `case_when()` to replace factors with actual values
distractions_cleaned <- distractions_renamed %>% 
  mutate(
    driver_distraction = case_when(
      driver_distraction == 0 ~ "Not Distracted",
      driver_distraction == 3 ~ "By Other Occupants",
      driver_distraction == 4 ~ "By a Moving Object In Vehicle",
      driver_distraction == 5 ~ "Talking/Listening on Cell Phone",
      driver_distraction == 6 ~ "Manipulating Cell Phone",
      driver_distraction == 7 ~ "While Adjusting Audio Or Climate Controls",
      driver_distraction == 9 ~ "While Using Other Component/Controls Integral To Vehicle",
      driver_distraction == 10 ~ "While Using Or Reaching For Device/Object Brought into Vehicle",
      driver_distraction == 12 ~ "Distracted By Outside Person, Object Or Event",
      driver_distraction == 13 ~ "Eating Or Drinking",
      driver_distraction == 14 ~ "Smoking Related",
      driver_distraction == 15 ~ "Other Cellular Phone Related",
      driver_distraction == 16 ~ "No Driver Present/Unknown if Driver Present",
      driver_distraction == 17 ~ "Careless/Inattentive",
      driver_distraction == 18 ~ "Careless/Inattentive",
      driver_distraction == 19 ~ "Careless/Inattentive",
      driver_distraction == 92 ~ "Distraction, Details Unknown",
      driver_distraction == 93 ~ "Inattention, Details Unknown",
      driver_distraction == 96 ~ "Not Reported/Unknown",
      driver_distraction == 98 ~ "Other Distraction",
      driver_distraction == 99 ~ "Not Reported/Unknown"))

# --------------------
## Cleaning the `people` item
people_selected <- people %>% 
  # Selecting relevant variables
  select(1, 3, 4, 28:32)

# Renaming variables to follow `snakecase` naming conventions
people_renamed <- people_selected %>% 
  rename(
    case_num = CASENUM,
    vehicle_num = VEH_NO,
    person_num = PER_NO,
    age = AGE,
    sex = SEX,
    person_type = PER_TYP,
    injury_sev = INJ_SEV,
    seat_pos = SEAT_POS
  )

# Converting all of the variables to the appropriate type
people_renamed <- people_renamed %>% 
  mutate(
    case_num = as.character(case_num),
    vehicle_num = as.character(vehicle_num),
    age = as.numeric(age),
    sex = as_factor(sex),
    person_type = as_factor(person_type),
    injury_sev = as_factor(injury_sev),
    seat_pos = as_factor(seat_pos)
  ) %>% 
  filter(age <= 120)

# Using `case_when()` to replace factors with actual values
people_cleaned <- people_renamed %>% 
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      sex == 8 ~ "Not Reported/Unknown",
      sex == 9 ~ "Not Reported/Unknown"),
    person_type = case_when(
      person_type == 1 ~ "Driver of a Motor Vehicle in Transport",
      person_type == 2 ~ "Passenger of a Motor Vehicle in Transport",
      person_type == 9 ~ "Unknown in a Motor Vehicle in Transport",
      person_type == 3 ~ "Occupant of a Motor Vehicle Not in Transport",
      person_type == 4 ~ "Occupant of a Non-Motor Vehicle Transport Device",
      person_type == 5 ~ "Pedestrian",
      person_type == 6 ~ "Bicyclist",
      person_type == 7 ~ "Other Cyclist",
      person_type == 8 ~ "Persons on Personal Conveyances",
      person_type == 10 ~ "Persons in or on Buildings",
      person_type == 19 ~ "Unknown Type of Non-Motorist"),
    injury_sev = case_when(
      injury_sev == 4 ~ "Fatal",
      injury_sev == 3 ~ "Suspected Serious Injury",
      injury_sev == 2 ~ "Suspected Minor Injury",
      injury_sev == 1 ~ "Possible Injury",
      injury_sev == 5 ~ "Injured, Severity Unknown",
      injury_sev == 0 ~ "No Apparent Injury",
      injury_sev == 6 ~ "Died Prior to Crash",
      injury_sev == 9 ~ "Unknown/Not Reported")) %>% 
  select(-seat_pos)

# --------------------
## Cleaning the `vehicles` item
vehicles_selected <- vehicles %>% 
  # Selecting relevant variables
  select(1, 2, 10, 14, 15, 18:21, 40, 43, 44, 46, 50)

# Renaming variables to follow `snakecase` naming conventions
vehicles_renamed <- vehicles_selected %>% 
  rename(
    case_num = CASENUM,
    vehicle_num = VEH_NO,
    num_occs = NUMOCCS,
    first_harm_ev_c = HARM_EV,
    manner_coll = MAN_COLL,
    vehicle_make = MAKE,
    vehicle_model = MODEL,
    vehicle_body = BODY_TYP,
    vehicle_mod_yr = MOD_YEAR,
    speed = TRAV_SP,
    initial_impact = IMPACT1,
    dmg_extent = DEFORMED,
    vehicle_most_harm_ev = M_HARM,
    vehicle_max_sev = MAX_VSEV)

# Converting all of the variables to the appropriate type
vehicles_renamed <- vehicles_renamed %>% 
  mutate(
    case_num = as.character(case_num),
    num_occs = as.numeric(num_occs),
    manner_coll = as_factor(manner_coll),
    vehicle_make = as_factor(vehicle_make),
    vehicle_body = as_factor(vehicle_body),
    vehicle_mod_yr = as.numeric(vehicle_mod_yr),
    speed = as.numeric(speed),
    initial_impact = as_factor(initial_impact),
    dmg_extent = as_factor(dmg_extent),
    vehicle_max_sev = as_factor(vehicle_max_sev)) %>% 
  filter(num_occs < 99) %>% 
  # Dropping problematic/extremely time-consuming variables
  select(-first_harm_ev_c, -vehicle_most_harm_ev, -vehicle_model, -vehicle_body)

# Checking the `speed variable` before proceeding
checking_speed <- vehicles_renamed %>%
  group_by(speed) %>% 
  summarize(
    n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))

# Based off of this observation, we should convert `speed` to a factor
vehicles_renamed <- vehicles_renamed %>% 
  mutate(speed = as_factor(speed))
  # We can use fct_collapse to create different categories;
  # and probably fct_reorder() too

# Checking list of `vehicle_make` and `vehicle_body` variable values
# (In order to determine whether or not to include said variables)
vehicle_make_list<- vehicles_renamed %>% 
  group_by(vehicle_make) %>% 
  summarize(
    count = n()
  ) %>% 
  arrange(desc(count))

# vehicle_body_list<- vehicles_renamed %>% 
  # group_by(vehicle_body) %>% 
  # summarize(
  #   count = n()
  # ) %>% 
  # arrange(desc(count))

# Using `case_when()` to replace factors with actual values
vehicles_cleaned <- vehicles_renamed %>% 
  mutate(
    manner_coll = case_when(
      manner_coll == 0 ~ "Not Collision with Motor Vehicle",
      manner_coll == 1 ~ "Front-to-Rear",
      manner_coll == 2 ~ "Front-to-Front",
      manner_coll == 6 ~ "Angle",
      manner_coll == 7 ~ "Sideswipe, Same Direction",
      manner_coll == 8 ~ "Sideswipe, Opposite Direction",
      manner_coll == 9 ~ "Rear-to-Side",
      manner_coll == 10 ~ "Rear-to-Rear",
      manner_coll == 11 ~ "Other",
      manner_coll == 98 ~ "Not Reported/Reported as Unknown",
      manner_coll == 99 ~ "Not Reported/Reported as Unknown"),
    vehicle_make = case_when(
      vehicle_make == 1 ~ "American Motors",
      vehicle_make == 2 ~ "Jeep/Kaiser-Jeep/Willys-Jeep",
      vehicle_make == 3 ~ "AM General",
      vehicle_make == 6 ~ "Chrysler",
      vehicle_make == 7 ~ "Dodge",
      vehicle_make == 9 ~ "Plymouth",
      vehicle_make == 10 ~ "Eagle",
      vehicle_make == 12 ~ "Ford",
      vehicle_make == 13 ~ "Lincoln",
      vehicle_make == 14 ~ "Mercury",
      vehicle_make == 18 ~ "Buick/Opel",
      vehicle_make == 19 ~ "Cadillac",
      vehicle_make == 20 ~ "Chevrolet",
      vehicle_make == 21 ~ "Oldsmobile",
      vehicle_make == 22 ~ "Pontiac",
      vehicle_make == 23 ~ "GMC",
      vehicle_make == 24 ~ "Saturn",
      vehicle_make == 25 ~ "Grumman",
      vehicle_make == 29 ~ "Other Domestic Manufacturers",
      vehicle_make == 30 ~ "Volkswagen",
      vehicle_make == 31 ~ "Alfa Romeo",
      vehicle_make == 32 ~ "Audi",
      vehicle_make == 34 ~ "BMW",
      vehicle_make == 35 ~ "Datsun/Nissan",
      vehicle_make == 36 ~ "Fiat",
      vehicle_make == 37 ~ "Honda",
      vehicle_make == 38 ~ "Isuzu",
      vehicle_make == 39 ~ "Jaguar",
      vehicle_make == 41 ~ "Mazda",
      vehicle_make == 42 ~ "Mercedes-Benz",
      vehicle_make == 43 ~ "MG",
      vehicle_make == 45 ~ "Porsche",
      vehicle_make == 47 ~ "Saab",
      vehicle_make == 48 ~ "Subaru",
      vehicle_make == 49 ~ "Toyota",
      vehicle_make == 50 ~ "Triumph",
      vehicle_make == 51 ~ "Volvo",
      vehicle_make == 52 ~ "Mitsubishi",
      vehicle_make == 53 ~ "Suzuki",
      vehicle_make == 54 ~ "Acura",
      vehicle_make == 55 ~ "Hyundai",
      vehicle_make == 58 ~ "Infiniti",
      vehicle_make == 59 ~ "Lexus",
      vehicle_make == 62 ~ "Land Rover",
      vehicle_make == 63 ~ "Kia",
      vehicle_make == 64 ~ "Daewoo",
      vehicle_make == 65 ~ "Smart",
      vehicle_make == 67 ~ "Scion",
      vehicle_make == 69 ~ "Other Import",
      vehicle_make == 71 ~ "Ducati",
      vehicle_make == 72 ~ "Harley-Davidson",
      vehicle_make == 73 ~ "Kawasaki",
      vehicle_make == 74 ~ "Moto-Guzzi",
      vehicle_make == 76 ~ "Yamaha",
      vehicle_make == 77 ~ "Unknown Make",
      vehicle_make == 82 ~ "Freightliner/White",
      vehicle_make == 84 ~ "International Harvester/Navistar",
      vehicle_make == 85 ~ "Kenworth",
      vehicle_make == 86 ~ "Mack",
      vehicle_make == 87 ~ "Peterbilt",
      vehicle_make == 89 ~ "White/Autocar, White/GMC",
      vehicle_make == 90 ~ "Bluebird",
      vehicle_make == 92 ~ "Gillig",
      vehicle_make == 93 ~ "MCI",
      vehicle_make == 94 ~ "Thomas Built",
      vehicle_make == 97 ~ "Not Reported/Unknown",
      vehicle_make == 98 ~ "Other Make",
      vehicle_make == 99 ~ "Not Reported/Unknown"),
    initial_impact = case_when(
      initial_impact == 0 ~ "Non-Collision",
      initial_impact == 1 ~ "1 o'clock",
      initial_impact == 2 ~ "2 o'clock",
      initial_impact == 3 ~ "3 o'clock",
      initial_impact == 4 ~ "4 o'clock",
      initial_impact == 5 ~ "5 o'clock",
      initial_impact == 6 ~ "6 o'clock",
      initial_impact == 7 ~ "7 o'clock",
      initial_impact == 8 ~ "8 o'clock",
      initial_impact == 9 ~ "9 o'clock",
      initial_impact == 10 ~ "10 o'clock",
      initial_impact == 11 ~ "11 o'clock",
      initial_impact == 12 ~ "12 o'clock",
      initial_impact == 13 ~ "Top",
      initial_impact == 14 ~ "Undercarriage",
      initial_impact == 18 ~ "Cargo/Vehicle Parts Set-In-Motion",
      initial_impact == 19 ~ "Other Objects Set-In-Motion",
      initial_impact == 20 ~ "Object Set in Motion, Unknown",
      initial_impact == 61 ~ "Left",
      initial_impact == 62 ~ "Left-Front Side",
      initial_impact == 63 ~ "Left-Back Side",
      initial_impact == 81 ~ "Right",
      initial_impact == 82 ~ "Right-Front Side",
      initial_impact == 83 ~ "Right-Back Side",
      initial_impact == 98 ~ "Not Reported/Unknown",
      initial_impact == 99 ~ "Not Reported/Unknown"),
    dmg_extent = case_when(
      dmg_extent == 0 ~ "No Damage",
      dmg_extent == 2 ~ "Minor Damage",
      dmg_extent == 4 ~ "Functional Damage",
      dmg_extent == 6 ~ "Disabling Damage",
      dmg_extent == 8 ~ "Not Reported/Unknown",
      dmg_extent == 9 ~ "Not Reported/Unknown"),
    vehicle_max_sev = case_when(
      vehicle_max_sev == 0 ~ "No Apparent Injury",
      vehicle_max_sev == 1 ~ "Possible Injury",
      vehicle_max_sev == 2 ~ "Suspected Minor Injury",
      vehicle_max_sev == 3 ~ "Suspected Serious Injury",
      vehicle_max_sev == 4 ~ "Fatal",
      vehicle_max_sev == 5 ~ "Injured, Severity Unknown",
      vehicle_max_sev == 6 ~ "Died Prior to Crash",
      vehicle_max_sev == 8 ~ "No Person in Vehicle",
      vehicle_max_sev == 9 ~ "Not Reported/Unknown"))

# fct_collapse() with `speed`
vehicles_cleaned <- vehicles_cleaned %>% 
  mutate(
    speed = as.character(speed)) %>% 
  mutate(
    speed = fct_collapse(speed,
                         "0 mph" = c("0"),
                         "1-10 mph" = c("1", "2", "3", "4", "5",
                                        "6", "7", "8", "9", "10"),
                         "11-20 mph" = c("11", "12", "13", "14", "15",
                                         "16", "17", "18", "19", "20"),
                         "21-30 mph" = c("21", "22", "23", "24", "25",
                                         "26", "27", "28", "29", "30"),
                         "31-40 mph" = c("31", "32", "33", "34", "35",
                                         "36", "37", "38", "39", "40"),
                         "41-50 mph" = c("41", "42", "43", "44", "45",
                                         "46", "47", "48", "49", "50"),
                         "51-60 mph" = c("51", "52", "53", "54", "55",
                                         "56", "57", "58", "59", "60"),
                         "61-70 mph" = c("61", "62", "63", "64", "65",
                                         "66", "67", "68", "69", "70"),
                         "71-80 mph" = c("71", "72", "73", "74", "75",
                                         "76", "77", "78", "79", "80"),
                         "81-90 mph" = c("81", "82", "83", "84", "85",
                                         "86", "87", "88", "89", "90"),
                         "91-100 mph" = c("91", "92", "93", "94", "95",
                                          "96", "97", "98", "99", "100"),
                         "101-110 mph" = c("101", "102", "103", "104", "105",
                                           "106", "107", "108", "109", "110"),
                         "111-120 mph" = c("111", "112", "113", "114", "115",
                                           "116", "117", "118", "119", "120"),
                         "121-130 mph" = c("121", "122", "123", "124", "125",
                                           "126", "127", "128", "129", "130"),
                         "131-140 mph" = c("131", "132", "133", "134", "135",
                                           "136", "137", "138", "139", "140"),
                         "141-151 mph" = c("141", "142", "143", "144", "145",
                                           "146", "147", "148", "149", "150",
                                           "151"),
                         "Greater than 151 mph" = c("997"),
                         "Not Reported/Unknown" = c("998", "999")))
# --------------------
## Saving the `cleaned` data items to the `processed` directory

# `accidents_cleaned`
write_rds(accidents_cleaned, file = "data/processed/accidents_cleaned.rds")

# `distractions_cleaned`
write_rds(distractions_cleaned, file = "data/processed/distractions_cleaned.rds")

# `people_cleaned`
write_rds(people_cleaned, file = "data/processed/people_cleaned.rds")

# `vehicles_cleaned`
write_rds(vehicles_cleaned, file = "data/processed/vehicles_cleaned.rds")

# ----------------------------------------
