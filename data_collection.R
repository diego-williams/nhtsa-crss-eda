# ----------------------------------------
# Data Collection
# STAT 310-1
# Diego Williams
# ----------------------------------------

# ----------------------------------------
## Load Packages
library(tidyverse)
# ----------------------------------------

# ----------------------------------------
## Import the CRSS 'Data Files'

# These 'Data Files' are related to each other, and represent the
# different approaches one can take when organizing crash information

# The `ACCIDENTS` File
accidents <- read_csv("data/unprocessed/CRSS_18/ACCIDENT.csv")

# The `DISTRACT` File
distractions <- read_csv("data/unprocessed/CRSS_18/DISTRACT.csv")

# The `VEHICLE` File
vehicles <- read_csv("data/unprocessed/CRSS_18/VEHICLE.csv")

# The `PERSON` File
people <- read_csv("data/unprocessed/CRSS_18/PERSON.csv")
# ----------------------------------------