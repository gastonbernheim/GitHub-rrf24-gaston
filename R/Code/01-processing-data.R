# Reproducible Research Fundamentals 
# 01. Data processing

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

### Loading data ----

# Load the dataset

# Exercise 1: Explore the data ---
data_path <- "C:/Users/wb639770/OneDrive - WBG/RRF 2024/DataWork/"
data      <- read.csv(file.path(data_path, "Data/Raw/TZA_CCT_baseline.csv"))
#data      <- read_dta(file.path(data_path, "Data/Raw/TZA_CCT_baseline.dta"))

glimpse(data)

# Exercise 2: Remove duplicates based on hhid

# Identify and display duplicate hhid values
duplicates <- data %>%
    group_by(hhid) %>%
    filter(n() > 1)

# Exercise 3: Clean data

data_clean <- data %>%
    distinct(hhid, .keep_all = TRUE)

### Household (HH) level data ----

### Data cleaning for Household-member (HH-member) level
data_clean_hh <- data_clean %>%
    mutate(crop_other = str_to_title(crop_other)) %>%
    # Replace values in the crop variable based on crop_other using regex for new cropS
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut") ~ 40,
        str_detect(crop_other, "Sesame") ~ 41,
        TRUE ~ crop
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(where(is.numeric), ~ replace(., . == -88, NA)))

# Identify outliers
#area
boxplot.stats(data_clean_hh$ar_farm)

#consumption
boxplot.stats(data_clean_hh$food_cons)    

#### Tidying data for HH level
#data_tidy_hh <- data_dedup %>%
#   ......

    # Convert submissionday to date
    #mutate(...... = as.Date(......, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    #mutate(......) %>%
    # Convert ar_farm_unit to factor (categorical data)
    #mutate(......) %>%
    # Replace values in the crop variable based on crop_other using regex for new crops
    #mutate(crop = case_when(
    #    ......
    #)) %>%
    # Recode negative numeric values (-88) as missing (NA)
    #mutate(across(......) ) %>%
    # Add variable labels
    #set_variable_labels(
    #    ......
    #)

# Save the household data
write.csv(data_clean_hh, file.path(data_path, "Data/Intermediate/TZA_CCT_HH.csv"), row.names = FALSE)

### Household member (HH-member) level data ----

#### Tidying data for HH-member level
data_tidy_mem <- data_dedup %>%
    select(......,
           starts_with(......)) %>%
    pivot_longer(cols = -c(vid, hhid, enid),  # Keep IDs static
                 names_to = ......,
                 names_pattern = "(.*)_(\\d+)")  # Capture the variable and the suffix

### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    ...... %>%
    # Variable labels
    ......

# Save the tidy household-member data
write_dta(data_clean_mem, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = ......,
                values_from = ......,
                names_prefix = ......)

# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))
