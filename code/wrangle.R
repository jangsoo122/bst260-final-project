#There are three data frames: one df contains the MARA score by category for the GIC membership for the HPHC Access America HP, the second df contains the demographics of the membership of the GIC, the third df contains the total paid medical claims.

#Libraries
library(readxl)
library(tidyverse)
library(zipcodeR)

#Wrangle total paid claims df
#Load data
df_paid_raw <- read_csv("data/hphc access america total paid.csv")

#Wrangle data to only have relevant columns, create municipality column, and convert Total Paid to numeric
df_paid_clean <- df_paid_raw %>%
  select(-`FY24 (Incurred Year Month)`, -`Plan Name`) %>%
  mutate(
    municipality_type = case_when(
      Municipality == "Unknown" ~ "state",
      TRUE ~ "municipal"
    ),
    `Total Paid` = as.numeric(gsub("[$,]", "", `Total Paid`))
  ) %>%
  select(-Municipality)

#Add all the total paid for each unit combination of HCG setting and HCG Line and delete Member ID
df_paid_clean <- df_paid_clean %>%
  group_by(`HCG Setting`, `HCG Line`, municipality_type) %>%
  summarise(paid = sum(`Total Paid`, na.rm = TRUE)) %>%
  ungroup()

# Calculate total for each municipality_type
total_by_type <- df_paid_clean %>%
  group_by(municipality_type) %>%
  summarise(paid = sum(paid)) %>%
  mutate(component = "total")

#Map HCG Setting and HCG Line to a MARA component. Note, the component dollars do not need to add up to the total dollars. For example, a claim dollar could be attributed to both inpatient and medical. 
df_paid_categories <- df_paid_clean %>%
  mutate(
    pharmacy = case_when(
      `HCG Setting` == "4. Prescription Drug" ~ paid,
      `HCG Line` %in% c("O16 - Pharmacy", "P34 - Office Administered Drugs") ~ paid,
      TRUE ~ 0
    ),
    medical = case_when(
      `HCG Setting` %in% c("1. Facility Inpatient", "2. Facility Outpatient", "3. Professional", "5. Ancillary") ~ paid,
      TRUE ~ 0
    ),
    inpatient = case_when(
      `HCG Setting` == "1. Facility Inpatient" ~ paid,
      `HCG Line` %in% c("P11 - Inpatient Surgery", "P13 - Inpatient Anesthesia", "P31 - Inpatient Visits") ~ paid,
      TRUE ~ 0
    ),
    outpatient = case_when(
      `HCG Setting` == "2. Facility Outpatient" ~ paid,
      `HCG Line` %in% c("P14 - Outpatient Surgery", "P16 - Outpatient Anesthesia") ~ paid,
      TRUE ~ 0
    ),
    physician = case_when(
      `HCG Setting` == "3. Professional" ~ paid,
      `HCG Line` %in% c("P32 - Office/Home Visits", "P15 - Office Surgery") ~ paid,
      TRUE ~ 0
    ),
    emergency_room = case_when(
      `HCG Line` %in% c("O11 - Emergency Room", "P51 - ER Visits and Observation Care") ~ paid,
      TRUE ~ 0
    ),
    other = case_when(
      `HCG Setting` == "5. Ancillary" ~ paid,
      `HCG Line` %in% c("P82 - Home Health Care", "P83 - Ambulance", "P84 - DME and Supplies", "P85 - Prosthetics") ~ paid,
      TRUE ~ 0
    )
  ) %>%
  select(-`HCG Setting`, -`HCG Line`, -paid) %>%
  pivot_longer(cols = c(pharmacy, medical, inpatient, outpatient, physician, emergency_room, other),
               names_to = "component",
               values_to = "paid") %>%
  group_by(municipality_type, component) %>%
  summarise(paid = sum(paid, na.rm = TRUE)) %>%
  ungroup()

# Combine with original data
df_paid_clean <- bind_rows(df_paid_categories, total_by_type)
df_paid_clean <- df_paid_clean %>%
  mutate(component = factor(component, levels = c("emergency_room", "inpatient", "medical", "other", "outpatient", "pharmacy", "physician", "total"))) %>%
  arrange(municipality_type, component)

# Save df_mara_clean to a CSV file
write_csv(df_paid_clean, "data/df_paid_clean.csv")



#Wrangle MARA score df
#Load data
df_mara_raw <- read_excel("data/HPHC Mara Risk Scores.xlsx")

#Wrangle data to only have Access America for FY24
df_mara_clean <- df_mara_raw %>% 
  filter(`_ENR_UDF_08_` == "HPHC ACCESS AMERICA") %>% 
  rename(plan = `_ENR_UDF_08_`) %>% 
  select(-1) %>% 
  filter(period == "2023/07 - 2024/06")
#Standardize component scores (ex. rx_score)
df_mara_clean <- df_mara_clean %>%
  mutate(across(
    tot_score:oth_score, 
    ~ . / pat_count,     
    .names = "{.col}"   
  ))
# Save df_mara_clean to a CSV file
write_csv(df_mara_clean, "data/df_mara_clean.csv")




#Wrangle demographic df
#Load data
df_demo_raw <- read_csv("data/access-america-demo-data-fy24.csv")

# Clean data and make tidy
df_demo_clean <- df_demo_raw %>%
  rename(
    total_medical_member_months = `Total Medical Member Months`,
    municipality = Municipality,
    zip_code = `Zip (5-digit)`,
    age = Age,
    gender = Gender
  ) %>%
  filter(
    zip_code != "Unknown",
    age != 255,
    gender != "U"
  ) %>%
  mutate(
    zip_code = as.character(zip_code),
    zip_code = str_pad(zip_code, width = 5, side = "left", pad = "0"),
    municipality = str_to_title(municipality),
    gender = as.factor(gender),
    age = as.numeric(age)
  ) %>%
  uncount(total_medical_member_months) 

# Create df with zipcodes and states
zip_state_lookup <- df_demo_clean %>%
  distinct(zip_code) %>%
  rowwise() %>%
  mutate(state = reverse_zipcode(zip_code)$state) %>%
  ungroup()

# Create state abb var and state/muni var
df_demo_clean <- df_demo_clean %>%
  left_join(zip_state_lookup, by = "zip_code") %>%
  mutate(
    municipality_type = case_when(
      municipality == "Unknown" ~ "state",
      TRUE ~ "municipal"
    )
  )

# Save df_demo_clean to a CSV file
write_csv(df_demo_clean, "data/df_demo_clean.csv")



#Re-wrangle df_paid_clean to calculate paid per member
# Extract pat_count for each municipality_type from df_mara_clean
pat_count_by_type <- df_mara_clean %>%
  select(Program, pat_count) %>%
  mutate(municipality_type = case_when(
    Program == "Muni" ~ "municipal",
    Program == "State" ~ "state",
    TRUE ~ NA_character_
  )) %>%
  select(municipality_type, pat_count)

# Join pat_count to df_paid_clean and calculate paid_per
df_paid_clean <- df_paid_clean %>%
  left_join(pat_count_by_type, by = "municipality_type") %>%
  mutate(paid_per = paid / pat_count) %>%
  select(-paid, -pat_count) %>%
  rename(paid = paid_per)

# Save the updated df_paid_clean
write_csv(df_paid_clean, "data/df_paid_clean.csv")