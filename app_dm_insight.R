# Load necessary libraries
# If you don't have these, run: install.packages(c("tidyverse", "janitor"))
library(tidyverse)
library(janitor)

# Load the APP bite
# Replace "Your_File_Name.csv" with the actual name of your export
app_raw <- read_csv("Your_File_Name.csv") %>%
  slice(-c(1, 2)) %>% # This removes the two messy Qualtrics header rows
  clean_names()

# Quick check: See your Academic vs. Community breakdown
app_raw %>%
  count(q3_2_hospital_type)

# Load necessary libraries
# If you don't have these, run: install.packages(c("tidyverse", "janitor"))
install.packages(c("tidyverse", "janitor"))
library(tidyverse)
library(janitor)
library(tidyverse)
library(janitor)

# 1. Load the data 
# We tell R to treat everything as text ('c') to avoid date errors
app_raw <- read_csv("dm_insight_app.csv", col_types = cols(.default = "c")) %>%
  slice(-c(1, 2)) %>% # Removes those two messy Qualtrics header rows
  clean_names()

# 2. Recode your groups: Academic vs. Other
app_final <- app_raw %>%
  mutate(hospital_group = case_match(
    q3_2_hospital_type,
    "Academic" ~ "Academic",
    .default = "Other"
  ))

# 3. Check your work
app_final %>% count(hospital_group)
app_final <- app_raw %>%
  mutate(hospital_group = case_match(
    x1_q3_2_number_1_1,  # Use the name R found
    "Academic" ~ "Academic",
    .default = "Other"
  ))

# Check the results
app_final %>% count(hospital_group)

app_final %>% 
  select(contains("q5_1")) %>% 
  glimpse()


app_final %>% 
  select(q2_1, x1_q3_2_number_1_1, q5_2, q5_3) %>% 
  glimpse()


app_final_clean <- app_final %>%
  mutate(
    # Extract the first number found in the text (e.g., "20-24" becomes 20)
    # Then convert to numeric and treat NAs as 0
    app_headcount = q5_2 %>% 
      str_extract("\\d+") %>% 
      as.numeric() %>% 
      replace_na(0)
  )

# Now run the professional summary table
app_results <- app_final_clean %>%
  group_by(hospital_group) %>%
  summarise(
    n_hospitals = n(),
    avg_app_staffing = mean(app_headcount, na.rm = TRUE),
    median_staffing = median(app_headcount, na.rm = TRUE),
    max_staffing = max(app_headcount, na.rm = TRUE)
  )

print(app_results)
install.packages('gt')
# This creates a publication-quality table
library(gt)

app_results %>%
  gt() %>%
  tab_header(title = "Inpatient Diabetes APP Staffing",
             subtitle = "Comparison: Academic vs. Community/VA") %>%
  cols_label(
    hospital_group = "Hospital Type",
    avg_app_staffing = "Mean APPs",
    median_staffing = "Median APPs",
    max_staffing = "Max APPs"
  ) %>%
  fmt_number(columns = contains("avg"), decimals = 1)