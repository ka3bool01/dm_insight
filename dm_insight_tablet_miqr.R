install.packages(c('tidyverse', 'readxl', 'gtsummary'))

# 1. Load the libraries
library(tidyverse)
library(readxl)
library(gtsummary)

# 2. The Critical Cleaning Step (Paste this exactly)
df_clean <- read_excel("hospital_data_clean.xlsx") %>%
  mutate(across(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults), 
                ~as.numeric(as.character(.)))) %>%
  filter(!is.na(academic_v_non))

# 3. The Table Code
table1 <- df_clean %>%
  select(academic_v_non, attending_count, trainee_count, np_pa_count, 
         pharmd_count, cde_count, avg_census, avg_new_consults) %>%
  tbl_summary(
    by = academic_v_non,
    # 1. Force everything to stay on one row
    type = everything() ~ "continuous", 
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    digits = all_continuous() ~ 1,
    # 2. Rename the variables for the manuscript
    label = list(
      attending_count ~ "Attendings",
      trainee_count ~ "Trainees",
      np_pa_count ~ "NPs/PAs",
      pharmd_count ~ "PharmDs",
      cde_count ~ "CDCES",
      avg_census ~ "Average Daily Census",
      avg_new_consults ~ "Average New Consults"
    ),
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "wilcox.test") %>%
  bold_labels()

# View your masterpiece
table1