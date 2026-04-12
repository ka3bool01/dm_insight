install.packages(c('tidyverse', 'readxl', 'gtsummary'))

# 1. Load the libraries
library(tidyverse)
library(readxl)
library(gtsummary)

# 1. Load and Clean
df_clean <- df %>%
  mutate(across(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults), 
                ~as.numeric(as.character(.)))) %>%
  # THIS IS THE MAPPING STEP:
  mutate(team_type = factor(team_type, 
                            levels = c(1, 2), 
                            labels = c("Endocrinology", "GMT"))) %>%
  filter(!is.na(team_type))

# 2. Generate the Table
suppressWarnings({
  table2 <- df_clean %>%
    select(team_type, attending_count, trainee_count, np_pa_count, 
           pharmd_count, cde_count, avg_census, avg_new_consults) %>%
    tbl_summary(
      by = team_type, 
      type = everything() ~ "continuous",
      statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      digits = all_continuous() ~ 1,
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
})

table2

# 1. Load and Clean
df_clean <- df %>%
  mutate(across(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults), 
                ~as.numeric(as.character(.)))) %>%
  # THIS IS THE MAPPING STEP:
  mutate(team_type = factor(team_type, 
                            levels = c(1, 2), 
                            labels = c("Endocrinology", "GMT"))) %>%
  filter(!is.na(team_type))

# 2. Generate the Table
suppressWarnings({
  table2 <- df_clean %>%
    select(team_type, attending_count, trainee_count, np_pa_count, 
           pharmd_count, cde_count, avg_census, avg_new_consults) %>%
    tbl_summary(
      by = team_type, 
      type = everything() ~ "continuous",
      statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      digits = all_continuous() ~ 1,
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
})

table2


# 1. Load and Clean with Mapping
df_clean <- df %>%
  mutate(across(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults), 
                ~as.numeric(as.character(.)))) %>%
  # We map the labels DIRECTLY onto team_type so the table can see them
  mutate(team_type = factor(team_type, 
                            levels = c(1, 2), 
                            labels = c("Endocrinology", "GMT"))) %>%
  # Remove rows where team_type is missing or not 1 or 2
  filter(!is.na(team_type))

# 2. The Final Table
suppressWarnings({
  table_final <- df_clean %>%
    select(team_type, attending_count, trainee_count, np_pa_count, 
           pharmd_count, cde_count, avg_census, avg_new_consults) %>%
    tbl_summary(
      by = team_type, # Now this column contains the words "Endocrinology" and "GMT"
      type = everything() ~ "continuous",
      statistic = all_continuous() ~ "{median} ({p25}, {p75})",
      digits = all_continuous() ~ 1,
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
})

table_final

install.packages("webshot2")
# --- EXPORT TO WORD (.docx) ---
table_final %>%
  as_gt() %>%
  gt::gtsave("Table_Endo_vs_GMT.docx")

# --- EXPORT TO IMAGE (.png) ---
table_final %>%
  as_gt() %>%
  gt::gtsave("Table_Endo_vs_GMT.png")