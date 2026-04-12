# --- PART 1: INSTALL TOOLS (Only run once) ---
# 
install.packages(c("readxl", "dplyr", "gtsummary", "janitor", "gt"))

# --- PART 2: LOAD TOOLS ---
library(readxl)
library(dplyr)
library(gtsummary)
library(janitor)

# --- PART 3: DATA CLEANING ---
# Note: Ensure "hospital_data_clean.xlsx" is in the same folder as this script
df <- read_excel("hospital_data_clean.xlsx") %>% 
  clean_names() %>%
  mutate(across(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults), 
                ~as.numeric(as.character(.x)))) %>%
  filter(!is.na(academic_v_non), !is.na(team_type))

# Define clean labels for the rows
my_labels <- list(
  attending_count ~ "No. of Attendings",
  trainee_count ~ "No. of Trainees",
  np_pa_count ~ "No. of NPs/PAs",
  pharmd_count ~ "No. of PharmDs",
  cde_count ~ "No. of CDEs",
  avg_census ~ "Avg. Daily Census",
  avg_new_consults ~ "Avg. New Consults"
)

# --- PART 4: GENERATE TABLES ---

# Table 1: Academic vs Other
table1 <- df %>%
  select(academic_v_non, attending_count, trainee_count, np_pa_count, 
         pharmd_count, cde_count, avg_census, avg_new_consults) %>%
  tbl_summary(
    by = academic_v_non,
    label = my_labels,
    type = list(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults) ~ "continuous"),
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall(last = FALSE, col_label = "**Overall**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>% 
  modify_header(label = "** **") %>% 
  bold_labels()

# Table 2: Regular Endo vs GMT
table2 <- df %>%
  filter(team_type %in% c(1, 2)) %>%
  mutate(Team_Label = ifelse(team_type == 1, "Regular Endo", "GMT")) %>%
  select(Team_Label, attending_count, trainee_count, np_pa_count, 
         pharmd_count, cde_count, avg_census, avg_new_consults) %>%
  tbl_summary(
    by = Team_Label,
    label = my_labels,
    type = list(c(attending_count, trainee_count, np_pa_count, 
                  pharmd_count, cde_count, avg_census, avg_new_consults) ~ "continuous"),
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = all_continuous() ~ 1,
    missing = "no"
  ) %>%
  add_overall(last = FALSE, col_label = "**Overall**") %>% 
  add_p(test = all_continuous() ~ "t.test") %>% 
  modify_header(label = "** **") %>% 
  bold_labels()

# --- PART 5: OUTPUT ---
table1
table2

# --- OPTIONAL: SAVE TO WORD ---
# table1 %>% as_gt() %>% gt::gtsave("Table_Academic.docx")
# table2 %>% as_gt() %>% gt::gtsave("Table_TeamType.docx")