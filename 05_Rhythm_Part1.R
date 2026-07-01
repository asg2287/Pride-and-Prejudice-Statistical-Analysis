

library(tidyverse)
library(readxl)


home_dir   <- "/Users/ale"
base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")

if (!file.exists(excel_path)) stop("Longitudinal Excel spreadsheet missing.")

tier_6_characters <- c(
  "Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", "Mr. Bingley", 
  "Wickham", "Miss Bingley", "Mr. Collins", "Lydia", "Mr. Gardiner", 
  "Charlotte Lucas", "Mrs. Gardiner", "Kitty", "Mr. Bennet", 
  "Lady Catherine de Bourgh", "Sir William Lucas", "Colonel Fitzwilliam", 
  "Mary", "Mrs. Philips", "Mrs. Reynolds", "Mrs. Hurst", "Maria Lucas", 
  "Lady Lucas", "Mr. Hurst", "Mrs. Jenkinson"
)


df_raw <- read_excel(excel_path, sheet = "ALL INSTANCES")

timeline_all <- df_raw %>%
  mutate(
    Character = str_trim(Character),
    Character = case_when(
      Character == "Lady Catherine" ~ "Lady Catherine de Bourgh",
      Character == "Darcy"          ~ "Mr. Darcy",
      Character == "Bingley"        ~ "Mr. Bingley",
      TRUE ~ Character
    )
  ) %>%
  group_by(Character, `Graph Chapter`) %>% 
  summarise(Presence = as.numeric(max(`Total Score`, na.rm = TRUE) > 0), .groups = 'drop') %>%
  rename(Name = Character, Chapter = `Graph Chapter`) %>%
  filter(Name %in% tier_6_characters) %>%
  complete(Name = tier_6_characters, Chapter = 1:61, fill = list(Presence = 0))


universal_recurrence <- timeline_all %>%
  filter(Presence == 1) %>%
  arrange(Name, Chapter) %>%
  group_by(Name) %>%
  mutate(Interval = Chapter - lag(Chapter)) %>%
  filter(!is.na(Interval)) %>%
  summarise(
    Total_Active_Chapters = sum(Presence),
    Mean_Absence_Interval = mean(Interval),
    SD_Absence_Interval   = sd(Interval),

    Coeff_of_Variation    = SD_Absence_Interval / Mean_Absence_Interval,
    Distribution_Type     = ifelse(Coeff_of_Variation < 1.0, "Predictable (Periodic)", "Unpredictable (Bursty)"),
    .groups = 'drop'
  )


summary_table <- universal_recurrence %>%
  count(Distribution_Type) %>%
  mutate(Percentage = (n / sum(n)) * 100)

print("=== WHOLE-NETWORK SYSTEMIC PATTERN SUMMARY ===")
print(summary_table)

print("=== COMPREHENSIVE CHARACTER RECURRENCE METRICS ===")
print(universal_recurrence %>% arrange(Coeff_of_Variation))


if(!dir.exists("figures")) dir.create("figures")
write_csv(universal_recurrence, "figures/UNIVERSAL_RECURRENCE_METRICS.csv")
write_csv(universal_recurrence, "/Users/ale/UNIVERSAL_RECURRENCE_METRICS.csv")
