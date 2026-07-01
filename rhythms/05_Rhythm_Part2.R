library(tidyverse)
library(readxl)
library(survival)


home_dir   <- "/Users/ale"
base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
csv_path   <- file.path(base_dir, "Pride_And_Prejudice_Percentage_Clusters.csv")

if (!file.exists(excel_path)) stop("Longitudinal Excel spreadsheet missing.")
if (!file.exists(csv_path)) stop("Character cluster mapping CSV missing. Run the clustering script first.")

cluster_map <- read_csv(csv_path) %>%
  mutate(Character = str_trim(Character)) %>%
  select(Character, Cluster)

df_raw <- read_excel(excel_path, sheet = "ALL INSTANCES")

all_characters <- df_raw %>% 
  mutate(Character = str_trim(Character)) %>%
  filter(Character != "Elizabeth", !is.na(Character)) %>%
  pull(Character) %>% 
  unique()

timeline_all <- df_raw %>%
  mutate(
    Character = str_trim(Character),
    Character = case_when(
      Character == "Lady Catherine" ~ "Lady Catherine de Bourgh",
      Character == "Darcy"          ~ "Mr. Darcy",
      Character == "Bingley"        ~ "Mr. Bingley",
      Character == "Mrs. Bennett"    ~ "Mrs. Bennet",
      TRUE ~ Character
    )
  ) %>%
  group_by(Character, `Graph Chapter`) %>% 
  summarise(Presence = as.numeric(max(`Total Score`, na.rm = TRUE) > 0), .groups = 'drop') %>%
  rename(Name = Character, Chapter = `Graph Chapter`) %>%
  filter(Name %in% all_characters) %>%
  complete(Name = all_characters, Chapter = 1:61, fill = list(Presence = 0)) %>%
  left_join(cluster_map, by = c("Name" = "Character")) %>%
  filter(!is.na(Cluster))

markov_results <- timeline_all %>%
  arrange(Name, Chapter) %>%
  group_by(Name, Cluster) %>%
  mutate(
    Next_State = lead(Presence),
    Transition = case_when(
      Presence == 0 & Next_State == 0 ~ "P_00",
      Presence == 0 & Next_State == 1 ~ "P_01",
      Presence == 1 & Next_State == 0 ~ "P_10",
      Presence == 1 & Next_State == 1 ~ "P_11",
      TRUE ~ as.character(NA)
    )
  ) %>%
  filter(!is.na(Transition)) %>%
  count(Transition) %>%
  mutate(Probability = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Transition, values_from = Probability, values_fill = list(Probability = 0))


for(col in c("P_00", "P_01", "P_10", "P_11")) {
  if(!col %in% names(markov_results)) markov_results[[col]] <- 0
}


absence_spells <- timeline_all %>%
  arrange(Name, Chapter) %>%
  group_by(Name, Cluster) %>%
  mutate(spell_id = cumsum(Presence != lag(Presence, default = first(Presence))) + 1) %>%
  group_by(Name, Cluster, spell_id, Presence) %>%
  summarise(
    Absence_Duration = n(),
    Max_Chapter = max(Chapter),
    .groups = "drop"
  ) %>%
  filter(Presence == 0) %>%
  mutate(
    Reintroduced = ifelse(Max_Chapter == 61, 0, 1)
  )

surv_fit_clusters <- survfit(Surv(Absence_Duration, Reintroduced) ~ Cluster, data = absence_spells)

survival_summary <- absence_spells %>%
  group_by(Name) %>%
  summarise(
    Total_Absence_Spells = n(),
    Median_Absence_Length = median(Absence_Duration),
    Reintroduction_Rate  = sum(Reintroduced) / n(),
    .groups = "drop"
  )


comprehensive_rhythm_metrics <- markov_results %>%
  left_join(survival_summary, by = "Name") %>%
  arrange(Cluster, desc(P_01)) 

print("=== COHORT TRANSITION & REINTRODUCTION METRICS BY CLUSTER ===")
print(head(comprehensive_rhythm_metrics, 15))

if(!dir.exists("figures")) dir.create("figures")
write_csv(comprehensive_rhythm_metrics, "figures/ALL_CHARACTERS_RHYTHM_ANALYSIS.csv")
write_csv(comprehensive_rhythm_metrics, "/Users/ale/ALL_CHARACTERS_RHYTHM_ANALYSIS.csv")


png("/Users/ale/ALL_CHARACTERS_SURVIVAL_REINTRODUCTION.png", width = 2100, height = 1500, res = 300)

cluster_colors <- Skinner_colors <- c("#1B4F72", "#78281F", "#5D6D7E")

plot(surv_fit_clusters, conf.int = FALSE, col = cluster_colors, lwd = 3,
     xlab = "Chapters Spent in Absence (Time-at-Risk)",
     ylab = "Probability of Remaining Absent",
     main = "Narrative Reintroduction Survival Function by Cluster (K2, K3, K4)",
     font.main = 2, col.main = "#2C3E50", family = "serif")

grid(nx = NULL, ny = NULL, lty = "dotted", col = "gray70")

legend("topright", 
       legend = c("K4 (Top Curve)", 
                  "K3 (Middle Curve)", 
                  "K2 (Bottom Curve)"), 
       col = rev(cluster_colors), 
       lwd = 3, 
       bty = "n",
       cex = 0.85,
       text.font = 2)

dev.off()

message("Success! Data charts, legends, and tables have been cleanly exported to /Users/ale/")
