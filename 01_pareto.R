
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(readxl)
library(openxlsx)
library(dplyr)

excel_path <- "/Users/ale/Desktop/Pride_and_Prejudice_Final_6Tier_Analysis_v2.xlsx"


sheets <- excel_sheets(excel_path)
selected_sheet <- if("ALL INSTANCES" %in% sheets) "ALL INSTANCES" else sheets[1]
df <- read_excel(excel_path, sheet = selected_sheet)


colnames(df) <- trimws(colnames(df))

if (!"Total Scores" %in% colnames(df)) {
  stop("Error: Could not find a column named 'Total Scores' in your file.")
}


df_analyzed <- df %>%

  mutate(`Total Scores` = as.numeric(`Total Scores`)) %>%

  arrange(desc(`Total Scores`)) %>%
  mutate(Rank_Index = row_number()) %>%
  mutate(Pareto = ifelse(Rank_Index <= 7, "P1", "P2")) %>%
  dplyr::select(-Rank_Index)


write.xlsx(df_analyzed, excel_path, overwrite = TRUE)

cat("\n======================================================\n")
cat("Success! The real Pareto tiers have been calculated.\n")
cat("The top 7 ensemble characters are marked P1, and the tail is marked P2.\n")
cat("======================================================\n")
