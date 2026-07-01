

rm(list = ls())

required_packages <- c("tidyverse", "MASS", "cluster", "ggrepel", "reshape2", "readxl")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}


username <- Sys.info()[["user"]]
desktop_path <- paste0("/Users/", username, "/Desktop")
target_book_folder <- "Pride and Prejudice"
target_excel_name <- "Pride and Prejudice, Summer 2026.xlsx"
excel_file_path <- file.path(desktop_path, target_book_folder, target_excel_name)

if (!file.exists(excel_file_path)) {
  stop(sprintf("\n\n[CRITICAL ERROR] Excel file not found at: %s\n", excel_file_path))
}


characters_47 <- c(
  "Young Lucas", "Wickham Sr.", "Wickham", "Sir William Lucas", "Sarah", 
  "Richard", "Nicholls", "Mrs. Younge", "Mrs. Reynolds", "Mrs. Philips", 
  "Mrs. Nicholls", "Mrs. Long", "Mrs. Jenkinson", "Mrs. Hurst", "Mrs. Gardiner", 
  "Mrs. Forster", "Mrs. Bennet", "Mrs. Annesley", "Mr. Robinson", "Mr. Philips", 
  "Mr. Morris", "Mr. Jones", "Mr. Hurst", "Mr. Gardiner", "Mr. Denny", 
  "Mr Collins", "Mr. Bennet", "Miss Watson", "Miss King", "Miss de Bourgh", 
  "Mary", "Maria Lucas", "Lydia", "Lady Lucas", "Lady Catherine de Bourgh", 
  "Kitty", "John", "Jane", "Hill", "Elizabeth", "Dawson", 
  "Darcy", "Mr. Darcy", "Colonel Forster", "Colonel Fitzwilliam", "Charlotte Lucas", 
  "Captain Carter", "Bingley"
)
characters_55 <- c(characters_47, paste0("Supporting_Cast_", 1:8))
heatmap_characters <- c(
  "Wickham Sr.", "Wickham", "Sir William Lucas", "Mrs. Reynolds", "Mrs. Philips",
  "Mrs. Hurst", "Mrs. Gardiner", "Mrs. Bennet", "Mrs. Annesley", "Mr. Jones",
  "Mr. Hurst", "Mr. Gardiner", "Mr. Denny", "Mr. Darcy", "Mr. Collins",
  "Mr. Bingley", "Mr. Bennet", "Miss de Bourgh", "Miss Darcy", "Miss Bingley",
  "Mary", "Maria Lucas", "Lydia", "Lady Catherine de Bourgh", "Kitty",
  "Jane", "Elizabeth", "Darcy's father", "Colonel Forster", "Colonel Fitzwilliam",
  "Charlotte Lucas"
)


generate_component_analysis <- function(component_code, component_full_name, excel_path, desktop_dir) {
  
  message(sprintf("Processing System: %s (%s)", component_full_name, component_code))
  
  target_folder_name <- sprintf("%s_Volatility", component_code)
  output_directory <- file.path(desktop_dir, target_folder_name)
  if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)
  
  df <- read_excel(excel_path, sheet = "ALL INSTANCES")
  
 
  target_col <- intersect(colnames(df), c(component_code, sprintf("Component %s", component_code), sprintf("Component_%s", component_code)))
  if(length(target_col) == 0) {
    target_col <- colnames(df)[grep(sprintf("^%s$|%s", component_code, component_code), colnames(df))][1]
  }
  
  message(sprintf("Matched dataset column: '%s' for visualization grouping.", target_col))
  

  timeline_base <- df %>%
    rename(TargetValue = !!sym(target_col)) %>%
    filter(!is.na(TargetValue)) %>%
    group_by(Character, `Graph Chapter`) %>% 
    summarise(
      Distance = max(TargetValue, na.rm = TRUE), 
      InteractionScore = sum(TargetValue, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Distance = ifelse(is.infinite(Distance) | is.nan(Distance), 0, Distance),
      InteractionScore = ifelse(is.na(InteractionScore), 0, InteractionScore)
    ) %>%
    rename(Chapter = `Graph Chapter`) %>%
    filter(Character %in% characters_55) %>%
    complete(Character = characters_55, Chapter = 1:61, fill = list(Distance = 0, InteractionScore = 0))
  
  timeline_base$Volume <- factor(case_when(
    timeline_base$Chapter <= 23 ~ "VOLUME I",
    timeline_base$Chapter <= 42 ~ "VOLUME II",
    TRUE ~ "VOLUME III"
  ), levels = c("VOLUME I", "VOLUME II", "VOLUME III"))


  fig_figure1 <- ggplot(timeline_base, aes(x = Chapter, y = Distance, color = Volume)) +
    geom_jitter(aes(size = InteractionScore), alpha = 0.6, width = 0.2) +
    scale_color_manual(values = c("#2c3e50", "#16a085", "#2980b9")) +
    scale_size_continuous(range = c(1, 6), name = "Interaction Score") +
    geom_text_repel(data = filter(timeline_base, Distance > quantile(Distance, 0.95)), 
                    aes(label = Character), size = 2.5, color = "black", max.overlaps = 10) +
    labs(
      title = sprintf("Figure 1: Macro-Systemic Outlier Scatter Matrix - Focus: %s", component_full_name),
      subtitle = sprintf("Overall Structural Deviations by Narrative Progression across Volumes (%s)", component_code),
      x = "Continuous Timeline (Chapters 1-61)", y = "Mahalanobis System Distance"
    ) +
    theme_minimal() + 
    theme(legend.position = "right", plot.title = element_text(face = "bold"))

  ggsave(file.path(output_directory, sprintf("%sfigure1.pdf", component_code)), plot = fig_figure1, width = 11, height = 5.5)


  fig_figure2 <- ggplot(timeline_base, aes(x = Chapter, y = Distance, size = InteractionScore, color = InteractionScore)) +
    geom_point(alpha = 0.75) +
    facet_grid(. ~ Volume, scales = "free_x", space = "free") +
    scale_color_gradient(low = "#1c2833", high = "#c0392b", name = "Total Scale Metric") +
    scale_size_continuous(range = c(1, 5.5), name = "Total Scale Metric") +
    geom_text_repel(data = filter(timeline_base, Distance > 45 & Character %in% c("Elizabeth", "Darcy", "Mr. Darcy", "Wickham", "Mrs. Bennet")), 
                    aes(label = Character), size = 3, fontface = "bold", color = "black", max.overlaps = 15) +
    labs(
      title = sprintf("Figure 2: Micro-Contextual Structural Outliers - Focus: %s", component_full_name),
      subtitle = sprintf("Inter-Chapter Mahalanobis Distance Metrics across Volume Segments (%s)", component_code),
      x = "Sequential Text Narrative Timeline (Graph Chapters 1-61)", y = "Local Mahalanobis Distance Squared (D2)"
    ) +
    theme_bw() + 
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold", size = 11),
          legend.position = "bottom", panel.grid.minor = element_blank())
  
  ggsave(file.path(output_directory, sprintf("%sfigure2.pdf", component_code)), plot = fig_figure2, width = 11, height = 5.5)

  heatmap_data <- timeline_base %>% filter(Character %in% heatmap_characters)
  
  fig_volatility <- ggplot(heatmap_data, aes(x = Chapter, y = factor(Character, levels = rev(heatmap_characters)), fill = Distance)) +
    geom_tile(color = "white", linewidth = 0.1) +
    facet_grid(. ~ Volume, scales = "free_x", space = "free") +
    scale_fill_gradient(low = "#ffffff", high = "#78281f", name = "D2 Variance") +
    scale_x_continuous(breaks = seq(0, 60, by = 20)) +
    labs(
      title = sprintf("Figure 3: Global Narrative Volatility Heatmap Matrix - Focus: %s", component_full_name),
      subtitle = sprintf("Continuous 61-Chapter Scope Tracking Structural Network Deviations (%s)", component_code),
      x = "Continuous Narrative Timeline (Graph Chapters 1 - 61)", y = "Network Character Profile"
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 7, color = "black"),
      axis.text.x = element_text(size = 8, color = "black"),
      strip.background = element_rect(fill = "#eaeded"),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid = element_blank(), legend.position = "right"
    )
  
  ggsave(file.path(output_directory, sprintf("%sfigure3.pdf", component_code)), plot = fig_volatility, width = 10.5, height = 7.5)
  

  fig_figure4 <- ggplot(timeline_base, aes(x = Volume, y = Distance, fill = Volume)) +
    geom_boxplot(outlier.color = "#c0392b", outlier.size = 2, width = 0.45, alpha = 0.85) +
    scale_fill_manual(values = c("#7f8c8d", "#34495e", "#bb8fce")) +
    labs(
      title = sprintf("Figure 4: Structural Outlier Distribution Profiling - Focus: %s", component_full_name),
      subtitle = sprintf("Network Variance and Mathematical Anomaly Spikes (%s)", component_code),
      x = "Jane Austen Novel Volume Segments", y = "Local Mahalanobis Distance (D2)"
    ) +
    theme_classic() + 
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11))
  
  ggsave(file.path(output_directory, sprintf("%sfigure4.pdf", component_code)), plot = fig_figure4, width = 7, height = 4.5)
  
  top_20_data <- timeline_base %>%
    group_by(Character) %>%
    summarise(CumulativeImpact = sum(Distance, na.rm = TRUE), .groups = 'drop') %>%
    filter(CumulativeImpact > 0) %>%
    top_n(20, wt = CumulativeImpact) %>%
    arrange(desc(CumulativeImpact))
  
  fig_top20 <- ggplot(top_20_data, aes(x = CumulativeImpact, y = reorder(Character, CumulativeImpact), fill = CumulativeImpact)) +
    geom_bar(stat = "identity", alpha = 0.9) +
    scale_fill_gradient(low = "#34495e", high = "#c0392b", name = "Cumulative Impact") +
    labs(
      title = sprintf("Figure 5: Isolated Top 20 Episodes of Destabilization - Focus: %s", component_full_name),
      subtitle = sprintf("Sorted by Cumulative Character System Impact Matrix (%s)", component_code),
      x = "Cumulative Destabilization Index Summary Score", y = "Character Identifier Profile"
    ) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 11), legend.position = "right")
  
  ggsave(file.path(output_directory, sprintf("%sfigure5.pdf", component_code)), plot = fig_top20, width = 8, height = 4.5)

 
  chaps <- unique(timeline_base$Chapter)
  chars <- sort(unique(timeline_base$Character))
  
  raw_matrix <- matrix(0, nrow = length(chaps), ncol = length(chars))
  rownames(raw_matrix) <- chaps
  colnames(raw_matrix) <- chars
  
  for(i in 1:nrow(timeline_base)) {
    row_idx <- as.character(timeline_base$Chapter[i])
    col_idx <- timeline_base$Character[i]
    raw_matrix[row_idx, col_idx] <- timeline_base$Distance[i]
  }
  
  final_csv_output <- data.frame(Chapter = as.numeric(rownames(raw_matrix)), raw_matrix)
  
 
  csv_dest_path <- file.path(desktop_dir, sprintf("mahalanobis_matrix_Component_%s.csv", component_code))
  write.csv(final_csv_output, file = csv_dest_path, row.names = FALSE)
  
  
  utils::winDialog(type = "ok", message = paste("File Written:", csv_dest_path))
  cat(sprintf("\n\n!!! CRITICAL VERIFICATION: FILE IS LOCATED AT: %s !!!\n\n", csv_dest_path))
}

generate_component_analysis(
  component_code = "DN", 
  component_full_name = "Description by Narrator", 
  excel_path = excel_file_path, 
  desktop_dir = desktop_path
)

message("Execution complete. 'DN_Volatility' directory successfully compiled. Figures prefixed with 'DN'.")
