
library(readxl)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(stringr)


file_path <- "/Users/ale/Desktop/Pride_and_Prejudice_Final_6Tier_Analysis_v2.xlsx"
output_file <- "figure2_character_landscape.png"


if (!file.exists(file_path)) {
  stop(paste("CRITICAL ERROR: Matrix file not found at path:", file_path))
}

df <- read_excel(file_path)


df_clean <- df %>%
  mutate(
   
    Components_Checked = as.numeric(str_replace_all(`Component Checklist`, "C", "")),
    
  
    K_Means_Cluster = factor(`K-Means`, levels = c("K1", "K2", "K3", "K4")),
    

    Total_Score = as.numeric(`Total Scores`),
    Character_Label = Character
  ) %>%
  
  filter(!is.na(Total_Score) & Total_Score > 0 & !is.na(Components_Checked))


p <- ggplot(df_clean, aes(x = Components_Checked, y = Total_Score, color = K_Means_Cluster)) +
  
  geom_point(size = 5, alpha = 0.85) +
  
 
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000),
    labels = c("10^0", "10^1", "10^2", "10^3", "10^4")
  ) +
  

  scale_x_continuous(breaks = 1:6) +
  

  scale_color_manual(
    values = c(
      "K1" = "#E66101",
      "K2" = "#B2ABD2",
      "K3" = "#5E3C99",
      "K4" = "#1B9E77"
    ),
    name = "K-Means Cluster"
  ) +
  

  geom_text_repel(
    aes(label = Character_Label),
    size = 3.5,
    fontface = "plain",
    max.overlaps = 45,          
    box.padding = 0.35,
    point.padding = 0.3,
    segment.color = "grey80",
    segment.size = 0.2,
    show.legend = FALSE          
  ) +
  

  labs(
    title = "Number of Components Checked vs Total Score",
    x = "Components Checked (C1 down to C6 spectrum)",
    y = "Total Score (Log Layout)"
  ) +
  

  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )


ggsave(
  filename = output_file,
  plot = p,
  width = 14,
  height = 8,
  dpi = 300,
  device = "png"
)

message("SUCCESS: File 'figure2_character_landscape.png' successfully compiled and saved.")
