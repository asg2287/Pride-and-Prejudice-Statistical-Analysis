library(readxl)
library(ggplot2)
library(reshape2)


file_path <- "/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx"
sheet1_all_instances <- read_excel(file_path, sheet = "ALL INSTANCES")
sheet3_percentages  <- read_excel(file_path, sheet = "CHARACTER PERCENTAGES")


sheet3_percentages <- subset(sheet3_percentages, !is.na(Character) & Character != "Character" & Character != "NA")
sheet1_all_instances <- subset(sheet1_all_instances, !is.na(Character) & Character != "Character" & Character != "NA")

X_raw <- as.data.frame(sheet3_percentages)
rownames(X_raw) <- X_raw$Character
pct_cols <- c("N %", "DC %", "C %", "I %", "DN %", "A %")
char_weights <- as.matrix(X_raw[, pct_cols]) / 100 


all_characters <- unique(X_raw$Character)
total_chapters <- 61

presence_tensor <- array(0, dim = c(length(all_characters), total_chapters, 6),
                         dimnames = list(all_characters, 1:total_chapters, c("N", "DC", "C", "I", "DN", "A")))

for(i in 1:nrow(sheet1_all_instances)) {
  char <- sheet1_all_instances$Character[i]
  vol  <- as.numeric(sheet1_all_instances$Volume[i])
  local_ch <- as.numeric(sheet1_all_instances$Chapter[i])
  
  absolute_ch <- local_ch
  if (!is.na(vol)) {
    if (vol == 2) {
      absolute_ch <- local_ch + 23
    } else if (vol == 3) {
      absolute_ch <- local_ch + 42
    }
  }
  
  if (char %in% all_characters && absolute_ch <= total_chapters) {
    presence_tensor[char, absolute_ch, ] <- as.numeric(sheet1_all_instances[i, c("N", "DC", "C", "I", "DN", "A")])
  }
}


riemann_matrix <- matrix(0, nrow = length(all_characters), ncol = total_chapters,
                         dimnames = list(all_characters, paste0("Ch_", 1:total_chapters)))

delta_x <- 1
for (char in all_characters) {
  cumulative_auc <- 0
  w <- char_weights[char, ]
  for (ch in 1:total_chapters) {
    local_traits <- presence_tensor[char, ch, ]
    local_intensity <- sum(local_traits * w)
    cumulative_auc <- cumulative_auc + (local_intensity * delta_x)
    riemann_matrix[char, ch] <- cumulative_auc
  }
}


plot_data <- melt(riemann_matrix)
colnames(plot_data) <- c("Character", "Chapter", "Cumulative_AUC")
plot_data$Chapter <- as.numeric(gsub("Ch_", "", plot_data$Chapter))


color_count <- length(all_characters)
get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

full_trajectory_plot <- ggplot(plot_data, aes(x = Chapter, y = Cumulative_AUC, group = Character, color = Character)) +
  geom_line(size = 0.8, alpha = 0.7, show.legend = FALSE) + 
  scale_color_manual(values = get_palette(color_count)) +
  scale_x_continuous(breaks = seq(1, 61, by = 5)) +
  labs(
    title = "Complete Longitudinal Character Structural Trajectories",
    subtitle = "Cumulative Presence Mass via Riemann Integration (All 51 Characters, Volume-Corrected)",
    x = "Timeline (Continuous Chapter 1-61)",
    y = "Cumulative Structural Mass (AUC)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(face = "italic", color = "gray30", size = 10, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("/Users/ale/Desktop/RIEMANN_ALL_CHARACTER_TRAJECTORIES.pdf", plot = full_trajectory_plot, width = 11, height = 7, device = "pdf")

