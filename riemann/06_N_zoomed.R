
rm(list = ls())
library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)
library(ggrepel)

home_dir   <- "/Users/ale"
base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
csv_path   <- file.path(base_dir, "New Mahalanobis", "AUSTEN_NETWORK_MAHALANOBIS.csv")

if (!file.exists(excel_path)) stop("Excel file missing.")
if (!file.exists(csv_path))   stop("CSV file missing.")


raw_data         <- read_xlsx(excel_path)
mahalanobis_data <- read_csv(csv_path)


raw_data <- raw_data %>%
  group_by(Character) %>%
  mutate(Absolute_Chapter = case_when(
    Volume == 1 ~ Chapter, 
    Volume == 2 ~ Chapter + 23, 
    Volume == 3 ~ Chapter + 42, 
    TRUE ~ Chapter
  )) %>%
  ungroup()


components  <- c("N", "DC", "C", "I", "DN")
matrix_5d   <- as.matrix(mahalanobis_data[, components])
cov_matrix  <- cov(matrix_5d)
mean_vector <- colMeans(matrix_5d)


if("ALL INSTANCES TOTAL" %in% colnames(raw_data)) {
  raw_data <- raw_data %>% rename(Total_Score = `ALL INSTANCES TOTAL`)
} else {
  raw_data$Total_Score <- rowSums(raw_data[, intersect(colnames(raw_data), c(components, "A"))], na.rm = TRUE)
}

raw_data <- raw_data %>%
  mutate(
    D2 = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix)
  )


global_integration <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  group_by(Character) %>%
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, N = 0, DC = 0, D2 = 0, Total_Score = 0)) %>%
  summarise(
    `Action Volume`                     = pracma::trapz(Absolute_Chapter, A),
    `Name Mentions Volume`              = pracma::trapz(Absolute_Chapter, N),
    `Discussion of Character Volume`    = pracma::trapz(Absolute_Chapter, DC),
    `Global Anomaly Score (D²)`         = pracma::trapz(Absolute_Chapter, D2),
    `Total Score Volume`                = pracma::trapz(Absolute_Chapter, Total_Score),
    .groups = 'drop'
  )


theme_manuscript <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "italic", hjust = 0.5),
      panel.border      = element_blank(),
      axis.line         = element_line(color = "black", linewidth = 0.5),
      axis.title        = element_text(face = "bold", size = 11),
      legend.position   = "right"
    )
}


v_act_mean  <- mean(global_integration$`Action Volume`, na.rm = TRUE)
v_n_mean    <- mean(global_integration$`Name Mentions Volume`, na.rm = TRUE)
v_ts_mean   <- mean(global_integration$`Total Score Volume`, na.rm = TRUE)
v_d2_mean   = mean(global_integration$`Global Anomaly Score (D²)`, na.rm = TRUE)

plot_data_full <- global_integration %>% filter(!Character %in% c("Elizabeth", "Mr. Darcy"))


fig1_6_n_full <- ggplot(plot_data_full, aes(x = `Action Volume`, y = `Name Mentions Volume`, label = Character)) +
  geom_vline(xintercept = v_act_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_hline(yintercept = v_n_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(family = "serif", size = 3.5, fontface = "bold", max.overlaps = Inf, min.segment.length = 0, box.padding = 0.5, point.padding = 0.3, force = 2.0) + 
  scale_color_gradient(low = "#1F618D", high = "#E67E22") + 
  labs(title = "Figure 1.6: Character Topology Map (Excluding Primary Protagonists)", subtitle = "Sub-Protagonist and Minor Character Explicit Name Mention Profile", x = "Accumulated Action Volume", y = "Accumulated Name Mentions Volume") +
  theme_manuscript()

ggsave(file.path(home_dir, "FIGURE_1_6_N_TOPOLOGY_MAP_CLEAN.png"), plot = fig1_6_n_full, width = 10, height = 7, dpi = 300)


max_zoom_n_x <- quantile(plot_data_full$`Action Volume`, 0.75, na.rm = TRUE)
max_zoom_n_y <- quantile(plot_data_full$`Name Mentions Volume`, 0.75, na.rm = TRUE)
plot_data_n_zoomed <- plot_data_full %>% filter(`Action Volume` <= max_zoom_n_x, `Name Mentions Volume` <= max_zoom_n_y)

fig1_6_n_zoomed <- ggplot(plot_data_n_zoomed, aes(x = `Action Volume`, y = `Name Mentions Volume`, label = Character)) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(family = "serif", size = 3.2, fontface = "bold", max.overlaps = Inf, min.segment.length = 0, arrow = arrow(length = unit(0.015, "npc"), type = "closed"), box.padding = 0.6, point.padding = 0.3, force = 2.5) + 
  scale_color_gradient(low = "#1F618D", high = "#E67E22") + 
  coord_cartesian(xlim = c(0, max_zoom_n_x), ylim = c(0, max_zoom_n_y)) +
  labs(title = "Figure 1.6: Character Topology Map (Optimized Cluster Zoom Profile)", subtitle = "Statistically Bounded Viewport Minimizing Label Density Overlap", x = "Accumulated Action Volume", y = "Accumulated Name Mentions Volume") +
  theme_manuscript()

ggsave(file.path(home_dir, "FIGURE_1_6_N_TOPOLOGY_MAP_ZOOMED.png"), plot = fig1_6_n_zoomed, width = 10, height = 7, dpi = 300)

message("--- Name Mentions (N) Topology Maps Generated Successfully ---")
