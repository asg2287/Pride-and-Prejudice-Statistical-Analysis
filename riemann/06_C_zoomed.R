
library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)     
library(ggrepel)
library(patchwork)


home_dir   <- "/Users/ale"
base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
csv_path   <- file.path(base_dir, "New Mahalanobis", "AUSTEN_NETWORK_MAHALANOBIS.csv")


if (!file.exists(excel_path)) stop(paste("Excel file missing at verified location:", excel_path))
if (!file.exists(csv_path)) {
  cat("\n--- Path Diagnostic Catch ---\n")
  cat("Target folder contents to find correct spelling:\n")
  print(list.files(base_dir))
  stop(paste("CSV file missing at verified location:", csv_path))
}


raw_data         <- read_xlsx(excel_path, sheet = "ALL INSTANCES")
mahalanobis_data <- read_csv(csv_path)


raw_data <- raw_data %>%
  mutate(Absolute_Chapter = case_when(
    Volume == 1 ~ Chapter, 
    Volume == 2 ~ Chapter + 23, 
    Volume == 3 ~ Chapter + 42, 
    TRUE ~ Chapter
  ))


components  <- c("N", "DC", "C", "I", "DN")
matrix_5d   <- as.matrix(mahalanobis_data[, components])
cov_matrix  <- cov(matrix_5d)
mean_vector <- colMeans(matrix_5d)

raw_data <- raw_data %>%
  mutate(D2 = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix))


theme_manuscript <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "italic", hjust = 0.5),
      panel.border      = element_blank(),
      axis.line         = element_line(color = "black", linewidth = 0.5),
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", size = 10),
      axis.title        = element_text(face = "bold", size = 11),
      legend.position   = "bottom"
    )
}


global_integration <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  group_by(Character) %>%
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, C = 0, D2 = 0)) %>%
  summarise(
    `Action Volume`             = pracma::trapz(Absolute_Chapter, A),
    `Communication Volume`      = pracma::trapz(Absolute_Chapter, C),
    `Global Anomaly Score (D²)` = pracma::trapz(Absolute_Chapter, D2),
    .groups = 'drop'
  )


v_act_mean <- mean(global_integration$`Action Volume`, na.rm = TRUE)
v_com_mean <- mean(global_integration$`Communication Volume`, na.rm = TRUE)


plot_data_full <- global_integration %>%
  filter(!Character %in% c("Elizabeth", "Mr. Darcy"))

fig1_6_act_com_full <- ggplot(plot_data_full, aes(x = `Action Volume`, y = `Communication Volume`, label = Character)) +
  geom_vline(xintercept = v_act_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_hline(yintercept = v_com_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(
    family              = "serif", 
    size                = 3.5, 
    fontface            = "bold", 
    max.overlaps        = Inf,
    min.segment.length  = 0,
    box.padding         = 0.5, 
    point.padding       = 0.3, 
    force               = 2.0
  ) + 
  scale_color_gradient(low = "#1F618D", high = "#E67E22") + 
  labs(
    title = "Figure 1.6: Character Topology Map (Excluding Primary Protagonists)",
    subtitle = "Sub-Protagonist and Minor Character Narrative Communication Profile",
    x     = "Accumulated Action Volume (Plot Mechanics)",
    y     = "Accumulated Communication Volume (Communication Weight)"
  ) +
  theme_manuscript() + 
  theme(legend.position = "right")

output_full <- file.path(home_dir, "FIGURE_1_6_COMMUNICATION_TOPOLOGY_MAP_CLEAN.png")
ggsave(output_full, plot = fig1_6_act_com_full, width = 10, height = 7, units = "in", dpi = 300)


plot_data_zoomed <- global_integration %>%
  filter(
    !Character %in% c("Elizabeth", "Mr. Darcy"),
    `Action Volume` < 100,
    `Communication Volume` < 100
  )

fig1_6_act_com_zoomed <- ggplot(plot_data_zoomed, aes(x = `Action Volume`, y = `Communication Volume`, label = Character)) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(
    family              = "serif", 
    size                = 3.2, 
    fontface            = "bold", 
    max.overlaps        = Inf,
    min.segment.length  = 0,      
    arrow               = arrow(length = unit(0.015, "npc"), type = "closed"),
    box.padding         = 0.5, 
    point.padding       = 0.3,
    force               = 2.0
  ) + 
  scale_color_gradient(low = "#1F618D", high = "#E67E22") + 
  labs(
    title = "Figure 1.6: Character Topology Map (Zoomed Lower-Left Profile)", 
    subtitle = "Expanded Scale for Predictable Anchor and Minor Character Action-Communication Clusters",
    x     = "Accumulated Action Volume (Plot Mechanics)",
    y     = "Accumulated Communication Volume (Communication Weight)"
  ) +
  theme_manuscript() + 
  theme(legend.position = "right")

output_zoomed <- file.path(home_dir, "FIGURE_1_6_COMMUNICATION_TOPOLOGY_MAP_ZOOMED.png")
ggsave(output_zoomed, plot = fig1_6_act_com_zoomed, width = 10, height = 7, units = "in", dpi = 300)


message("---")
message(paste("Success! Full Scale Map (Action x Communication) saved to:", output_full))
message(paste("Success! Zoomed Quadrant Profile (Action x Communication) saved to:", output_zoomed))
message("---")
