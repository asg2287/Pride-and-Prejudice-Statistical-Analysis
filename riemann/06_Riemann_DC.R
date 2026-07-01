library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)     
library(ggrepel)
library(patchwork)

base_dir   <- path.expand("~/Desktop/Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
csv_path   <- file.path(base_dir, "New Mahalanobis", "AUSTEN_NETWORK_MAHALANOBIS.csv")

raw_data         <- read_xlsx(excel_path)
mahalanobis_data <- read_csv(csv_path)

raw_data <- raw_data %>%
  group_by(Character) %>%
  mutate(Absolute_Chapter = case_when(Volume == 1 ~ Chapter, Volume == 2 ~ Chapter + 23, Volume == 3 ~ Chapter + 42, TRUE ~ Chapter)) %>%
  ungroup()

components <- c("N", "DC", "C", "I", "DN", "A")
matrix_6d   <- as.matrix(mahalanobis_data[, components])
cov_matrix  <- cov(matrix_6d)
mean_vector <- colMeans(matrix_6d)

raw_data <- raw_data %>%
  mutate(
    Euclidean_Norm = sqrt(N^2 + DC^2 + C^2 + I^2 + DN^2 + A^2),
    D2             = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix)
  )

theme_manuscript <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "italic", hjust = 0.5),
      panel.border      = element_blank(),
      axis.line         = element_line(color = "black", size = 0.5),
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", size = 10),
      axis.title        = element_text(face = "bold", size = 11),
      legend.position   = "bottom"
    )
}

global_integration <- raw_data %>%
  mutate(Character = str_trim(Character)) %>%
  group_by(Character) %>%
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, C = 0, DN = 0, DC = 0, I = 0, N = 0, D2 = 0)) %>%
  summarise(
    `Action`                            = pracma::trapz(Absolute_Chapter, A),
    `Communication`                     = pracma::trapz(Absolute_Chapter, C),
    `Description by Narrator`           = pracma::trapz(Absolute_Chapter, DN),
    `Discussion of Character`           = pracma::trapz(Absolute_Chapter, DC),
    `Global Anomaly Score (D²)`         = pracma::trapz(Absolute_Chapter, D2),
    `Interiority`                       = pracma::trapz(Absolute_Chapter, I),
    `Name Mentions`                     = pracma::trapz(Absolute_Chapter, N),
    .groups = 'drop'
  )


riemann_bars_DC <- global_integration %>% filter(Character %in% c("Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", "Mr. Bingley", "Mr. Wickham", "Lydia", "Mr. Collins"))


top_12_characters_DC <- raw_data %>% group_by(Character) %>% summarise(Total = sum(DC)) %>% slice_max(Total, n = 12) %>% pull(Character)
fig1_1 <- ggplot(filter(raw_data, Character %in% top_12_characters_DC), aes(x = Absolute_Chapter, y = DC)) +
  geom_area(fill = "#F3D1D1", alpha = 0.7, color = "#A35C5C", size = 0.3) +
  facet_wrap(~ factor(Character, levels = top_12_characters_DC), ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 10, 21, 31, 41, 51, 61)) +
  labs(title = "Figure 1.1: DC Vector Trajectories", subtitle = "Continuous Riemann Distribution of Discussion of Character", x = "Chapters 1-61", y = "Discussion Intensity (DC)") +
  theme_manuscript()
ggsave("FIGURE_1_1_DC_VECTOR_TRAJECTORIES.png", plot = fig1_1, width = 9.5, height = 11, units = "in", dpi = 300)


fig1_2_bars_data <- riemann_bars_DC %>% pivot_longer(cols = -Character, names_to = "Dimension", values_to = "Volume")
fig1_2_plots <- lapply(seq_along(c("Action", "Communication", "Description by Narrator", "Discussion of Character", "Global Anomaly Score (D²)", "Interiority", "Name Mentions")), function(i) {
  dims <- c("Action", "Communication", "Description by Narrator", "Discussion of Character", "Global Anomaly Score (D²)", "Interiority", "Name Mentions")
  cols <- c("#7FB3D5", "#F9E79F", "#BB8FCE", "#F1948A", "#85C1E9", "#F8C471", "#ABEBC6")
  ggplot(filter(fig1_2_bars_data, Dimension == dims[i]), aes(x = reorder(Character, Volume), y = Volume)) +
    geom_bar(stat = "identity", fill = cols[i], color = "grey40", size = 0.2, width = 0.7) +
    coord_flip() + labs(title = dims[i], x = NULL, y = NULL) + theme_manuscript()
})
fig1_2_final <- wrap_plots(fig1_2_plots) + plot_annotation(title = "Figure 1.2: True Macro-Structural Volume Landscape", subtitle = "Definite Riemann Integration Over 61 Chapters")
ggsave("FIGURE_1_2_TRUE_MACRO_STRUCTURAL_VOLUME_LANDSCAPE.png", plot = fig1_2_final, width = 11, height = 12, units = "in", dpi = 300)

fig1_3 <- ggplot(raw_data %>% filter(Character %in% top_12_characters_DC), aes(x = Absolute_Chapter, y = DC, fill = reorder(Character, -DC, FUN = sum))) +
  geom_area(position = "stack", alpha = 0.85, color = "white", size = 0.1) +
  scale_fill_brewer(palette = "Spectral", name = "DC Volume Hierarchy") +
  labs(title = "Figure 1.3: Geometric Mass of Social Discussion", subtitle = "Cumulative Riemann Integration of DC Across 61 Chapters", x = "Chapters 1-61", y = "Stacked DC Magnitude") +
  theme_manuscript()
ggsave("FIGURE_1_3_GEOMETRIC_MASS_OF_DC.png", plot = fig1_3, width = 10, height = 6.5, units = "in", dpi = 300)

fig1_4 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Jane", "Mr. Darcy", "Mrs. Bennet")), aes(x = Absolute_Chapter, y = DC, color = Character, fill = Character)) +
  geom_area(alpha = 0.15, size = 0.5) + geom_line(size = 0.8) + facet_wrap(~ Character, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  scale_fill_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  labs(title = "Figure 1.4: Trajectory of Character Discussion (DC)", x = "Chapters 1-61", y = "DC Score") +
  theme_manuscript() + theme(legend.position = "none")
ggsave("FIGURE_1_4_TRAJECTORY_OF_DC.png", plot = fig1_4, width = 9.0, height = 8, units = "in", dpi = 300)

fig1_5 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Mr. Darcy")), aes(x = Absolute_Chapter, y = DC, color = Character)) +
  geom_line(size = 1.2) + scale_color_manual(values = c("Elizabeth" = "#C0392B", "Mr. Darcy" = "#2980B9")) +
  annotate("rect", xmin = 35, xmax = 36, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#F39C12") +
  labs(title = "Figure 1.5: Macro-Temporal Social Scrutiny: Elizabeth and Darcy", subtitle = "Direct Covariance of DC Across the 61 Chapters", x = "Chapters 1-61", y = "DC Intensity Metric") +
  theme_manuscript()
ggsave("FIGURE_1_5_DC_COVARIANCE.png", plot = fig1_5, width = 9.5, height = 5.5, units = "in", dpi = 300)

v_a_mean  <- mean(riemann_bars_DC$Action, na.rm = TRUE)
v_dc_mean <- mean(riemann_bars_DC$`Discussion of Character`, na.rm = TRUE)

fig1_6 <- ggplot(riemann_bars_DC, aes(x = Action, y = `Discussion of Character`, label = Character)) +
  geom_vline(xintercept = v_a_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_hline(yintercept = v_dc_mean, linetype = "dashed", color = "grey60", size = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(family = "serif", size = 3.5, fontface = "bold") +
  scale_color_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Figure 1.6: Character Topology Map", x = "Action Volume", y = "Discussion of Character Volume") +
  theme_manuscript()
ggsave("FIGURE_1_6_DC_TOPOLOGY_MAP.png", plot = fig1_6, width = 8.5, height = 7, units = "in", dpi = 300)
