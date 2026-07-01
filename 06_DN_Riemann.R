library(tidyverse)
library(readxl)
library(pracma)   
library(MASS)     
library(ggrepel)
library(patchwork)
library(RColorBrewer)

base_dir   <- path.expand("~/Desktop/Pride and Prejudice")
excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
csv_path   <- file.path(base_dir, "New Mahalanobis", "AUSTEN_NETWORK_MAHALANOBIS.csv")

if (!file.exists(excel_path)) stop("Excel file missing.")
if (!file.exists(csv_path)) stop("CSV file missing.")

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
  complete(Absolute_Chapter = 1:61, fill = list(A = 0, C = 0, DN = 0, DC = 0, I = 0, N = 0, D2 = 0)) %>%
  summarise(
    `Action`                            = pracma::trapz(Absolute_Chapter, A),
    `Communication`                     = pracma::trapz(Absolute_Chapter, C),
    `Description by Narrator`           = pracma::trapz(Absolute_Chapter, DN),
    `Discussion of Character by Others` = pracma::trapz(Absolute_Chapter, DC),
    `Global Anomaly Score (D²)`        = pracma::trapz(Absolute_Chapter, D2),
    `Interiority`                       = pracma::trapz(Absolute_Chapter, I),
    `Name Mentions`                     = pracma::trapz(Absolute_Chapter, N),
    .groups = 'drop'
  )

riemann_bars_1_2 <- global_integration %>% 
  filter(Character %in% c("Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", "Mr. Bingley", "Mr. Wickham", "Lydia", "Mr. Collins"))

top_12_characters_DN <- raw_data %>%
  group_by(Character) %>%
  summarise(Total_Imp = sum(DN, na.rm = TRUE)) %>%
  arrange(desc(Total_Imp)) %>%
  slice_max(Total_Imp, n = 12) %>%
  pull(Character)

fig1_1 <- ggplot(filter(raw_data, Character %in% top_12_characters_DN), aes(x = Absolute_Chapter, y = DN)) +
  geom_area(fill = "#E8DAEF", alpha = 0.7, color = "#884EA0", linewidth = 0.3) +
  facet_wrap(~ factor(Character, levels = top_12_characters_DN), ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(1, 61), breaks = c(1, 10, 21, 31, 41, 51, 61)) +
  labs(title = "Figure 1.1: Narrator Description Vector Trajectories",
       subtitle = "Continuous Riemann Volume Distribution of DN Across Top 12 Characters",
       x = "Chapters 1-61", y = "DN Intensity Metric") +
  theme_manuscript()
ggsave("FIGURE_1_1_DN_VECTOR_TRAJECTORIES.png", plot = fig1_1, width = 9.5, height = 11, units = "in", dpi = 300)

fig1_2_bars_data <- riemann_bars_1_2 %>%
  rename(`Discussion of Character` = `Discussion of Character by Others`) %>%
  pivot_longer(cols = -Character, names_to = "Dimension", values_to = "Volume")

fig1_2_plots <- list()
dimensions_list <- c("Action", "Communication", "Description by Narrator", 
                     "Discussion of Character", "Global Anomaly Score (D²)", 
                     "Interiority", "Name Mentions")
colors_list     <- c("#7FB3D5", "#F9E79F", "#BB8FCE", "#F1948A", "#85C1E9", "#F8C471", "#ABEBC6")

for(i in seq_along(dimensions_list)) {
  fig1_2_plots[[dimensions_list[i]]] <- ggplot(filter(fig1_2_bars_data, Dimension == dimensions_list[i]), aes(x = reorder(Character, Volume), y = Volume)) +
    geom_bar(stat = "identity", fill = colors_list[i], color = "grey40", linewidth = 0.2, width = 0.7) +
    coord_flip() + labs(title = dimensions_list[i], x = NULL, y = NULL) + theme_manuscript() +
    theme(plot.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_blank())
}

fig1_2_final <- (fig1_2_plots[[1]] + fig1_2_plots[[2]]) / (fig1_2_plots[[3]] + fig1_2_plots[[4]]) / (fig1_2_plots[[5]] + fig1_2_plots[[6]]) / fig1_2_plots[[7]] +
  plot_annotation(title = "Figure 1.2: True Macro-Structural Volume Landscape", subtitle = "Definite Riemann Integration Over 61 Chapters")
ggsave("FIGURE_1_2_DN_MACRO_LANDSCAPE.png", plot = fig1_2_final, width = 11, height = 12, units = "in", dpi = 300)

plot_data_13 <- raw_data %>% 
  filter(Character %in% top_12_characters_DN) %>%
  mutate(Character = fct_reorder(Character, DN, .fun = sum))

num_chars <- length(unique(plot_data_13$Character))
my_palette <- colorRampPalette(brewer.pal(12, "Paired"))(num_chars)

fig1_3 <- ggplot(plot_data_13, aes(x = Absolute_Chapter, y = DN, fill = Character)) +
  geom_area(position = "stack", alpha = 0.9, color = "grey20", linewidth = 0.1) +
  scale_fill_manual(values = my_palette, name = "DN Volume Hierarchy") +
  scale_x_continuous(limits = c(1, 61), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Figure 1.3: Geometric Mass of Narrator Description", 
       subtitle = "Cumulative Riemann Integration of DN Across 61 Chapters", 
       x = "Chapters 1-61", y = "Stacked DN Magnitude") +
  theme_manuscript() + 
  theme(legend.position = "right", legend.text = element_text(size = 8))

ggsave("FIGURE_1_3_GEOMETRIC_MASS_OF_DN.png", plot = fig1_3, width = 10, height = 6.5, units = "in", dpi = 300)

fig1_4 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Jane", "Mr. Darcy", "Mrs. Bennet")), aes(x = Absolute_Chapter, y = DN, color = Character, fill = Character)) +
  geom_area(alpha = 0.15, linewidth = 0.5) + geom_line(linewidth = 0.8) + facet_wrap(~ Character, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  scale_fill_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
  labs(title = "Trajectory of Narrator Description (DN)", x = "Chapters 1-61", y = "DN Score") +
  theme_manuscript() + theme(legend.position = "none")
ggsave("FIGURE_1_4_TRAJECTORY_OF_DN.png", plot = fig1_4, width = 9.0, height = 8, units = "in", dpi = 300)

fig1_5 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Mr. Darcy")), aes(x = Absolute_Chapter, y = DN, color = Character)) +
  geom_line(linewidth = 1.2) + scale_color_manual(values = c("Elizabeth" = "#C0392B", "Mr. Darcy" = "#2980B9")) +
  annotate("rect", xmin = 35, xmax = 36, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#F39C12") +
  labs(title = "Macro-Temporal Narratorial Scrutiny: Elizabeth and Darcy", subtitle = "Direct Covariance of DN Across the 61 Chapters", x = "Chapters 1-61", y = "DN Intensity") +
  theme_manuscript()
ggsave("FIGURE_1_5_DN_COVARIANCE.png", plot = fig1_5, width = 9.5, height = 5.5, units = "in", dpi = 300)

fig1_6 <- ggplot(riemann_bars_1_2, aes(x = Action, y = `Description by Narrator`, label = Character)) +
  geom_vline(xintercept = mean(riemann_bars_1_2$Action), linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_hline(yintercept = mean(riemann_bars_1_2$`Description by Narrator`), linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(aes(size = `Global Anomaly Score (D²)`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
  geom_text_repel(family = "serif", size = 3.5, fontface = "bold") +
  scale_color_gradient(low = "#deebf7", high = "#3182bd") +
  labs(title = "Figure 1.6: Character Topology Map", x = "Action Volume", y = "Narrator Description Volume") +
  theme_manuscript()
ggsave("FIGURE_1_6_DN_TOPOLOGY_MAP.png", plot = fig1_6, width = 8.5, height = 7, units = "in", dpi = 300)