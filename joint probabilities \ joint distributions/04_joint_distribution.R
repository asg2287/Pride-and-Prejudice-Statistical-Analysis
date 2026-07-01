
library(dplyr)
library(tidyr)
library(ggplot2)


core_and_mid_tier <- data.frame(
  Character = c(
    "Elizabeth", "Mr. Darcy", "Jane", "Bingley", "Wickham", "Mr. Bennet", 
    "Mrs. Bennet", "Lady Catherine", "Mr. Collins", "Lydia", "Charlotte Lucas", 
    "Miss Bingley", "Mary", "Kitty", "Mrs. Gardiner", "Mr. Gardiner", 
    "Georgiana Darcy", "Colonel Fitzwilliam", "Sir William Lucas", "Mrs. Phillips", 
    "Mrs. Hurst", "Mr. Hurst", "Miss de Bourgh", "Mr. Phillips", "Mrs. Annesley", 
    "Captain Carter"
  ),
  N  = c(1062, 422, 281, 238, 172, 160, 155, 112, 104, 91, 74, 53, 44, 40, 36, 31, 22, 21, 19, 15, 11, 8, 7, 5, 1, 3),
  DC = c(1271, 500, 241, 211, 151, 142, 137, 89, 83, 76, 61, 44, 34, 32, 28, 24, 18, 17, 14, 11, 9, 6, 5, 4, 2, 3),
  C  = c(561, 220, 120, 96, 65, 62, 58, 38, 32, 29, 24, 16, 11, 10, 9, 7, 5, 5, 4, 3, 2, 1, 1, 1, 0, 1),
  I  = c(1156, 315, 122, 94, 58, 48, 44, 25, 22, 18, 12, 8, 5, 4, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
  DN = c(132, 50, 24, 20, 12, 10, 9, 6, 5, 4, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  A  = c(767, 250, 135, 112, 84, 78, 75, 48, 45, 41, 32, 24, 19, 18, 15, 13, 10, 9, 8, 6, 5, 3, 3, 2, 2, 2)
)


missing_peripherals <- data.frame(
  Character = c(
    "Mr. Denny", "Mr. Pratt", "Mrs. Jenkinson", "Miss Younge", "Lady Lucas", 
    "Mr. Long", "Mrs. Long", "Miss Long", "Colonel Forster", "Mrs. Forster", 
    "Mary King", "Mr. Robinson", "Lady Catherine's Butler", "Mr. Jones (Apothecary)", 
    "Madam Jenkins", "Sarah (Maid)", "Hannah (Housemaid)", "John (Groom)", 
    "Mrs. Reynolds", "Mr. Stone", "Mrs. Younge", "Mrs. Nicholls", 
    "Richard (Stable boy)", "Sally", "Mr. Morris", "Mrs. Long's Nieces", 
    "The Netherfield Cook"
  ),
  N  = c(3, 2, 2, 2, 2, 1, 2, 1, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1),
  DC = c(3, 2, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
  C  = c(1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  I  = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  DN = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  A  = c(2, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1)
)


full_network <- rbind(core_and_mid_tier, missing_peripherals)
metrics <- c("N", "DC", "C", "I", "DN", "A")

spearman_matrix <- cor(as.matrix(full_network[, metrics]), method = "spearman")
spearman_melted <- as.data.frame(spearman_matrix)
spearman_melted$Var1 <- rownames(spearman_melted)
spearman_melted <- spearman_melted %>% 
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")


p1 <- ggplot(spearman_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", lwd = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Correlation)), color = "white", size = 3.5) +
  scale_fill_viridis_c(option = "mako", direction = -1, end = 0.9) +
  theme_minimal() +
  labs(
    title = "Spearman Rank Joint Correlations",
    subtitle = "Non-Linear Metric Coupling across Austen's Network Topology",
    x = NULL, y = NULL
  ) +
  theme(plot.title = element_text(face = "bold", size = 12))

labeled_nodes <- full_network[full_network$Character %in% c("Elizabeth", "Mr. Darcy", "Jane", "Bingley"), ]

p2 <- ggplot(full_network, aes(x = DC, y = I)) +
  geom_point(color = "#2b5c8f", size = 3.5, alpha = 0.6) +
  geom_text(data = labeled_nodes, aes(label = Character), 
            vjust = -1.2, fontface = "bold", size = 3) +
  theme_minimal() +
  labs(
    title = "Joint Scatter Space: DC vs. I",
    subtitle = "Discussion of Character by Others vs. Interiority",
    x = "Discussion of Character by Other Characters",
    y = "Interiority"
  ) +
  theme(plot.title = element_text(face = "bold", size = 12))


ggsave("/Users/ale/Desktop/joint_correlation_matrix.png", plot = p1, width = 6, height = 5, dpi = 300)
ggsave("/Users/ale/Desktop/joint_distribution_scatterspace.png", plot = p2, width = 6, height = 5, dpi = 300)


if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined_plot <- p1 + p2 + plot_layout(ncol = 2)
  ggsave("/Users/ale/Desktop/joint_distribution_analysis.png", plot = combined_plot, width = 11, height = 5, dpi = 300)
  print("--- SUCCESS: Combined plot and individual matrices exported to Desktop! ---")
} else {
  print("--- SUCCESS: Matrix and Scatterplots saved individually to Desktop! ---")
}
