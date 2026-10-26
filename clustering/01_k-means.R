library(cluster)
library(factoextra)
library(ggplot2)
library(gridExtra)

df <- data.frame(
  Character = c(
    "Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", "Mr. Bingley", 
    "Wickham", "Lydia", "Mr. Bennet", "Mr. Collins", "Lady Catherine", 
    "Charlotte Lucas", "Miss Bingley", "Mr. Gardiner", "Mrs. Gardiner", "Kitty", 
    "Miss Darcy", "Sir William Lucas", "Colonel Fitzwilliam", "Mary", "Miss de Bourgh", 
    "Mrs. Philips", "Mrs. Reynolds", "Mrs. Hurst", "Colonel Forster", "Maria Lucas", 
    "Lady Lucas", "Miss King", "Mrs. Forster", "Mrs. Long", "Mr. Denny", 
    "Mr. Hurst", "Mrs. Jenkinson", "Darcy's father", "Mr. Philips", "Mrs. Younge", 
    "Hill", "Mr. Jones", "Mrs. Annesley", "Young Lucas", "Captain Carter", 
    "Lady Anne Darcy", "Mr. Robinson", "Mrs. Nicholls", "Dawson", "John", 
    "Nicholls", "Richard", "Sarah", "Wickham Sr.", "Miss Watson", "Mr. Morris"
  ),
  Total_Score = c(
    3331, 1618, 1002, 751, 701, 671, 532, 526, 513, 422, 325, 319, 239, 207, 205, 
    196, 116, 112, 98, 78, 73, 58, 50, 49, 45, 36, 35, 34, 31, 26, 25, 23, 21, 17, 
    16, 16, 9, 8, 6, 6, 4, 4, 4, 2, 2, 2, 2, 2, 2, 1, 1
  ),
  N  = c(731, 331, 297, 137, 209, 183, 145, 89, 147, 111, 132, 97, 54, 54, 76, 51, 42, 31, 33, 19, 26, 10, 21, 25, 18, 16, 6, 13, 10, 10, 12, 9, 4, 7, 4, 8, 2, 3, 1, 3, 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1),
  C  = c(549, 722, 295, 118, 309, 343, 187, 169, 110, 147, 79, 71, 64, 47, 46, 114, 11, 37, 11, 49, 16, 14, 6, 23, 5, 7, 29, 17, 14, 7, 2, 5, 17, 5, 9, 3, 0, 1, 1, 3, 2, 1, 2, 1, 1, 1, 1, 1, 1, 0, 0),
  I  = c(426, 135, 95, 161, 46, 45, 45, 99, 68, 55, 23, 61, 20, 26, 11, 0, 12, 18, 9, 0, 7, 15, 7, 0, 5, 3, 0, 0, 5, 2, 3, 1, 0, 2, 0, 2, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  A  = c(993, 163, 172, 174, 32, 29, 63, 79, 50, 30, 28, 34, 23, 32, 17, 13, 15, 1, 19, 0, 8, 9, 6, 1, 7, 2, 0, 0, 0, 0, 2, 2, 0, 0, 0, 1, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  DC = c(74, 58, 31, 60, 27, 20, 25, 32, 27, 12, 13, 12, 6, 3, 17, 7, 7, 4, 12, 4, 4, 1, 1, 0, 2, 1, 0, 2, 0, 0, 2, 1, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  DN = c(558, 209, 112, 101, 78, 51, 67, 58, 111, 67, 50, 44, 72, 45, 38, 11, 29, 21, 14, 6, 12, 9, 9, 0, 8, 7, 0, 2, 2, 7, 4, 5, 0, 2, 3, 2, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

feature_matrix <- df[, c("N", "C", "I", "A", "DC", "DN")]
scaled_matrix <- scale(feature_matrix)

desktop_dir <- "~/Desktop/Pride_And_Prejudice_KMeans_Output"
if (!dir.exists(desktop_dir)) dir.create(desktop_dir, recursive = TRUE)

p_elbow <- fviz_nbclust(scaled_matrix, kmeans, method = "wss", k.max = 8) +
  geom_vline(xintercept = 4, linetype = 2, color = "red") +
  labs(
    title = "(A) Elbow Optimization Curve (WCSS)",
    subtitle = "Optimal inflection point identified at K = 4",
    x = "Number of Clusters (K)",
    y = "Within-Cluster Sum of Squares (WCSS)"
  ) +
  theme_minimal(base_size = 11)

sil_plots <- list()

for (k in 2:4) {
  set.seed(42)
  km_k <- kmeans(scaled_matrix, centers = k, algorithm = "Lloyd", nstart = 25)
  sil_k <- silhouette(km_k$cluster, dist(scaled_matrix))
  
  p_sil_k <- fviz_silhouette(sil_k, label = FALSE) +
    labs(
      title = paste0("(", LETTERS[k], ") Silhouette Profile (K = ", k, ")"),
      subtitle = sprintf("Avg Silhouette Width: %.2f", mean(sil_k[, 3])),
      x = "Characters (N = 51)",
      y = "Silhouette Width (s_i)"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  sil_plots[[paste0("k", k)]] <- p_sil_k
}

combined_fig <- grid.arrange(
  p_elbow, sil_plots[["k2"]], 
  sil_plots[["k3"]], sil_plots[["k4"]], 
  ncol = 2, nrow = 2
)

ggsave(
  file.path(desktop_dir, "Figure_3_3_Combined_KMeans_Diagnostics.png"), 
  combined_fig, 
  width = 11, 
  height = 8, 
  dpi = 300
)
