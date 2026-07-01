library(readxl)
> library(tidyverse)
> library(cluster)
> 

> file_path <- "/Users/ale/Desktop/Pride_and_Prejudice_Final_6Tier_Analysis_v2.xlsx"
> if (!file.exists(file_path)) {
+   stop(paste("CRITICAL ERROR: Matrix file not found at path:", file_path))
+ }
> 
> df <- read_excel(file_path)
                                                                         
> 
>
> feature_cols <- c("N", "DC", "C", "I", "DN", "A")
> df[feature_cols] <- lapply(df[feature_cols], as.numeric)
> df$`Total Scores` <- as.numeric(df$`Total Scores`)
>
> matrix_data <- df %>%
+   filter(!is.na(`Total Scores`)) %>%
+   remove_rownames() %>%
+   column_to_rownames(var = "Character")
> 
> 
>
> cat("\n[TEST 1] Processing Global Subclusters (K1 Omitted)...\n")

[TEST 1] Processing Global Subclusters (K1 Omitted)...
> 

> global_sub_raw <- matrix_data[matrix_data$`K-Means` != "K1", ]
> global_sub_features <- global_sub_raw[, feature_cols]
> 
> global_sub_scaled <- scale(global_sub_features)
> global_sub_scaled[is.na(global_sub_scaled)] <- 0
> 
> set.seed(42)
> kmeans_global <- kmeans(global_sub_scaled, centers = 4, nstart = 25)
> 
>
> global_rankings <- tibble(
+   Raw_Cluster = kmeans_global$cluster,
+   Total_Volume = global_sub_raw$`Total Scores`
+ ) %>%
+   group_by(Raw_Cluster) %>%
+   summarize(Mean_Volume = mean(Total_Volume), .groups = 'drop') %>%
+   arrange(desc(Mean_Volume)) %>%
+   mutate(Letter_Cluster = c("KK1", "KK2", "KK3", "KK4"))
> 
> global_map <- deframe(dplyr::select(global_rankings, Raw_Cluster, Letter_Cluster))
> 
>
> global_results <- data.frame(global_sub_raw, check.names = FALSE) %>%
+   mutate(
+     Character = rownames(global_sub_raw),
+     Subcluster = global_map[as.character(kmeans_global$cluster)]
+   ) %>%
+   dplyr::select(Character, Subcluster, `Total Scores`, N, DC, C, I, DN, A, `Component Checklist`, `K-Means`, Pareto) %>%
+   arrange(Subcluster, desc(`Total Scores`))
> 
>
> write_csv(global_results, "/Users/ale/Desktop/PRIDE_PREJUDICE_GLOBAL_SUBCLUSTERS.csv")
                                                                         
> 

> png("/Users/ale/Desktop/PRIDE_PREJUDICE_GLOBAL_CLUSTERS_4D.png", width = 2400, height = 1600, res = 300)
> clusplot(global_sub_scaled, kmeans_global$cluster, color = TRUE, shade = TRUE,
+          labels = 2, lines = 0, main = "Global Subcluster Boundaries (KK1-KK4 Split)",
+          sub = "K1 Core Omitted to Prevent Scale Compression", family = "serif", font.main = 2)
> dev.off()
null device 
          1
> cat("\n[TEST 2] Algorithmatically Determining Optimal Peripheral Subclusters...\n")

[TEST 2] Algorithmatically Determining Optimal Peripheral Subclusters...
> 

> lower_tiers_raw <- matrix_data[matrix_data$Pareto == "P2", ]
> lower_tiers_features <- lower_tiers_raw[, feature_cols]
> 
> lower_tiers_scaled <- scale(lower_tiers_features)
> lower_tiers_scaled[is.na(lower_tiers_scaled)] <- 0
> 
>
> sil_widths <- numeric(5)
> names(sil_widths) <- 2:6
> 
> set.seed(42)
> for (k in 2:6) {
+   km_res <- kmeans(lower_tiers_scaled, centers = k, nstart = 25)
+   ss <- silhouette(km_res$cluster, dist(lower_tiers_scaled))
+   sil_widths[as.character(k)] <- mean(ss[, 3])
+ }
>
> optimal_k <- as.numeric(names(which.max(sil_widths)))
> cat(paste("--> Data Engine isolated highest Silhouette Width at K =", optimal_k, "subclusters.\n"))
--> Data Engine isolated highest Silhouette Width at K = 2 subclusters.
> 

> set.seed(42)
> kmeans_lower <- kmeans(lower_tiers_scaled, centers = optimal_k, nstart = 25)
> 
>
> pp_labels <- paste0("PP", 1:optimal_k)
> 
> lower_rankings <- tibble(
+   Raw_Cluster = kmeans_lower$cluster,
+   Total_Volume = lower_tiers_raw$`Total Scores`
+ ) %>%
+   group_by(Raw_Cluster) %>%
+   summarize(Mean_Volume = mean(Total_Volume), .groups = 'drop') %>%
+   arrange(desc(Mean_Volume)) %>%
+   mutate(Letter_Cluster = pp_labels)
> 
> lower_map <- deframe(dplyr::select(lower_rankings, Raw_Cluster, Letter_Cluster))
> 
>
> lower_results <- data.frame(lower_tiers_raw, check.names = FALSE) %>%
+   mutate(
+     Character = rownames(lower_tiers_raw),
+     Subcluster = lower_map[as.character(kmeans_lower$cluster)]
+   ) %>%
+   dplyr::select(Character, Subcluster, `Total Scores`, N, DC, C, I, DN, A, `Component Checklist`, `K-Means`, Pareto) %>%
+   arrange(Subcluster, desc(`Total Scores`))
>
> write_csv(lower_results, "/Users/ale/Desktop/PRIDE_PREJUDICE_ISOLATED_PERIPHERALS.csv")
                                                                         
> 

> color_palette <- colorRampPalette(c("#1E8449", "#7FB3D5", "#D4AC0D", "#C0392B"))(optimal_k)
> names(color_palette) <- pp_labels
> 
> peripheral_plot <- ggplot(lower_results, aes(x = reorder(Character, `Total Scores`), y = `Total Scores`, fill = Subcluster)) +
+   geom_bar(stat = "identity", width = 0.7) +
+   coord_flip() +
+   scale_fill_manual(values = color_palette) +
+   labs(
+     title = paste("Isolated Peripheral Subcluster Variance (Optimized K =", optimal_k, ")"),
+     subtitle = "Separating Mathematically Selected Subclusters (Pareto P2)",
+     x = "",
+     y = "Aggregate Structural Volume Score"
+   ) +
+   theme_minimal(base_family = "serif") +
+   theme(
+     plot.title = element_text(face = "bold", size = 12),
+     axis.text.y = element_text(size = 5, face = "bold"),
+     legend.position = "bottom"
+   )
> 

> ggsave("/Users/ale/Desktop/PRIDE_PREJUDICE_PERIPHERAL_BARS.png", plot = peripheral_plot, width = 9, height = 7, dpi = 300)
