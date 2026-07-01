rm(list = ls())
> library(tidyverse)
> library(readxl)
> 

> file_instances <- "/Users/ale/Desktop/Pride and Prejudice/Pride and Prejudice, Summer 2026.xlsx"
> file_tiers     <- "/Users/ale/Desktop/Pride_and_Prejudice_Final_6Tier_Analysis_v2.xlsx"
> output_dir     <- "/Users/ale/Desktop/Austen_Analysis_Outputs/"
> 

> df_instances_raw <- read_excel(file_instances, sheet = "ALL INSTANCES")
                                                                         
> df_chars_raw     <- read_excel(file_tiers)
                                                                         
> 
> component_tags <- c("N", "DC", "C", "I", "DN", "A")
> 

> df_clean <- df_instances_raw %>%
+   dplyr::select(Character, N, DC, C, I, DN, A) %>%
+   mutate(across(all_of(component_tags), as.numeric)) %>%
+   filter(!is.na(Character))
> 
> char_tiers <- df_chars_raw %>%
+   dplyr::select(Character, KMeans_Cluster = `K-Means`) %>%
+   filter(!is.na(Character) & !is.na(KMeans_Cluster)) %>%
+   distinct(Character, .keep_all = TRUE)
> 
>
> df_manova_input <- df_clean %>%
+   inner_join(char_tiers, by = "Character") %>%
+   mutate(KMeans_Cluster = factor(KMeans_Cluster))
> 

> dependent_vars <- as.matrix(df_manova_input[, component_tags])
> 

> manova_model <- manova(dependent_vars ~ KMeans_Cluster, data = df_manova_input)
> manova_summary <- summary(manova_model, test = "Pillai")$stats
> 
>
> df_manova_csv <- data.frame(
+   Matrix_Operator = c("Group Separation Matrix", "Residual Workspace"),
+   Df              = manova_summary[1:2, 1],
+   Pillais_Trace   = c(manova_summary[1, 2], NA),
+   Approx_F        = c(manova_summary[1, 3], NA),
+   Num_Df          = c(manova_summary[1, 4], NA),
+   Den_Df          = c(manova_summary[1, 5], NA),
+   Pr_F            = c(manova_summary[1, 6], NA)
+ )
>
> write.csv(df_manova_csv, file.path(output_dir, "manova_structural_breakdown.csv"), row.names = FALSE)
> 
>
> cluster_avges <- df_clean %>%
+   inner_join(char_tiers, by = "Character") %>%
+   group_by(KMeans_Cluster) %>%
+   summarize(across(all_of(component_tags), \(x) sum(x, na.rm = TRUE)), .groups = 'drop')
> 
>
> cluster_proportions <- cluster_avges %>%
+   rowwise() %>%
+   mutate(Total_Volume = sum(c_across(all_of(component_tags)))) %>%
+   mutate(across(all_of(component_tags), ~ (. / Total_Volume) * 100)) %>%
+   ungroup() %>%
+   dplyr::select(-Total_Volume) %>%
+   mutate(KMeans_Cluster = factor(paste0("Cluster K", KMeans_Cluster)))
> 

> plot_data <- cluster_proportions %>%
+   pivot_longer(cols = all_of(component_tags), names_to = "Metric", values_to = "Percentage")
> 
>
> loop_data <- plot_data %>%
+   group_by(KMeans_Cluster) %>%
+   do(rbind(., data.frame(KMeans_Cluster = .$"KMeans_Cluster"[1], 
+                          Metric = .$"Metric"[1], 
+                          Percentage = .$"Percentage"[1]))) %>%
+   ungroup()
>
> fig_radar <- ggplot(loop_data, aes(x = Metric, y = Percentage, group = KMeans_Cluster, color = KMeans_Cluster)) +
+   geom_polygon(aes(fill = KMeans_Cluster), alpha = 0.1, linewidth = 1.2) +
+   geom_point(size = 2.5) +
+   coord_polar(start = 0) +
+   scale_color_manual(values = c("#3498db", "#27ae60", "#e74c3c", "#9b59b6")) +
+   scale_fill_manual(values = c("#3498db", "#27ae60", "#e74c3c", "#9b59b6")) +
+   theme_minimal() +
+   theme(
+     panel.grid.major = element_line(color = "grey80", linetype = 2),
+     axis.text.x = element_text(face = "bold", size = 12, color = "black"),
+     axis.title.x = element_blank(),
+     axis.title.y = element_blank(),
+     plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
+   ) +
+   labs(title = "Figure 2.2: True Asymmetric Behavioral Footprint (% of Total Signature)",
+        color = "Strategic Tier", fill = "Strategic Tier")
> 

> ggsave(file.path(output_dir, "figure2_hexagon_spider_PROPORTIONAL.pdf"), plot = fig_radar, width = 8, height = 7)
> 

> cat("\n[SUCCESS] MANOVA structural matrix exported to 'manova_structural_breakdown.csv'.\n")
