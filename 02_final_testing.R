 rm(list = ls())
> 
> required_packages <- c("tidyverse", "MASS", "cluster", "ggrepel", "reshape2", "readxl", "car", "fmsb")
> for (pkg in required_packages) {
+   if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, quiet = TRUE)
+   library(pkg, character.only = TRUE)
+ }
> 

> file_instances <- "/Users/ale/Desktop/Pride and Prejudice/Pride and Prejudice, Summer 2026.xlsx"
> file_tiers     <- "/Users/ale/Desktop/Pride_and_Prejudice_Final_6Tier_Analysis_v2.xlsx"
> output_dir     <- "/Users/ale/Desktop/Austen_Analysis_Outputs/"
> 
> if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
> 
> if (!file.exists(file_instances)) stop(paste("File missing:", file_instances))
> if (!file.exists(file_tiers)) stop(paste("File missing:", file_tiers))
>
> df_instances_raw <- read_excel(file_instances, sheet = "ALL INSTANCES")
                                                                         
> 
> df_instances_clean <- df_instances_raw %>%
+   dplyr::select(
+     Character = `Character`,
+     Graph_Chapter = `Graph Chapter`,
+     Total_Score = `Total Score`,
+     N, DC, C, I, DN, A
+   ) %>%
+   mutate(across(c(Graph_Chapter, N, DC, C, I, DN, A, Total_Score), as.numeric)) %>%
+   filter(!is.na(Character) & !is.na(Graph_Chapter))
> 

> df_chars_raw <- read_excel(file_tiers)
                                                                         
> 
> char_tiers <- df_chars_raw %>%
+   dplyr::select(
+     Character = `Character`,
+     KMeans_Cluster = `K-Means`
+   ) %>%
+   filter(!is.na(Character) & !is.na(KMeans_Cluster)) %>%
+   distinct(Character, .keep_all = TRUE)
> 

> component_tags <- c("N", "DC", "C", "I", "DN", "A")
> 
> char_summary <- df_instances_clean %>%
+   group_by(Character) %>%
+   summarize(across(all_of(component_tags), \(x) sum(x, na.rm = TRUE)),
+             TOTAL_SCORES = sum(Total_Score, na.rm = TRUE), .groups = 'drop') %>%
+   inner_join(char_tiers, by = "Character") %>%
+   mutate(KMeans_Cluster = factor(KMeans_Cluster))

> 
> shapiro_res <- shapiro.test(char_summary$TOTAL_SCORES)
> shapiro_csv <- data.frame(
+   Parameter = c("Statistic (W)", "P-Value (p)", "Methodology Profile"),
+   Value = c(as.character(shapiro_res$statistic), as.character(shapiro_res$p.value), "Shapiro-Wilk Normality Matrix")
+ )
> write.csv(shapiro_csv, file.path(output_dir, "shapiro_normality_report.csv"), row.names = FALSE)
> 
> char_long <- char_summary %>% 
+   pivot_longer(cols = all_of(component_tags), names_to = "Component", values_to = "Count")
> 
> chisq_table <- xtabs(Count ~ Component + KMeans_Cluster, data = char_long)
> chisq_res <- chisq.test(chisq_table)
> 
> chisq_csv <- data.frame(
+   Engine = "Chi-Squared Test of Independence",
+   ChiSquared_Value = chisq_res$statistic,
+   Df = chisq_res$parameter,
+   P_Value = chisq_res$p.value
+ )
> write.csv(chisq_csv, file.path(output_dir, "chisq_independence_report.csv"), row.names = FALSE)
> write.csv(as.data.frame.matrix(chisq_table), file.path(output_dir, "contingency_table_frequencies.csv"))
> 
> anova_fit <- aov(TOTAL_SCORES ~ KMeans_Cluster, data = char_summary)
> anova_summary <- summary(anova_fit)[[1]]
> anova_csv <- data.frame(
+   Source = c("Cluster Matrix Segment", "Residual Workspace"),
+   Df = anova_summary$Df,
+   Sum_Sq = anova_summary$`Sum Sq`,
+   Mean_Sq = anova_summary$`Mean Sq`,
+   F_Value = c(anova_summary$`F value`[1], NA),
+   Pr_F = c(anova_summary$`Pr(>F)`[1], NA)
+ )
> write.csv(anova_csv, file.path(output_dir, "anova_variance_report.csv"), row.names = FALSE)
> 
> tukey_res <- as.data.frame(TukeyHSD(anova_fit)$KMeans_Cluster)
> tukey_res$Comparison <- rownames(tukey_res)
> write.csv(tukey_res %>% dplyr::select(Comparison, everything()), file.path(output_dir, "tukey_posthoc_pairwise.csv"), row.names = FALSE)
> 
> dependent_matrix <- as.matrix(char_summary[, component_tags])
> manova_fit <- manova(dependent_matrix ~ KMeans_Cluster, data = char_summary)
> manova_summary <- summary(manova_fit, test = "Pillai")$stats
> manova_csv <- data.frame(
+   Operator = c("Group Separation Matrix", "Residual Workspace"),
+   Df = manova_summary[1:2, "Df"],
+   Pillais_Trace = c(manova_summary[1, "Pillai"], NA),
+   Approx_F = c(manova_summary[1, "Approx F"], NA),
+   Num_Df = c(manova_summary[1, "Num Df"], NA),
+   Den_Df = c(manova_summary[1, "Den Df"], NA),
+   Pr_F = c(manova_summary[1, "Pr(>F)"], NA)
+ )
Error in manova_summary[1, "Approx F"] : subscript out of bounds
> write.csv(manova_csv, file.path(output_dir, "manova_multivariate_report.csv"), row.names = FALSE)
Error in eval(expr, p) : object 'manova_csv' not found
> 
> pca_fit <- prcomp(char_summary[, component_tags], scale. = TRUE)
> pca_scores <- as.data.frame(pca_fit$x)
> pca_scores <- cbind(Character = char_summary$Character, Tier = char_summary$KMeans_Cluster, pca_scores)
> write.csv(pca_scores, file.path(output_dir, "pca_matrix_projections.csv"), row.names = FALSE)
> 
> corr_matrix <- cor(char_summary[, component_tags])
> write.csv(as.data.frame(corr_matrix), file.path(output_dir, "correlation_matrix_coefficients.csv"))
>
> hexagon_means <- char_summary %>% 
+   group_by(KMeans_Cluster) %>% 
+   summarize(across(all_of(component_tags), \(x) mean(x, na.rm = TRUE)), .groups = 'drop')
> 
> raw_data_only <- as.data.frame(hexagon_means[, -1])
> 
> max_bounds <- apply(raw_data_only, 2, max) * 1.15
> min_bounds <- rep(0, length(component_tags))
> radar_matrix <- rbind(max_bounds, min_bounds, raw_data_only)
> rownames(radar_matrix) <- c("MAX", "MIN", as.character(hexagon_means$KMeans_Cluster))
> 
> num_tiers <- nrow(hexagon_means)
> colors_border <- c("#3498db", "#27ae60", "#e74c3c", "#9b59b6", "#f1c40f", "#e67e22", "#1abc9c")[1:num_tiers]
> colors_background <- paste0(colors_border, "33")
> 
> pdf(file.path(output_dir, "figure2_hexagon_spider.pdf"), width = 9, height = 9)
> par(mar = c(2, 2, 4, 2))
> radarchart(radar_matrix, axistype = 1, seg = 5,
+            pcol = colors_border, pfcol = colors_background, plwd = 3, plty = 1,
+            cglcol = "grey70", cglty = 2, axislabcol = "grey30",
+            vlcex = 1.1, title = "Figure 2.2: Character Stratification Behavior Radar Mapping")
> legend(x = "topright", legend = rownames(radar_matrix)[3:nrow(radar_matrix)], 
+        bty = "n", pch = 20, col = colors_border, text.col = "black", cex = 1.0, pt.cex = 2)
> dev.off()
null device 
          1 
> 
> loadings_df <- as.data.frame(pca_fit$rotation)
> loadings_df$Variable <- rownames(loadings_df)
> mult <- max(abs(pca_scores$PC1)) / max(abs(loadings_df$PC1)) * 0.75
> 
> fig_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
+   geom_point(aes(color = Tier), size = 3.5, alpha = 0.9) +
+   scale_color_manual(values = colors_border) +
+   geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult), 
+                arrow = arrow(length = unit(0.2, "cm")), color = "#c0392b", linewidth = 0.8) +
+   geom_text(data = loadings_df, aes(x = PC1 * mult * 1.15, y = PC2 * mult * 1.15, label = Variable), 
+             color = "#c0392b", fontface = "bold", size = 4) +
+   geom_text_repel(aes(label = Character), size = 2.8, fontface = "bold", max.overlaps = 40) + 
+   theme_bw() + labs(title = "Figure 4: PCA Vector Space Field Mapping")
> ggsave(file.path(output_dir, "figure4_pca_biplot.pdf"), plot = fig_pca, width = 9, height = 7)
> 
> melted_corr <- melt(corr_matrix)
> fig_corr <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) + 
+   geom_tile() + geom_text(aes(label = round(value, 2)), fontface = "bold") +
+   scale_fill_gradient2(low = "#2980b9", high = "#e74c3c", midpoint = 0) + theme_minimal()
> ggsave(file.path(output_dir, "figure2_correlation_heatmap.pdf"), plot = fig_corr, width = 7, height = 6)
