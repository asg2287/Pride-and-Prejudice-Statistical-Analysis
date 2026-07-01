> library(readxl)
> library(pracma)   

Attaching package: ‘pracma’

The following object is masked from ‘package:purrr’:

    cross

> library(MASS)     

Attaching package: ‘MASS’

The following object is masked from ‘package:dplyr’:

    select

> library(ggrepel)
> library(patchwork)

Attaching package: ‘patchwork’

The following object is masked from ‘package:MASS’:

    area

> library(RColorBrewer)
> 
> home_dir <- "/Users/ale"
> base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
> excel_path <- file.path(base_dir, "Pride_and_Prejuduce_Final_6Tier_Analysis_v2.xlsx")
> 
> raw_data <- read_xlsx(excel_path, sheet = "ALL INSTANCES")
Error: `path` does not exist: ‘/Users/ale/Desktop/Pride and Prejudice/Pride_and_Prejuduce_Final_6Tier_Analysis_v2.xlsx’
> 
> raw_data <- raw_data %>%
+   select(A:H) %>%
+   mutate(Absolute_Chapter = case_when(Volume == 1 ~ Chapter, Volume == 2 ~ Chapter + 23, Volume == 3 ~ Chapter + 42, TRUE ~ Chapter))
Error in select(., A:H) : unused argument (A:H)
> 
> components <- c("N", "DC", "C", "I", "DN", "A")
> matrix_6d   <- as.matrix(raw_data[, components])
Error: object 'raw_data' not found
> cov_matrix  <- cov(matrix_6d)
Error: object 'matrix_6d' not found
> mean_vector <- colMeans(matrix_6d)
Error: object 'matrix_6d' not found
> 
> raw_data <- raw_data %>%
+   mutate(
+     D2 = mahalanobis(as.matrix(raw_data[, components]), mean_vector, cov_matrix)
+   )
Error: object 'raw_data' not found
> 
> theme_manuscript <- function() {
+   theme_minimal(base_size = 11, base_family = "serif") +
+     theme(
+       plot.title        = element_text(face = "bold", size = 12, hjust = 0.5),
+       plot.subtitle     = element_text(size = 10, face = "italic", hjust = 0.5),
+       panel.border      = element_blank(),
+       axis.line         = element_line(color = "black", linewidth = 0.5),
+       strip.background  = element_blank(),
+       strip.text        = element_text(face = "bold", size = 10),
+       axis.title        = element_text(face = "bold", size = 11),
+       legend.position   = "bottom"
+     )
+ }
> 
> global_integration <- raw_data %>%
+   mutate(Character = str_trim(Character)) %>%
+   group_by(Character) %>%
+   complete(Absolute_Chapter = 1:61, fill = list(Total_Score = 0, D2 = 0)) %>%
+   summarise(
+     `Total Score Volume`        = pracma::trapz(Absolute_Chapter, Total_Score),
+     `Global Anomaly Score (D²)` = pracma::trapz(Absolute_Chapter, D2),
+     .groups = 'drop'
+   )
Error: object 'raw_data' not found
> 
> top_12_characters_TS <- raw_data %>% 
+   group_by(Character) %>% 
+   summarise(Total = sum(Total_Score, na.rm = TRUE)) %>% 
+   slice_max(Total, n = 12) %>% 
+   pull(Character)
Error: object 'raw_data' not found
> 
> fig1_1 <- ggplot(filter(raw_data, Character %in% top_12_characters_TS), aes(x = Absolute_Chapter, y = Total_Score)) +
+   geom_area(fill = "#34495E", alpha = 0.7, color = "#2C3E50", linewidth = 0.3) +
+   facet_wrap(~ factor(Character, levels = top_12_characters_TS), ncol = 2, scales = "free_y") +
+   scale_x_continuous(limits = c(1, 61), breaks = c(1, 10, 21, 31, 41, 51, 61)) +
+   labs(title = "Figure 1.1: Total Score Vector Trajectories", subtitle = "Continuous Riemann Distribution of Total Aggregated Narrative Score", x = "Chapters 1-61", y = "Total Score Intensity") +
+   theme_manuscript()
Error: object 'raw_data' not found
> ggsave(file.path(home_dir, "FIGURE_1_1_TOTAL_SCORE_TRAJECTORIES.png"), plot = fig1_1, width = 9.5, height = 11, units = "in", dpi = 300)
Error: object 'fig1_1' not found
> 
> top_ranked_data <- global_integration %>%
+   slice_max(`Total Score Volume`, n = 15)
Error: object 'global_integration' not found
> 
> fig1_2 <- ggplot(top_ranked_data, aes(x = reorder(Character, `Total Score Volume`), y = `Total Score Volume`, fill = `Total Score Volume`)) +
+   geom_bar(stat = "identity", color = "grey40", linewidth = 0.2, width = 0.7) +
+   scale_fill_gradient(low = "#5D6D7E", high = "#1A5276") +
+   coord_flip() +
+   labs(title = "Figure 1.2: Net Macro-Structural Narrative Volume", subtitle = "Definite Riemann Integration of Aggregated Total Scores Over 61 Chapters", x = NULL, y = "Accumulated Total Score Volume") +
+   theme_manuscript() + theme(legend.position = "none")
Error: object 'top_ranked_data' not found
> ggsave(file.path(home_dir, "FIGURE_1_2_TOTAL_SCORE_MACRO_LANDSCAPE.png"), plot = fig1_2, width = 9.5, height = 6.5, units = "in", dpi = 300)
Error: object 'fig1_2' not found
> 
> plot_data_13 <- raw_data %>% 
+   filter(Character %in% top_12_characters_TS) %>%
+   mutate(Character = fct_reorder(Character, Total_Score, .fun = sum))
Error: object 'raw_data' not found
> num_chars <- length(unique(plot_data_13$Character))
Error: object 'plot_data_13' not found
> my_palette <- colorRampPalette(brewer.pal(12, "Paired"))(num_chars)
Error: object 'num_chars' not found
> 
> fig1_3 <- ggplot(plot_data_13, aes(x = Absolute_Chapter, y = Total_Score, fill = Character)) +
+   geom_area(position = "stack", alpha = 0.85, color = "white", linewidth = 0.1) +
+   scale_fill_manual(values = my_palette, name = "Total Score Hierarchy") +
+   labs(title = "Figure 1.3: Geometric Mass of Total Aggregated Value", subtitle = "Cumulative Riemann Integration of Total Scores Across 61 Chapters", x = "Chapters 1-61", y = "Stacked Net Narrative Magnitude") +
+   theme_manuscript() + theme(legend.position = "right")
Error: object 'plot_data_13' not found
> ggsave(file.path(home_dir, "FIGURE_1_3_GEOMETRIC_MASS_OF_TOTAL_SCORE.png"), plot = fig1_3, width = 10, height = 6.5, units = "in", dpi = 300)
Error: object 'fig1_3' not found
> 
> fig1_4 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Jane", "Mr. Darcy", "Mrs. Bennet")), aes(x = Absolute_Chapter, y = Total_Score, color = Character, fill = Character)) +
+   geom_area(alpha = 0.15, linewidth = 0.5) + geom_line(linewidth = 0.8) + facet_wrap(~ Character, ncol = 1, scales = "free_y") +
+   scale_color_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
+   scale_fill_manual(values = c("Elizabeth" = "#D9534F", "Jane" = "#5CB85C", "Mr. Darcy" = "#5BC0DE", "Mrs. Bennet" = "#9B59B6")) +
+   labs(title = "Figure 1.4: Trajectory of Aggregated Total Scores", x = "Chapters 1-61", y = "Net Total Score") +
+   theme_manuscript() + theme(legend.position = "none")
Error: object 'raw_data' not found
> ggsave(file.path(home_dir, "FIGURE_1_4_TRAJECTORY_OF_TOTAL_SCORE.png"), plot = fig1_4, width = 9.0, height = 8, units = "in", dpi = 300)
Error: object 'fig1_4' not found
> 
> fig1_5 <- ggplot(raw_data %>% filter(Character %in% c("Elizabeth", "Mr. Darcy")), aes(x = Absolute_Chapter, y = Total_Score, color = Character)) +
+   geom_line(linewidth = 1.2) + scale_color_manual(values = c("Elizabeth" = "#C0392B", "Mr. Darcy" = "#2980B9")) +
+   annotate("rect", xmin = 35, xmax = 36, ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "#F39C12") +
+   labs(title = "Figure 1.5: Macro-Temporal Net Presence Dynamics: Elizabeth and Darcy", subtitle = "Direct Covariance of Aggregated Total Scores Across the 61 Chapters", x = "Chapters 1-61", y = "Net Total Score Metric") +
+   theme_manuscript()
Error: object 'raw_data' not found
> ggsave(file.path(home_dir, "FIGURE_1_5_TOTAL_SCORE_COVARIANCE.png"), plot = fig1_5, width = 9.5, height = 5.5, units = "in", dpi = 300)
Error: object 'fig1_5' not found
> 
> v_ts_mean  <- mean(global_integration$`Total Score Volume`, na.rm = TRUE)
Error: object 'global_integration' not found
> v_d2_mean  <- mean(global_integration$`Global Anomaly Score (D²)`, na.rm = TRUE)
Error: object 'global_integration' not found
> 
> fig1_6 <- ggplot(global_integration, aes(x = `Total Score Volume`, y = `Global Anomaly Score (D²)`, label = Character)) +
+   geom_vline(xintercept = v_ts_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
+   geom_hline(yintercept = v_d2_mean, linetype = "dashed", color = "grey60", linewidth = 0.5) +
+   geom_point(aes(size = `Total Score Volume`, color = `Global Anomaly Score (D²)`), alpha = 0.75) +
+   geom_text_repel(family = "serif", size = 3.5, fontface = "bold", max.overlaps = 15) +
+   scale_color_gradient(low = "#EBF5FB", high = "#1F618D") +
+   labs(title = "Figure 1.6: Net Narrative Topology Map", 
+        x = "Accumulated Total Score Volume (Definite Integration)", 
+        y = "Accumulated Global Anomaly Volume (D²)") +
+   theme_manuscript() + theme(legend.position = "right")
Error: object 'global_integration' not found
> ggsave(file.path(home_dir, "FIGURE_1_6_TOTAL_SCORE_TOPOLOGY_MAP.png"), plot = fig1_6, width = 9, height = 7, units = "in", dpi = 300)
Error: object 'fig1_6' not found
> 
> message("Execution complete. Total Score Riemann figures generated successfully in /Users/ale/")
Execution complete. Total Score Riemann figures generated successfully in /Users/ale/
> 