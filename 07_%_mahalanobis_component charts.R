library(readxl)
> library(ggplot2)
> 
>
> file_path <- "/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx"
> sheet3_percentages <- read_excel(file_path, sheet = "CHARACTER PERCENTAGES")
> X_raw <- as.data.frame(sheet3_percentages)
> 

> components <- list(
+   "Name Mentions"        = "N %",
+   "Discussion"           = "DC %",
+   "Communication"        = "C %",
+   "Interiority"          = "I %",
+   "Narrator Description" = "DN %",
+   "Action"               = "A %"
+ )
> 
>
> for (comp_display_name in names(components)) {
+   target_col <- components[[comp_display_name]]
+   

+   global_mean <- mean(X_raw[[target_col]], na.rm = TRUE)
+   global_sd   <- sd(X_raw[[target_col]], na.rm = TRUE)
+
+   plot_data <- data.frame(
+     Character = X_raw$Character,
+     Raw_Pct   = X_raw[[target_col]],
+     Z_Score   = (X_raw[[target_col]] - global_mean) / global_sd
+   )
+
+   plot_data$Character <- factor(plot_data$Character, levels = plot_data$Character[order(plot_data$Z_Score)])
+   
+
+   comp_plot <- ggplot(plot_data, aes(x = Character, y = Z_Score, fill = Z_Score)) +
+     geom_bar(stat = "identity", width = 0.75) +
+    
+     scale_fill_gradient2(low = "#3b0f70", mid = "#b13da1", high = "#fec287", 
+                          midpoint = 0, name = "Z-Score") +
+     coord_flip() +
+     labs(
+       title = paste("Component Profile Spectrum:", comp_display_name),
+       subtitle = paste0("Character Allocation Deviances (Measured in Standard Deviations from Global Mean of ", 
+                         round(global_mean, 2), "%)"),
+       x = "Character Profile",
+       y = "Standard Deviations from Austen Norm (Z-Score)"
+     ) +
+     theme_minimal(base_size = 11) +
+     theme(
+       plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
+       plot.subtitle = element_text(face = "italic", color = "gray30", size = 9, margin = margin(b = 15)),
+       axis.text.y = element_text(size = 8, face = "bold"),
+       axis.title = element_text(face = "bold"),
+       panel.grid.major.y = element_blank(),
+       panel.grid.minor = element_blank()
+     )
+   

+   clean_filename <- gsub(" ", "_", comp_display_name)
+   ggsave(
+     filename = paste0("/Users/ale/Desktop/DEVIATION_SPECTRUM_", clean_filename, ".pdf"),
+     plot = comp_plot,
+     width = 9,
+     height = 11,
+     device = "pdf"
+   )
+ }
> 
> print("All 6 component profiles have successfully generated on your Desktop!")
