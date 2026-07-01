library(readxl)
> library(stats)
> library(ggplot2)
> 
>
> file_path <- "/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx"
> sheet3_percentages <- read_excel(file_path, sheet = "CHARACTER PERCENTAGES")
Error: `path` does not exist: ‘/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx’
> 
> X_raw <- as.data.frame(sheet3_percentages)
> rownames(X_raw) <- X_raw$Character
> 
> pct_cols <- c("N %", "DC %", "C %", "I %", "DN %", "A %")
> pct_matrix <- X_raw[, pct_cols]
> pct_matrix[pct_matrix == 0] <- 0.01
> 
> ilr_transform <- function(X) {
+   logX <- log(X)
+   norm_matrix <- matrix(0, nrow=nrow(X), ncol=5)
+   for(i in 1:5) {
+     norm_matrix[,i] <- sqrt(i / (i + 1)) * (rowMeans(as.matrix(logX[, 1:i, drop=FALSE])) - logX[, i+1])
+   }
+   rownames(norm_matrix) <- rownames(X)
+   return(norm_matrix)
+ }
> 
> characters_ilr <- ilr_transform(pct_matrix)
> global_center   <- colMeans(characters_ilr)
> global_inv_cov  <- solve(cov(characters_ilr))
> d2_scores       <- mahalanobis(characters_ilr, center = global_center, cov = global_inv_cov, inverted = TRUE)
> 

> plot_data <- data.frame(
+   Character = X_raw$Character,
+   D2 = d2_scores
+ )
> 
>
> plot_data$Character <- factor(plot_data$Character, levels = plot_data$Character[order(plot_data$D2)])
> 

> architectural_plot <- ggplot(plot_data, aes(x = Character, y = D2, fill = D2)) +
+   geom_bar(stat = "identity", width = 0.75, show.legend = TRUE) +
+   scale_fill_viridis_c(option = "plasma", name = expression(D^2~~Distance)) +
+   coord_flip() +
+   labs(
+     title = "Character Architectural Deviation Spectrum",
+     subtitle = "Squared Mahalanobis Distance (D²) from Jane Austen's Global Baseline Style",
+     x = "Character Profile",
+     y = expression(Structural~Deviation~(D^2))
+   ) +
+   theme_minimal(base_size = 11) +
+   theme(
+     plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
+     plot.subtitle = element_text(face = "italic", color = "gray30", size = 10, margin = margin(b = 15)),
+     axis.text.y = element_text(size = 8, face = "bold"),
+     axis.title = element_text(face = "bold"),
+     panel.grid.major.y = element_blank(), 
+     panel.grid.minor = element_blank()
+   )
> 
>
> ggsave(
+   filename = "/Users/ale/Desktop/CHARACTER_ARCHITECTURAL_DEVIATION_SPECTRUM.pdf",
+   plot = architectural_plot,
+   width = 9,
+   height = 11,
+   device = "pdf"
+ )
> 
