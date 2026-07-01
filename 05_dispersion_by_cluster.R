> library(tidyverse)
> library(readxl)
> 
>
> home_dir   <- "/Users/ale"
> base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
> excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
> 
> if (!file.exists(excel_path)) stop("Longitudinal Excel spreadsheet missing.")
> 

> cluster_mapping <- tibble(
+   Name = c(
+     "Mrs. Bennet", "Jane", "Mr. Darcy",
+
+     "Mr. Bennet", "Lydia", "Miss Bingley", "Wickham", 
+     "Charlotte Lucas", "Lady Catherine de Bourgh", "Mr. Bingley", "Mr. Collins",
+
+     "Kitty", "Miss Darcy", "Mary", "Mrs. Philips", "Sir William Lucas", 
+     "Colonel Forster", "Lady Lucas", "Miss de Bourgh", "Mrs. Hurst", 
+     "Mr. Gardiner", "Darcy's father", "Maria Lucas", "Mr. Philips", 
+     "Mrs. Gardiner", "Miss King", "Mr. Hurst", "Mrs. Forster", 
+     "Colonel Fitzwilliam", "Hill", "Mr. Denny", "Mrs. Jenkinson", 
+     "Captain Carter", "Lady Anne Darcy", "Mrs. Annesley", "Mrs. Long", 
+     "Mrs. Reynolds", "Mrs. Younge", "Dawson", "John", "Miss Watson", 
+     "Mr. Jones", "Mr. Robinson", "Mrs. Nicholls", "Nicholls", "Richard", 
+     "Sarah", "Wickham Sr.", "Young Lucas", "Mr. Morris"
+   ),
+   Cluster = c(
+     rep("K2", 3),
+     rep("K3", 8),
+     rep("K4", 40)
+   )
+ )
Error in `tibble()`:
! Tibble columns must have compatible sizes.
• Size 50: Existing data.
• Size 51: Column `Cluster`.
ℹ Only values of size one are recycled.
Run `rlang::last_trace()` to see where the error occurred.
> 

> df_raw <- read_excel(excel_path, sheet = "ALL INSTANCES")
                                                                         
> 
> timeline_all <- df_raw %>%
+   mutate(
+     Character = str_trim(Character),
+     Character = case_when(
+       Character == "Lady Catherine" ~ "Lady Catherine de Bourgh",
+       Character == "Darcy"          ~ "Mr. Darcy",
+       Character == "Bingley"        ~ "Mr. Bingley",
+       Character == "Mrs. Bennett"    ~ "Mrs. Bennet",
+       TRUE ~ Character
+     )
+   ) %>%
+   group_by(Character, `Graph Chapter`) %>% 
+   summarise(Presence = as.numeric(max(`Total Score`, na.rm = TRUE) > 0), .groups = 'drop') %>%
+   rename(Name = Character, Chapter = `Graph Chapter`) %>%
+   filter(Name %in% cluster_mapping$Name) %>%
+   complete(Name = cluster_mapping$Name, Chapter = 1:61, fill = list(Presence = 0)) %>%
+   left_join(cluster_mapping, by = "Name")
Error in `filter()`:
ℹ In argument: `Name %in% cluster_mapping$Name`.
Caused by error:
! object 'cluster_mapping' not found
Run `rlang::last_trace()` to see where the error occurred.
> 

> generate_dispersion_plot <- tibble(
+   cluster_id = c("K2", "K3", "K4"),
+   title_text = c(
+     "Rhythm of Presence and Absence – K2",
+     "Rhythm of Presence and Absence – K3",
+     "Rhythm of Presence and Absence – K4"
+   ),
+   file_name  = c(
+     "RHYTHM_CLUSTER_TIER_K2.png",
+     "RHYTHM_CLUSTER_TIER_K3.png",
+     "RHYTHM_CLUSTER_TIER_K4.png"
+   )
+ )
> 

> pwalk(generate_dispersion_plot, function(cluster_id, title_text, file_name) {
+   

+   plot_data <- timeline_all %>% 
+     filter(Cluster == cluster_id)

+   img_height <- case_when(
+     cluster_id == "K2" ~ 1000,
+     cluster_id == "K3" ~ 1500,
+     TRUE               ~ 3800
+   )
+   
+   png(file.path(home_dir, file_name), width = 3000, height = img_height, res = 300)
+   
+   p <- ggplot(plot_data, aes(x = Chapter, y = Name)) +

+     geom_tile(aes(fill = as.factor(Presence)), color = "gray90", linewidth = 0.1) +
+     scale_fill_manual(values = c("0" = "#FFFFFF", "1" = "#2C3E50")) +
+     scale_x_continuous(breaks = seq(1, 61, by = 2), expand = c(0, 0)) +
+     labs(
+       x = "Sequential Narrative Timeline (Graph Chapters 1–61)",
+       y = "Character Profile",
+       title = title_text
+     ) +
+     theme_minimal(base_family = "serif") +
+     theme(
+       plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = "#2C3E50", margin = margin(b = 15)),
+       axis.title.x = element_text(size = 10, margin = margin(t = 10)),
+       axis.title.y = element_text(size = 10, margin = margin(r = 10)),
+       axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
+       axis.text.y = element_text(size = 9, face = "bold", color = "#333333"),
+       panel.grid = element_blank(),
+       legend.position = "none",
+       plot.margin = margin(20, 20, 20, 20)
+     )
+   
+   print(p)
+   dev.off()
+ })
> 
> message("Success! The three target dispersion figures have been cleanly generated at /Users/ale/")
