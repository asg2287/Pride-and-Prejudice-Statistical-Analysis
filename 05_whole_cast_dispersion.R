library(tidyverse)
> library(readxl)
>
> home_dir   <- "/Users/ale"
> base_dir   <- file.path(home_dir, "Desktop", "Pride and Prejudice")
> excel_path <- file.path(base_dir, "Pride and Prejudice, Summer 2026.xlsx")
> 
> if (!file.exists(excel_path)) stop("Longitudinal Excel spreadsheet missing.")
>
> df_raw <- read_excel(excel_path, sheet = "ALL INSTANCES")
                                                                         
>
> df_cleaned <- df_raw %>%
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
+   filter(!is.na(Character) & Character != "")
>
> all_cast_characters <- unique(df_cleaned$Character)
>
> timeline_full_cast <- df_cleaned %>%
+   group_by(Character, `Graph Chapter`) %>% 
+   summarise(Presence = as.numeric(max(`Total Score`, na.rm = TRUE) > 0), .groups = 'drop') %>%
+   rename(Name = Character, Chapter = `Graph Chapter`) %>%
+   complete(Name = all_cast_characters, Chapter = 1:61, fill = list(Presence = 0))
>
> timeline_full_cast <- timeline_full_cast %>%
+   mutate(Name = factor(Name, levels = rev(sort(all_cast_characters))))
>
> img_height <- max(1200, length(all_cast_characters) * 80)
> 
> png(file.path(home_dir, "COMPLETE_CAST_STRUCTURAL_DISPERSION.png"), 
+     width = 3200, height = img_height, res = 300)
> 
> p <- ggplot(timeline_full_cast, aes(x = Chapter, y = Name)) +
+   geom_tile(aes(fill = as.factor(Presence)), color = "gray90", linewidth = 0.1) +
+   scale_fill_manual(values = c("0" = "#FFFFFF", "1" = "#2C3E50")) +
+   scale_x_continuous(breaks = seq(1, 61, by = 2), expand = c(0, 0)) +
+   labs(
+     x = "Sequential Narrative Timeline (Graph Chapters 1–61)",
+     y = "Character Profile",
+     title = "Complete Character Structural Dispersion Matrix (Whole Cast)"
+   ) +
+   theme_minimal(base_family = "serif") +
+   theme(
+     plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "#2C3E50", margin = margin(b = 15)),
+     axis.title.x = element_text(size = 11, margin = margin(t = 10)),
+     axis.title.y = element_text(size = 11, margin = margin(r = 10)),
+     axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
+     axis.text.y = element_text(size = 8, face = "bold", color = "#333333"),
+     panel.grid = element_blank(),
+     legend.position = "none",
+     plot.margin = margin(20, 20, 20, 20)
+   )
> 
> print(p)
> dev.off()
