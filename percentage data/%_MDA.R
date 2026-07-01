required_packages <- c("readxl", "MASS", "ggplot2", "compositions", "dplyr", "stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

library(readxl)
library(MASS)         
library(ggplot2)
library(compositions) 
library(dplyr)
library(stringr)


excel_path <- "/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx"
if (!file.exists(excel_path)) {
  excel_path <- "/Users/ale/Desktop/Pride and Prejudice/Pride and Prejudice, Summer 2026.xlsx"
}
if (!file.exists(excel_path)) {
  stop("CRITICAL: Could not locate your Pride and Prejudice Excel spreadsheet on your Desktop.")
}


available_sheets <- excel_sheets(excel_path)
selected_sheet <- if("CHARACTER PERCENTAGES" %in% available_sheets) {
  "CHARACTER PERCENTAGES"
} else if("ALL INSTANCES" %in% available_sheets) {
  "ALL INSTANCES"
} else {
  available_sheets[1]
}

df_raw <- read_excel(excel_path, sheet = selected_sheet)


cols <- colnames(df_raw)
find_col <- function(patterns, cols) {
  for (p in patterns) {
    match <- cols[str_detect(tolower(cols), p)]
    if (length(match) > 0) return(match[1])
  }
  return(NULL)
}

char_col <- find_col(c("^character$", "char", "name"), cols)
n_col    <- find_col(c("n %", "^n$", "mentions", "name mention"), cols)
dc_col   <- find_col(c("dc %", "^dc$", "discussed", "discussion"), cols)
c_col    <- find_col(c("c %", "^c$", "dialogue", "communication"), cols)
i_col    <- find_col(c("i %", "^i$", "interiority"), cols)
dn_col   <- find_col(c("dn %", "^dn$", "narrator", "description"), cols)
a_col    <- find_col(c("a %", "^a$", "action"), cols)


df_cleaned <- df_raw %>%
  dplyr::rename(Character = !!char_col) %>%
  dplyr::mutate(Character = stringr::str_trim(as.character(Character))) %>%
  dplyr::filter(!is.na(Character) & Character != "" & Character != "Character" & Character != "NA")


if (!str_detect(tolower(n_col), "%")) {
  df_cleaned <- df_cleaned %>%
    dplyr::mutate(
      N_v = as.numeric(!!sym(n_col)),
      DC_v = if(!is.null(dc_col)) as.numeric(!!sym(dc_col)) else 0,
      C_v  = if(!is.null(c_col)) as.numeric(!!sym(c_col)) else 0,
      I_v  = if(!is.null(i_col)) as.numeric(!!sym(i_col)) else 0,
      DN_v = if(!is.null(dn_col)) as.numeric(!!sym(dn_col)) else 0,
      A_v  = if(!is.null(a_col)) as.numeric(!!sym(a_col)) else 0,
      Tot  = N_v + DC_v + C_v + I_v + DN_v + A_v
    ) %>%
    dplyr::filter(Tot > 0) %>%
    dplyr::group_by(Character) %>%
    dplyr::summarise(across(c(N_v, DC_v, C_v, I_v, DN_v, A_v, Tot), ~ sum(.x, na.rm = TRUE)), .groups = 'drop') %>%
    dplyr::mutate(
      `N %` = (N_v/Tot)*100, `DC %` = (DC_v/Tot)*100, `C %` = (C_v/Tot)*100,
      `I %` = (I_v/Tot)*100, `DN %` = (DN_v/Tot)*100, `A %` = (A_v/Tot)*100
    )
} else {
  df_cleaned <- df_cleaned %>%
    dplyr::mutate(across(c(!!n_col, !!dc_col, !!c_col, !!i_col, !!dn_col, !!a_col), as.numeric)) %>%
    dplyr::rename(`N %` = !!n_col, `DC %` = !!dc_col, `C %` = !!c_col, `I %` = !!i_col, `DN %` = !!dn_col, `A %` = !!a_col)
}


df_mda <- df_cleaned %>%
  dplyr::mutate(Tier = case_when(
    Character == "Elizabeth" ~ "K1",
    Character %in% c("Mr. Darcy", "Jane", "Mrs. Bennet") ~ "K2",
    Character %in% c("Charlotte Lucas", "Lady Catherine de Bourgh", "Lydia", 
                     "Miss Bingley", "Mr. Bennet", "Mr. Bingley", "Mr. Collins", "Wickham") ~ "K3",
    TRUE ~ "K4"
  )) %>%
  as.data.frame()

rownames(df_mda) <- df_mda$Character
df_mda$Tier <- factor(df_mda$Tier, levels = c("K1", "K2", "K3", "K4"))


pct_cols <- c("N %", "DC %", "C %", "I %", "DN %", "A %")
eps <- 1e-6
comp_matrix <- df_mda[, pct_cols]
comp_matrix[comp_matrix == 0] <- eps 

comp_acomp <- compositions::acomp(comp_matrix)
ilr_data <- as.data.frame(compositions::ilr(comp_acomp))
colnames(ilr_data) <- paste0("ILR_", 1:ncol(ilr_data))

mda_ready <- cbind(ilr_data, Tier = df_mda$Tier, Character = df_mda$Character)


cluster_counts <- table(mda_ready$Tier)
log_counts <- log(as.numeric(cluster_counts))
names(log_counts) <- names(cluster_counts)


tier_weights <- c("K1" = 3.985, "K2" = 3.0, "K3" = 2.0, "K4" = 1.0) 
smoothed_numerator <- log_counts * tier_weights[names(log_counts)]
calculated_priors <- smoothed_numerator / sum(smoothed_numerator)

mda_model <- lda(Tier ~ ILR_1 + ILR_2 + ILR_3 + ILR_4 + ILR_5, 
                 data = mda_ready, 
                 prior = as.numeric(calculated_priors))

mda_values <- predict(mda_model)
lda_scores <- as.data.frame(mda_values$x)
trace_variance <- (mda_model$svd)^2 / sum((mda_model$svd)^2)


plot_mda <- cbind(lda_scores, Tier = mda_ready$Tier, Character = mda_ready$Character)

mda_plot <- ggplot(plot_mda, aes(x = LD1, y = LD2, color = Tier)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_text(aes(label = Character), vjust = -0.8, size = 2.5, check_overlap = TRUE, show.legend = FALSE) +
  labs(
    title = "Multiple Discriminant Analysis (MDA) of Cast Architecture",
    subtitle = paste0("Canonical Space Projection Across K1-K4 Tiers | Variance: LD1 = ", 
                      round(trace_variance[1]*100, 1), "%, LD2 = ", round(trace_variance[2]*100, 1), "%"),
    x = paste0("First Discriminant Function (LD1 - ", round(trace_variance[1]*100, 1), "%)"),
    y = paste0("Second Discriminant Function (LD2 - ", round(trace_variance[2]*100, 1), "%)")
  ) +
  scale_color_brewer(palette = "Set1", name = "Structural Cluster Tier") + 
  theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2C3E50", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", color = "gray30", size = 9, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("/Users/ale/Desktop/MULTIPLE_DISCRIMINANT_ANALYSIS_SPACE.pdf", plot = mda_plot, width = 9, height = 7, device = "pdf")


output_predictions <- data.frame(
  Character             = plot_mda$Character,
  Assigned_Tier         = plot_mda$Tier,
  Posterior_Probability = round(apply(mda_values$posterior, 1, max), 4)
)

output_predictions <- output_predictions %>% arrange(Character)

write.csv(output_predictions, "/Users/ale/Desktop/MDA_Classification_Report.csv", row.names = FALSE)

cat("\n>>> Executed clean run.\n")
