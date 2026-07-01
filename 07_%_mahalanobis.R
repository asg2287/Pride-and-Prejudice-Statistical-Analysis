library(readxl)
library(stats)


file_path <- "/Users/ale/Desktop/Updated data_percentage calc_Pride and Prejudice, Summer 2026.xlsx"
sheet3_percentages <- read_excel(file_path, sheet = "CHARACTER PERCENTAGES")


X_raw <- as.data.frame(sheet3_percentages)
rownames(X_raw) <- X_raw$Character


pct_cols <- c("N %", "DC %", "C %", "I %", "DN %", "A %")
pct_matrix <- X_raw[, pct_cols]


pct_matrix[pct_matrix == 0] <- 0.01

ilr_transform <- function(X) {
  logX <- log(X)
  norm_matrix <- matrix(0, nrow=nrow(X), ncol=5)
  for(i in 1:5) {
    norm_matrix[,i] <- sqrt(i / (i + 1)) * (rowMeans(as.matrix(logX[, 1:i, drop=FALSE])) - logX[, i+1])
  }
  rownames(norm_matrix) <- rownames(X)
  return(norm_matrix)
}

characters_ilr <- ilr_transform(pct_matrix)

global_center <- colMeans(characters_ilr)
global_cov    <- cov(characters_ilr)
global_inv_cov <- solve(global_cov)

d2_scores <- mahalanobis(characters_ilr, center = global_center, cov = global_inv_cov, inverted = TRUE)


final_profile_sheet <- data.frame(
  Character = X_raw$Character,
  Architectural_D2 = round(d2_scores, 6),
  Total_Score = X_raw$`Total Score`,
  `N %`  = X_raw$`N %`,
  `DC %` = X_raw$`DC %`,
  `C %`  = X_raw$`C %`,
  `I %`  = X_raw$`I %`,
  `DN %` = X_raw$`DN %`,
  `A %`  = X_raw$`A %`,
  check.names = FALSE
)


final_profile_sheet <- final_profile_sheet[order(-final_profile_sheet$Architectural_D2), ]


write.csv(final_profile_sheet, 
          file = "/Users/ale/Desktop/Character_Profile_Mahalanobis_D2.csv", 
          row.names = FALSE)

print("Success! Your 51-row architectural D^2 sheet is now on your Desktop.")
