
raw_data <- data.frame(
  Character = c(
    "Elizabeth", "Mr. Darcy", "Jane", "Mrs. Bennet", "Mr. Bingley", 
    "Wickham", "Lydia", "Mr. Bennet", "Mr. Collins", "Lady Catherine de Bourgh", 
    "Charlotte Lucas", "Miss Bingley", "Mr. Gardiner", "Mrs. Gardiner", "Kitty", 
    "Miss Darcy", "Sir William Lucas", "Colonel Fitzwilliam", "Mary", "Miss de Bourgh", 
    "Mrs. Philips", "Mrs. Reynolds", "Mrs. Hurst", "Colonel Forster", "Maria Lucas", 
    "Lady Lucas", "Miss King", "Mrs. Forster", "Mrs. Long", "Mr. Denny", 
    "Mr. Hurst", "Mrs. Jenkinson", "Darcy's father", "Mr. Philips", "Mrs. Younge", 
    "Hill", "Mr. Jones", "Mrs. Annesley", "Young Lucas", "Captain Carter", 
    "Mrs. Nicholls", "Mr. Robinson", "Lady Anne Darcy", "Wickham Sr.", "Sarah", 
    "Richard", "Nicholls", "John", "Dawson", "Mr. Morris", "Miss Watson"
  ),
  Total_Score = c(
    3331, 1618, 1002, 751, 701, 
    671, 532, 526, 513, 422, 
    325, 319, 239, 207, 205, 
    196, 116, 112, 98, 78, 
    73, 58, 50, 49, 45, 
    36, 35, 34, 31, 26, 
    25, 23, 21, 17, 16, 
    16, 9, 8, 6, 6, 
    4, 4, 4, 2, 2, 
    2, 2, 2, 2, 1, 1
  ),
  stringsAsFactors = FALSE
)

raw_data$Cluster <- sapply(raw_data$Character, function(char) {
  if (char == "Elizabeth") {
    return("K1")
  } else if (char %in% c("Mr. Darcy", "Jane", "Mrs. Bennet")) {
    return("K2")
  } else if (char %in% c("Mr. Bingley", "Wickham", "Lydia", "Mr. Bennet", 
                         "Mr. Collins", "Lady Catherine de Bourgh", 
                         "Charlotte Lucas", "Miss Bingley")) {
    return("K3")
  } else {
    return("K4")
  }
})


final_summary <- raw_data[order(raw_data$Cluster, -raw_data$Total_Score), ]
rownames(final_summary) <- NULL
colnames(final_summary) <- c("Character", "Total Score", "Cluster")


output_csv <- "~/Desktop/Pride and Prejudice/Pride_And_Prejudice_Percentage_Clusters.csv"

if(!dir.exists(dirname(output_csv))) {
  dir.create(dirname(output_csv), recursive = TRUE)
}

write.csv(final_summary, file = output_csv, row.names = FALSE)


cat("Success! Enforced structural sorting completed.\n")
cat("File saved to:", output_csv, "\n\n")
print(final_summary)
