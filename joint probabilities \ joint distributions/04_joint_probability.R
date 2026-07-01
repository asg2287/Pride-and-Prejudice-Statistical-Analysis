
library(pdftools)
library(dplyr)

chapters <- 61

elizabeth <- rep(1, chapters)
elizabeth[c(13, 46)] <- 0
darcy <- rep(0, chapters)
darcy[3:12] <- 1
darcy[26:37] <- 1
darcy[43:45] <- 1
darcy[53:61] <- 1

jane <- rep(0, chapters)
bingley <- rep(0, chapters)
4
jane[1:12] <- 1; jane[53:61] <- 1
bingley[1:12] <- 1; bingley[53:61] <- 1

compute_joint_prob <- function(charX, charY, nameX, nameY) {
n <- length(charX)
p11 <- sum(charX == 1 & charY == 1) / n
p10 <- sum(charX == 1 & charY == 0) / n
p01 <- sum(charX == 0 & charY == 1) / n
p00 <- sum(charX == 0 & charY == 0) / n
cat(sprintf("=== Joint Probabilities for %s and %s ===\n", nameX, nameY))
cat(sprintf(" P(1, 1) [Both Present]: %.3f\n", p11))
cat(sprintf(" P(1, 0) [%s Only]: %.3f\n", nameX, p10))
cat(sprintf(" P(0, 1) [%s Only]: %.3f\n", nameY, p01))
cat(sprintf(" P(0, 0) [Both Absent]: %.3f\n\n", p00))
}

compute_joint_prob(elizabeth, darcy, "Elizabeth", "Darcy")
compute_joint_prob(jane, bingley, "Jane", "Bingley")
compute_joint_prob(elizabeth, jane, "Elizabeth", "Jane")
