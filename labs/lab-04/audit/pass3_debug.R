suppressMessages(library(arules))
data("Groceries")
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.3,
                 minlen=2, maxlen=4, target="rules"), control=list(verbose=FALSE))
srt <- sort(rules, by="lift", decreasing=TRUE)
pruned <- srt[!is.redundant(srt, measure="confidence")]
strg <- sort(subset(pruned, support>0.005 & lift>2), by="lift", decreasing=TRUE)

basket <- c("butter", "whipped/sour cream")
lhs_mat <- as(lhs(strg), "matrix"); rhs_mat <- as(rhs(strg), "matrix")
cols_lhs <- which(colnames(lhs_mat) %in% basket)
cols_rhs <- which(colnames(rhs_mat) %in% basket)
cat("cols_lhs:", colnames(lhs_mat)[cols_lhs], "| cols_rhs:", colnames(rhs_mat)[cols_rhs], "\n")
n_lhs <- rowSums(lhs_mat); n_in_lhs <- rowSums(lhs_mat[, cols_lhs, drop=FALSE])
direct <- n_lhs > 0 & n_lhs == n_in_lhs
n_rhs <- rowSums(rhs_mat); n_in_rhs <- rowSums(rhs_mat[, cols_rhs, drop=FALSE])
reverse <- n_rhs > 0 & n_rhs == n_in_rhs
cat("sum(direct):", sum(direct), " sum(reverse):", sum(reverse), "\n")
idx <- which(direct | reverse)
cat("idx:", idx, "\n")
for (i in idx) cat(sprintf("  i=%d %s lift=%.3f conf=%.3f | %s => %s\n", i,
  if (direct[i]) "D" else "", quality(strg[i])$lift, quality(strg[i])$confidence,
  gsub("[{}]","",labels(lhs(strg[i]))), gsub("[{}]","",labels(rhs(strg[i])))))
rec <- strg[idx]
rec <- sort(rec, by="confidence", decreasing=TRUE)
idx_sorted <- idx[order(rec@quality$confidence, decreasing=TRUE)]
cat("idx_sorted:", idx_sorted, "\n")
items <- character(0)
for (j in seq_along(idx_sorted)) {
  i <- idx_sorted[j]
  if (direct[i]) items <- c(items, itemLabels(rhs(strg[i])))
  else items <- c(items, itemLabels(lhs(strg[i])))
}
cat("items:", items, "\n")
