# =====================================================================
# ПРОХОД 2, независимый пересчёт (аудиторский артефакт).
# Не изменяет оригиналы; пишет только stdout (перенаправляется в
# audit/pass2_recalc.out.txt). Не использует функции проверяемого
# скрипта: частоты и метрики считаются напрямую по транзакциям.
# =====================================================================
suppressMessages(library(arules))
data("Groceries")

cat("== ВЕРСИИ ==\n")
cat("R:", R.version.string, "\n")
cat("arules:", as.character(packageVersion("arules")), "\n")

cat("\n== 7.1 ДАННЫЕ ==\n")
N <- length(Groceries)
M <- length(itemLabels(Groceries))
cat("N (транзакций) =", N, "\n")
cat("M (товаров) =", M, "\n")
lst <- as(Groceries, "list")
tab <- table(unlist(lst))
cat("доля непустых имен:", mean(nzchar(names(tab))), "\n")
for (it in c("whole milk", "other vegetables", "rolls/buns", "root vegetables",
             "citrus fruit", "butter", "whipped/sour cream", "yogurt"))
  cat(sprintf("count(%s) = %d\n", it, tab[[it]]))

cat("\n== 7.3 APRIORI (независимый вызов с параметрами задания) ==\n")
rules <- apriori(Groceries,
                 parameter = list(support = 0.001, confidence = 0.3,
                                  minlen = 2, maxlen = 4, target = "rules"),
                 control = list(verbose = FALSE))
cat("правил apriori:", length(rules), "\n")

cat("\n== 7.4 ИЗБЫТОЧНОСТЬ ==\n")
srt <- sort(rules, by = "lift", decreasing = TRUE)
red <- is.redundant(srt, measure = "confidence")
pruned <- srt[!red]
cat("избыточных:", sum(red), "; осталось:", length(pruned), "\n")
# реальная пара: первое избыточное правило и его более общий родитель
items_of <- function(s) trimws(strsplit(gsub("[{}]", "", s), ",")[[1]])
ri <- which(red)[1]
R <- srt[ri]
qR <- quality(R)$confidence
rhsR <- gsub("[{}]", "", labels(rhs(R))[[1]])
lhsR <- labels(lhs(R))[[1]]
cat("пример избыточного: {", lhsR, "} => {", rhsR,
    "} conf =", sprintf("%.6f", qR), "\n")
cand <- which(gsub("[{}]", "", labels(rhs(srt))) == rhsR)
parent <- NULL
for (j in cand) {
  if (j == ri) next
  lj <- items_of(labels(lhs(srt[j]))[[1]])
  lR <- items_of(lhsR)
  if (setequal(lj, lR)) next
  if (all(lj %in% lR) && quality(srt[j])$confidence >= qR) { parent <- j; break }
}
if (!is.null(parent)) {
  cat("родитель (более общее): {", labels(lhs(srt[parent]))[[1]], "} => {",
      rhsR, "} conf =", sprintf("%.6f", quality(srt[parent])$confidence), "\n")
} else cat("родитель не найден перебором\n")

cat("\n== 7.5 ВОРОНКА ФИЛЬТРОВ ==\n")
biz  <- subset(pruned, support > 0.001 & confidence > 0.3 & lift > 1.2)
strg <- subset(pruned, support > 0.005 & lift > 2)
cat("бизнес (как в коде, >):", length(biz), "\n")
cat("сильные (как в коде, support > 0.005):", length(strg), "\n")
# границы по заданию (нестрогие)
biz2  <- subset(pruned, support >= 0.001 & confidence >= 0.3 & lift > 1.2)
strg2 <- subset(pruned, support >= 0.005 & lift > 2)
cat("бизнес (>= по заданию):", length(biz2), "\n")
cat("сильные (>= по заданию):", length(strg2), "\n")
# сколько правил сидит ровно на границах
qs <- quality(pruned)
cat("pruned с support==0.001:", sum(qs$support == 0.001), "\n")
cat("pruned с confidence==0.3:", sum(qs$confidence == 0.3), "\n")
cat("pruned с support==0.005:", sum(qs$support == 0.005), "\n")
cat("pruned с lift==2:", sum(qs$lift == 2), "\n")
# контрольное значение 0.005*9835
cat("0.005*N =", 0.005 * N, " (целого k с k/N==0.005 не существует)\n")

cat("\n== 7.6 НЕЗАВИСИМЫЙ ПЕРЕСЧЁТ МЕТРИК ЭТАЛОННОГО ПРАВИЛА ==\n")
X <- c("citrus fruit", "other vegetables", "whole milk")
Y <- "root vegetables"
nX  <- sum(sapply(lst, function(t) all(X %in% t)))
nY  <- tab[[Y]]
nXY <- sum(sapply(lst, function(t) all(c(X, Y) %in% t)))
supp <- nXY / N
conf <- nXY / nX
lift <- conf / (nY / N)
cat("N =", N, "; n(X) =", nX, "; n(Y) =", nY, "; n(X и Y) =", nXY, "\n")
cat("support =", sprintf("%.8f", supp), "\n")
cat("confidence =", sprintf("%.8f", conf), "\n")
cat("lift =", sprintf("%.8f", lift), "\n")

cat("\n== ДОП: метрики топ-1 бизнес-правила (сверка с CSV) ==\n")
q1 <- quality(biz[1])
cat("{", labels(lhs(biz[1]))[[1]], "} => {", labels(rhs(biz[1]))[[1]],
    "} supp =", sprintf("%.8f", q1$support),
    "conf =", sprintf("%.8f", q1$confidence),
    "lift =", sprintf("%.8f", q1$lift), "\n")

cat("\n== ДОП: категории (статус — детально в проходе 3) ==\n")
pats <- list(
  "Zavtrak"   = "bread|butter|milk|cheese|egg",
  "Vypechka"  = "flour|sugar|baking powder|curd|margarine",
  "Alcohol"   = "beer|wine|liquor",
  "Sneki"     = "soda|snack|popcorn",
  "Uzhin"     = "instant|hamburger|sausage|rolls/buns",
  "FrOv"      = "fruit|vegetables|citrus|tropical|root"
)
for (nm in names(pats)) {
  cnt <- 0
  for (i in seq_len(length(strg))) {
    labs <- items_of(paste(labels(lhs(strg[i]))[[1]],
                           labels(rhs(strg[i]))[[1]], sep = ","))
    if (sum(grepl(pats[[nm]], labs)) >= 2) cnt <- cnt + 1
  }
  cat(nm, ":", cnt, "\n")
}
cat("\nГОТОВО\n")
