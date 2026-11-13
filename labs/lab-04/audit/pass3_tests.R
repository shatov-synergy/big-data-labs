# =====================================================================
# ПРОХОД 3 v2 (векторизовано): категории + recommend_for_basket.
# Оригиналы не изменяет; stdout -> audit/pass3_tests.out.txt
# =====================================================================
suppressMessages(library(arules))
data("Groceries")
N <- length(Groceries)
lst <- as(Groceries, "list")
tab <- table(unlist(lst))

rules <- apriori(Groceries,
                 parameter = list(support = 0.001, confidence = 0.3,
                                  minlen = 2, maxlen = 4, target = "rules"),
                 control = list(verbose = FALSE))
srt <- sort(rules, by = "lift", decreasing = TRUE)
red <- is.redundant(srt, measure = "confidence")
pruned <- srt[!red]
strg <- subset(pruned, support > 0.005 & lift > 2)

pats <- list(
  Zavtrak   = "bread|butter|milk|cheese|egg",
  Vypechka  = "flour|sugar|baking powder|curd|margarine",
  Alcohol   = "beer|wine|liquor",
  Sneki     = "soda|snack|popcorn",
  Uzhin     = "instant|hamburger|sausage|rolls/buns",
  FrOv      = "fruit|vegetables|citrus|tropical|root"
)
rus <- c(Zavtrak="Завтрак", Vypechka="Выпечка", Alcohol="Алкоголь",
         Sneki="Снеки", Uzhin="Быстрый ужин", FrOv="Фрукты-овощи")

items_of <- function(s) trimws(strsplit(gsub("[{}]", "", s), ",")[[1]])

## ---------- 8.1 КАТЕГОРИИ (векторизованно) ----------
cat("== 8.1 КАТЕГОРИИ ==\n")
il <- itemLabels(Groceries)
cat("\n-- товары, матчушие паттерн (169 itemLabels) --\n")
for (nm in names(pats)) {
  hit <- il[grepl(pats[[nm]], il)]
  cat(rus[[nm]], "(", length(hit), "):", paste(hit, collapse = "; "), "\n")
}

sides_match <- function(rules_obj) {
  L <- labels(lhs(rules_obj)); R <- labels(rhs(rules_obj))
  lapply(pats, function(p) grepl(p, L) & grepl(p, R))
}
items_match <- function(rules_obj) {
  L <- labels(lhs(rules_obj)); R <- labels(rhs(rules_obj))
  both <- paste0(gsub("[{}]","",L), ",", gsub("[{}]","",R))
  its <- strsplit(both, ",")
  lapply(pats, function(p) sapply(its, function(v) sum(grepl(p, trimws(v))) >= 2))
}

cat("\n-- итоговые счётчики по strong (255) --\n")
sm <- sides_match(strg); im <- items_match(strg)
cat("семантика кода (обе стороны): "); print(sapply(sm, sum))
cat("по-товарная (>=2 товаров):    "); print(sapply(im, sum))

cat("\n-- воронки категорий (семантика кода) --\n")
step_supp <- subset(pruned, support > 0.005)
sm_r <- sides_match(rules); sm_p <- sides_match(pruned); sm_s <- sides_match(step_supp)
for (nm in names(pats))
  cat(sprintf("%-14s: rules=%5d pruned=%5d supp>0.5%%=%5d +lift>2=%5d\n",
              rus[[nm]], sum(sm_r[[nm]]), sum(sm_p[[nm]]), sum(sm_s[[nm]]), sum(sm[[nm]])))

cat("\n-- лучшие релевантные правила 'нулевых' категорий (pruned) --\n")
for (nm in c("Vypechka", "Alcohol", "Sneki")) {
  Lp <- labels(lhs(pruned)); Rp <- labels(rhs(pruned))
  idx <- which(grepl(pats[[nm]], Lp) & grepl(pats[[nm]], Rp))
  cat(rus[[nm]], ": релевантных в pruned =", length(idx), "\n")
  if (length(idx) > 0) {
    qs <- quality(pruned[idx])
    o <- order(qs$support, decreasing = TRUE)
    for (k in head(o, 3))
      cat(sprintf("   supp=%.5f lift=%.2f | %s => %s\n", qs$support[k], qs$lift[k],
                  gsub("[{}]","",Lp[idx[k]]), gsub("[{}]","",Rp[idx[k]])))
    cat(sprintf("   max support=%.5f, max lift=%.2f\n", max(qs$support), max(qs$lift)))
  }
}

cat("\n-- пересечения категорий (strong) --\n")
mm <- sapply(sm, function(x) x)
multi <- rowSums(mm) >= 2
cat("правил в >=2 категориях:", sum(multi), "\n")
if (any(multi)) {
  Ls <- labels(lhs(strg)); Rs <- labels(rhs(strg))
  k <- which(multi)[1]
  cat("пример:", paste(rus[colnames(mm)[mm[k,]]], collapse=" + "),
      ":", Ls[k], "=>", Rs[k], "\n")
}

## ---------- 8.2 recommend_for_basket ----------
cat("\n== 8.2 recommend_for_basket ==\n")
src <- readLines("/project/labs/lab-04/task1_groceries_apriori.R")
st <- grep("^recommend_for_basket <- function", src)
en <- st + which(src[(st+1):length(src)] == "}")[1]
eval(parse(text = src[st:en]))
cat("функция извлечена из строк", st[1], "-", en, "\n")

Ls <- labels(lhs(strg)); Rs <- labels(rhs(strg))
L_items <- strsplit(gsub("[{}]","",Ls), ","); L_items <- lapply(L_items, trimws)
R_items <- strsplit(gsub("[{}]","",Rs), ","); R_items <- lapply(R_items, trimws)
conf_s <- quality(strg)$confidence

ref_recommend <- function(basket, top_n = 5) {
  basket <- intersect(basket, itemLabels(Groceries))
  if (length(basket) == 0) return(list(msg="нет товаров каталога", items=character(0)))
  direct <- sapply(L_items, function(v) all(v %in% basket))
  reverse <- sapply(R_items, function(v) all(v %in% basket))
  if (!any(direct | reverse)) return(list(msg="правил нет", items=character(0)))
  idx <- which(direct | reverse)
  ordr <- idx[order(conf_s[idx], decreasing = TRUE)]
  items <- character(0)
  for (i in ordr) {
    items <- c(items, if (direct[i]) R_items[[i]][1] else L_items[[i]][1])
    if (length(unique(items)) >= top_n * 2) break
  }
  items <- unique(setdiff(items, basket))
  list(msg="ok", items=head(items, top_n))
}

run_case <- function(name, basket, top_n = 5) {
  cat("\n-- ТЕСТ:", name, "| корзина:", deparse(basket), "--\n")
  r <- tryCatch(recommend_for_basket(basket, strg, top_n = top_n),
                error = function(e) paste("ОШИБКА:", conditionMessage(e)))
  cat("функция: "); print(r)
  e <- ref_recommend(basket, top_n)
  cat("эталон:  "); print(e$items)
  direct <- sapply(L_items, function(v) all(v %in% basket))
  reverse <- sapply(R_items, function(v) all(v %in% basket))
  appl <- which(direct | reverse)
  cat("применившихся правил:", length(appl))
  if (length(appl)) {
    a <- appl[order(conf_s[appl], decreasing = TRUE)]
    cat("; топ-5 по conf:\n")
    for (k in head(a, 5))
      cat(sprintf("   conf=%.4f %s %s => %s%s\n", conf_s[k],
                  if (direct[k]) "[прямой]" else "[обратный]",
                  Ls[k], Rs[k],
                  if (direct[k] && reverse[k]) " (обе стороны — прямой приоритет)" else ""))
  } else cat("\n")
}

run_case("пустая корзина", character(0))
run_case("неизвестный товар", c("qwerty", "foobar"))
run_case("один известный товар", c("whole milk"))
run_case("молоко+овощи (из отчёта)", c("whole milk", "other vegetables"))
run_case("белый хлеб+масло (из отчёта)", c("white bread", "butter"))
run_case("пиво+вино (из отчёта)", c("bottled beer", "red/blush wine"))
run_case("полное покрытие LHS", c("butter", "whipped/sour cream"))
run_case("кандидат уже в корзине", c("butter", "whole milk", "root vegetables"))
run_case("перестановка", c("other vegetables", "whole milk"))
run_case("повтор товара", c("whole milk", "whole milk", "other vegetables"))

## ---------- 8.3 МЯСО И МОЛОКО ----------
cat("\n== 8.3 МЯСО И МОЛОКО ==\n")
n_meat <- tab[["meat"]]
n_mm <- sum(sapply(lst, function(t) all(c("meat","whole milk") %in% t)))
cat(sprintf("{meat}=>{whole milk}: n(meat)=%d n(meat&milk)=%d conf(прямое)=%.4f lift=%.4f\n",
            n_meat, n_mm, n_mm/n_meat, (n_mm/n_meat)/(tab[["whole milk"]]/N)))
cat(sprintf("обратный confidence P(meat|whole milk)=%.4f (функцией не показывается)\n",
            n_mm/tab[["whole milk"]]))
in_strong <- any(grepl("^\\{meat\\} => \\{whole milk\\}$",
                       as(strg, "data.frame")$rules))
cat("правило {meat}=>{whole milk} в strong_rules:", in_strong, "\n")

cat("\n-- блок 10.4: rhs=whole milk, supp>0.005, топ-3 по conf --\n")
rev_milk <- subset(pruned, rhs %in% "whole milk" & support > 0.005)
rev_milk <- head(sort(rev_milk, by = "confidence", decreasing = TRUE), 3)
for (i in seq_len(length(rev_milk)))
  cat(sprintf("   conf=%.4f supp=%.5f %s => {whole milk}\n",
              quality(rev_milk[i])$confidence, quality(rev_milk[i])$support,
              gsub("[{}]","",labels(lhs(rev_milk[i])))))
cat("\nГОТОВО\n")
